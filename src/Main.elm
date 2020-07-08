module Main exposing (main)

import AStar
import Array exposing (Array)
import Array.Extra
import Browser
import Browser.Events
import Constants
    exposing
        ( baseEnemySize
        , blockedGrassIndices
        , blockedPathIndices
        , boardHeight
        , boardUpscale
        , boardWidth
        , buildsPerLevel
        , cellSize
        , dieDelay
        , fps
        , goalIndex
        , pathIndicies
        , postIndices
        , startIndex
        , startingStones
        )
import Dict exposing (Dict)
import Dict.Extra
import Helper exposing (actionButtonsPosition, imageAttributes, intToPxString)
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (class, classList, disabled, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Levels exposing (LevelInfo, getLevelInfo, numberOfLevels, viewLevels)
import List.Extra
import Ports exposing (deleteSaveState, saveState)
import Random exposing (Seed, initialSeed)
import Set exposing (Set)
import Time
import Tower
    exposing
        ( createTower
        , getTowerData
        , getTowerType
        , viewTower
        , viewTowerInformation
        )
import TowerTypes exposing (Tower, TowerEffect(..), TowerId, TowerType)
import Types
    exposing
        ( Board
        , Cell
        , CellIndex
        , CellObject(..)
        , CellType(..)
        , Enemy
        , EnemyId
        , GameModel
        , GameMsg(..)
        , GameState(..)
        , Position
        , Projectile
        , Projectiles
        , Selected(..)
        , Towers
        , gameModelDecoder
        , gameModelEncoder
        )


type Msg
    = StartNewGameClicked
    | LoadSavedGameClicked
    | GotNewWindowWidth Int
    | GameMsg GameMsg


type Screen
    = Large
    | Medium
    | Small


type alias Model =
    { gameModel : GameModel
    , savedGameModel : Maybe GameModel
    , savedTimestamp : Maybe String
    , windowWidth : Int
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        savedGameModel : Maybe GameModel
        savedGameModel =
            Json.Decode.decodeValue
                (Json.Decode.field "gameState" (gameModelDecoder 1))
                flags
                |> Result.toMaybe
                |> Maybe.andThen
                    (\m ->
                        if m.state == GameOver then
                            Nothing

                        else
                            Just m
                    )

        savedTimestamp : Maybe String
        savedTimestamp =
            Json.Decode.decodeValue
                (Json.Decode.field "timestamp" Json.Decode.string)
                flags
                |> Result.toMaybe

        seed =
            Json.Decode.decodeValue
                (Json.Decode.field "seed" Json.Decode.int)
                flags
                |> Result.toMaybe
                |> Maybe.withDefault 1

        windowWidth =
            Json.Decode.decodeValue
                (Json.Decode.field "windowWidth" Json.Decode.int)
                flags
                |> Result.toMaybe
                |> Maybe.withDefault 1

        default =
            { board = initBoard
            , enemies = []
            , enemyIdCount = 0
            , state = Build buildsPerLevel
            , selected = NothingSelected
            , towerIdCount = 0
            , towers = Dict.empty
            , projectiles = []
            , hp = 100
            , level = 1
            , seed = initialSeed seed
            }
    in
    ( { gameModel = default
      , savedGameModel = Maybe.map (\m -> { m | seed = initialSeed seed }) savedGameModel
      , savedTimestamp = Maybe.andThen (always savedTimestamp) savedGameModel
      , windowWidth = windowWidth
      }
    , Cmd.none
    )


initBoard : Board
initBoard =
    let
        boardType index =
            if index == startIndex then
                Start

            else if index == goalIndex then
                Goal

            else if List.member index blockedGrassIndices then
                Grass Blocked

            else if List.member index blockedPathIndices then
                Path Blocked

            else if List.member index postIndices then
                Post

            else if List.member index pathIndicies then
                Path NoCellObject

            else if List.member index startingStones then
                Grass Stone

            else
                Grass NoCellObject
    in
    Array.map
        (\i ->
            { cellType = boardType i
            , index = i
            }
        )
        (Array.fromList (List.range 0 ((boardWidth * boardHeight) - 1)))


addEffects : List TowerEffect -> Enemy -> Enemy
addEffects towerEffects enemy =
    let
        applicableEffects =
            towerEffects
                |> List.filterMap
                    (\effect ->
                        case effect of
                            SlowEffect value ->
                                Just { duration = fps * 3, effectType = SlowEffect value }

                            _ ->
                                Nothing
                    )
    in
    { enemy | effects = enemy.effects ++ applicableEffects }


scaleDownEnemyPosition : Position -> Position
scaleDownEnemyPosition pos =
    { x = pos.x // boardUpscale, y = pos.y // boardUpscale }


findAttackSpeedAuras : Tower -> Towers -> ( Int, List Int )
findAttackSpeedAuras tower towers =
    let
        towerPosition t =
            indexToCellCenterPosition t.cellIndex

        towerInRange t =
            distance
                (towerPosition t)
                (towerPosition tower)
                < toFloat t.range

        speedAuraEffects effect =
            case effect of
                SpeedAura value ->
                    Just value

                _ ->
                    Nothing

        auras =
            towers
                |> Dict.values
                |> List.filter towerInRange
                |> List.foldl (\t allEffects -> allEffects ++ t.effects) []
                |> List.filterMap speedAuraEffects
                |> Set.fromList
                |> Set.toList
                |> List.sort
    in
    ( auras
        |> List.map (\percent -> (toFloat percent / 100) * toFloat tower.rate)
        |> List.sum
        |> round
    , auras
    )


type alias TowerEnemyInteractionResult =
    { towers : Towers
    , enemies : List Enemy
    , projectiles : Projectiles
    , seed : Seed
    }


type alias DamageEnemiesResult =
    { projectiles : Projectiles
    , seed : Seed
    , totalDamage : Int
    }


towerEnemyInteraction : TowerId -> Tower -> TowerEnemyInteractionResult -> TowerEnemyInteractionResult
towerEnemyInteraction towerId tower { towers, enemies, projectiles, seed } =
    if tower.cooldown == 0 then
        let
            towerPosition =
                indexToCellCenterPosition tower.cellIndex

            enemyInRange : Enemy -> Bool
            enemyInRange enemy =
                distance
                    (scaleDownEnemyPosition enemy.position)
                    towerPosition
                    < toFloat tower.range

            ( spawned, notSpawned ) =
                List.partition (.spawnTime >> (==) 0) enemies

            ( validTargets, invalidTargets ) =
                List.partition (\e -> enemyInRange e && e.hp > 0) spawned

            ( targets, outOfTargetCount ) =
                List.Extra.splitAt tower.targets validTargets

            flyingDamage =
                tower.effects
                    |> List.foldl
                        (\effect acc ->
                            case effect of
                                FlyingDamage value ->
                                    value

                                _ ->
                                    acc
                        )
                        100

            dealDamage : DamageEnemiesResult -> Enemy -> ( DamageEnemiesResult, Enemy )
            dealDamage currentResult enemy =
                let
                    ( randomValue, nextSeed ) =
                        Random.step (Random.int 1 100) currentResult.seed

                    hit =
                        List.member TrueStrike tower.effects || randomValue >= enemy.evasion

                    updatedProjectiles =
                        currentResult.projectiles
                            ++ [ { enemyId = enemy.id
                                 , from = towerPosition
                                 , ttl = 12
                                 , color = tower.color
                                 , miss = not hit
                                 }
                               ]

                    damage =
                        if hit then
                            if enemy.flying then
                                round <| toFloat tower.damage * (toFloat flyingDamage / 100)

                            else
                                tower.damage

                        else
                            0
                in
                ( { projectiles = updatedProjectiles
                  , seed = nextSeed
                  , totalDamage = currentResult.totalDamage + min damage enemy.hp
                  }
                , { enemy | hp = max (enemy.hp - damage) 0 }
                )

            ( damageEnemiesResult, enemiesAfterDamage ) =
                List.Extra.mapAccuml
                    dealDamage
                    { projectiles = projectiles
                    , seed = seed
                    , totalDamage = 0
                    }
                    targets

            afterEffects =
                List.map (addEffects tower.effects) enemiesAfterDamage

            towersAfterDealingDamage =
                Dict.update towerId
                    (Maybe.map (\t -> { t | totalDamage = t.totalDamage + damageEnemiesResult.totalDamage }))
                    towers

            ( attackSpeedIncrease, _ ) =
                findAttackSpeedAuras tower towers

            towersAfterAddingCooldown =
                Dict.update towerId
                    (Maybe.map
                        (\t ->
                            { t
                                | cooldown =
                                    if List.isEmpty targets then
                                        0

                                    else
                                        round ((100 / toFloat (t.rate + attackSpeedIncrease)) * toFloat fps)
                            }
                        )
                    )
                    towersAfterDealingDamage
        in
        { towers = towersAfterAddingCooldown
        , enemies = List.sortBy .id (afterEffects ++ outOfTargetCount ++ invalidTargets ++ notSpawned)
        , projectiles = damageEnemiesResult.projectiles
        , seed = damageEnemiesResult.seed
        }

    else
        { towers =
            Dict.update towerId
                (Maybe.map (\t -> { t | cooldown = t.cooldown - 1 }))
                towers
        , enemies = enemies
        , projectiles = projectiles
        , seed = seed
        }


removeCellObject : Cell -> Cell
removeCellObject cell =
    case cell.cellType of
        Path _ ->
            { cell | cellType = Path NoCellObject }

        Grass _ ->
            { cell | cellType = Grass NoCellObject }

        Start ->
            cell

        Goal ->
            cell

        Post ->
            cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMsg gameMsg ->
            let
                ( gameModel, gameCmd ) =
                    updateGame gameMsg model.gameModel
            in
            ( { model | gameModel = gameModel }, Cmd.map GameMsg gameCmd )

        StartNewGameClicked ->
            ( { model | savedGameModel = Nothing }, Cmd.none )

        LoadSavedGameClicked ->
            ( { model
                | gameModel = Maybe.withDefault model.gameModel model.savedGameModel
                , savedGameModel = Nothing
                , savedTimestamp = Nothing
              }
            , Cmd.none
            )

        GotNewWindowWidth width ->
            ( { model | windowWidth = width }, Cmd.none )


updateGame : GameMsg -> GameModel -> ( GameModel, Cmd GameMsg )
updateGame msg model =
    let
        newModel =
            case msg of
                Step ->
                    let
                        aliveEnemies =
                            model.enemies
                                |> List.filter (.dieDelay >> (<) 0)

                        ( enemyDamage, enemiesMoved ) =
                            moveEnemies aliveEnemies

                        agedProjectiles =
                            model.projectiles
                                |> List.map (\p -> { p | ttl = p.ttl - 1 })
                                |> List.filter (.ttl >> (<) 0)

                        { towers, enemies, projectiles, seed } =
                            Dict.foldl
                                towerEnemyInteraction
                                { towers = model.towers
                                , enemies = enemiesMoved
                                , projectiles = agedProjectiles
                                , seed = model.seed
                                }
                                model.towers

                        hp =
                            model.hp - enemyDamage

                        ( level, state, selected ) =
                            if hp <= 0 then
                                ( model.level, GameOver, NothingSelected )

                            else if not (List.isEmpty model.enemies) && List.isEmpty enemies then
                                if numberOfLevels == model.level then
                                    ( model.level, GameCompleted, NothingSelected )

                                else
                                    ( model.level + 1, Build buildsPerLevel, NothingSelected )

                            else
                                ( model.level, model.state, model.selected )
                    in
                    { model
                        | enemies = enemies
                        , towers = towers
                        , projectiles = projectiles
                        , hp = hp
                        , state = state
                        , level = level
                        , seed = seed
                        , selected = selected
                    }

                CellClicked cell ->
                    case model.state of
                        Level ->
                            { model | selected = NothingSelected }

                        Build towersLeft ->
                            if towersLeft > 0 && model.selected == NothingSelected then
                                addTower cell model towersLeft

                            else
                                { model | selected = NothingSelected }

                        Paused ->
                            { model | selected = NothingSelected }

                        GameOver ->
                            { model | selected = NothingSelected }

                        GameCompleted ->
                            { model | selected = NothingSelected }

                TowerClicked towerId ->
                    { model | selected = TowerSelected towerId }

                EnemyClicked enemy ->
                    { model | selected = EnemySelected enemy.id }

                StoneClicked cellIndex ->
                    { model | selected = StoneSelected cellIndex }

                RemoveTowerButtonClicked towerId cellIndex ->
                    { model
                        | towers = Dict.remove towerId model.towers
                        , board = Array.Extra.update cellIndex removeCellObject model.board
                        , selected = NothingSelected
                    }

                RemoveStoneButtonClicked cellIndex ->
                    { model
                        | board = Array.Extra.update cellIndex removeCellObject model.board
                        , selected = NothingSelected
                    }

                KeepTowerClicked towerToKeepId ->
                    keepTower towerToKeepId model

                UpgradeTowerClicked towerId towerType ->
                    upgradeTower model towerId towerType

                PauseButtonClicked ->
                    { model | state = Paused }

                ResumeButtonClicked ->
                    { model | state = Level }

                GoalClicked ->
                    { model | selected = NothingSelected }
    in
    ( newModel
    , if model.state /= newModel.state then
        if model.state == GameOver || model.state == GameCompleted then
            deleteSaveState ()

        else
            saveState (gameModelEncoder newModel)

      else
        Cmd.none
    )


upgradeTower : GameModel -> TowerId -> TowerType -> GameModel
upgradeTower model towerId upgradeTo =
    let
        ( updatedBoard, updatedTowers ) =
            case Dict.get towerId model.towers of
                Just tower ->
                    let
                        combinations =
                            (getTowerData upgradeTo).combinations

                        towersEntriesToReplace =
                            combinations
                                |> List.filter ((/=) tower.towerType)
                                |> List.map
                                    (\towerType ->
                                        Dict.Extra.find
                                            (\_ t -> t.towerType == towerType)
                                            model.towers
                                    )
                                |> List.filterMap identity

                        board =
                            towersEntriesToReplace
                                |> List.map Tuple.second
                                |> List.map .cellIndex
                                |> List.foldl (\index result -> addStoneToCell index result) model.board
                    in
                    ( board
                    , towersEntriesToReplace
                        |> List.map Tuple.first
                        |> List.foldl (\id towers -> Dict.remove id towers) model.towers
                        |> Dict.update towerId (Maybe.map (.cellIndex >> createTower upgradeTo False))
                    )

                Nothing ->
                    ( model.board, model.towers )
    in
    { model
        | towers = updatedTowers
        , board = updatedBoard
        , selected = NothingSelected
    }


spawnEnemies : GameModel -> List Enemy
spawnEnemies model =
    let
        levelInfo =
            getLevelInfo model.level

        delayBetweenEnemiesFactor =
            (toFloat 100 / toFloat levelInfo.speed) * fps

        path =
            findFullPath model.board levelInfo.flying
    in
    List.range 0 (levelInfo.enemyCount - 1)
        |> List.map
            (\index ->
                createEnemy
                    (model.enemyIdCount + index)
                    (round (toFloat index * delayBetweenEnemiesFactor))
                    levelInfo
                    path
            )


keepTower : TowerId -> GameModel -> GameModel
keepTower towerToKeepId model =
    let
        afterRemovingTemporaryFlag =
            Dict.update
                towerToKeepId
                (Maybe.map
                    (\tower ->
                        { tower | temporary = False }
                    )
                )
                model.towers

        ( temporary, toKeep ) =
            Dict.partition (\_ tower -> tower.temporary) afterRemovingTemporaryFlag

        boardWithStones =
            temporary
                |> Dict.values
                |> List.map .cellIndex
                |> List.foldl addStoneToCell model.board

        newEnemies =
            spawnEnemies model
    in
    { model
        | towers = toKeep
        , board = boardWithStones
        , state = Level
        , enemies = newEnemies
        , enemyIdCount = model.enemyIdCount + List.length newEnemies
        , selected = NothingSelected
    }


moveEnemies : List Enemy -> ( Int, List Enemy )
moveEnemies enemies =
    let
        ( spawnedEnemies, notSpawnedEnemies ) =
            List.partition (.spawnTime >> (==) 0) enemies

        ( enemiesReachedGoal, otherEnemies ) =
            spawnedEnemies
                |> List.map moveEnemy
                |> List.partition (.path >> List.isEmpty)

        enemiesLeft =
            otherEnemies ++ notSpawnedEnemies
    in
    ( List.foldl (.damage >> (+)) 0 enemiesReachedGoal
    , List.map
        (\e ->
            { e
                | spawnTime = max 0 (e.spawnTime - 1)
                , dieDelay =
                    if e.hp == 0 then
                        e.dieDelay - 1

                    else
                        e.dieDelay
            }
        )
        enemiesLeft
    )


indexToCellPosition : Int -> ( Int, Int )
indexToCellPosition i =
    ( modBy boardWidth i, i // boardWidth )


availableSteps : Board -> Bool -> AStar.Position -> Set AStar.Position
availableSteps board flying ( x, y ) =
    let
        index =
            y * boardWidth + x

        noRowBreak i =
            AStar.pythagoreanCost (indexToCellPosition i) (indexToCellPosition index) < 2.0

        getCell : Int -> Maybe Cell
        getCell i =
            if noRowBreak i then
                Array.get i board |> Maybe.andThen walkable

            else
                Nothing

        up =
            getCell (index - boardWidth)

        right =
            getCell (index + 1)

        down =
            getCell (index + boardWidth)

        left =
            getCell (index - 1)

        upLeft =
            if up /= Nothing || left /= Nothing || flying then
                getCell (index - boardWidth - 1)

            else
                Nothing

        upRight =
            if up /= Nothing || right /= Nothing || flying then
                getCell (index - boardWidth + 1)

            else
                Nothing

        downRight =
            if down /= Nothing || right /= Nothing || flying then
                getCell (index + boardWidth + 1)

            else
                Nothing

        downLeft =
            if down /= Nothing || left /= Nothing || flying then
                getCell (index + boardWidth - 1)

            else
                Nothing

        walkable : Cell -> Maybe Cell
        walkable cell =
            if flying then
                Just cell

            else
                case cell.cellType of
                    Path cellObject ->
                        if cellObject == NoCellObject || cellObject == Blocked then
                            Just cell

                        else
                            Nothing

                    Grass cellObject ->
                        if cellObject == NoCellObject || cellObject == Blocked then
                            Just cell

                        else
                            Nothing

                    Start ->
                        Just cell

                    Goal ->
                        Just cell

                    Post ->
                        Just cell
    in
    [ upLeft, up, upRight, right, downRight, down, downLeft, left ]
        |> List.filterMap identity
        |> List.map (.index >> indexToCellPosition)
        |> Set.fromList


findPath : Array Cell -> Bool -> ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int )
findPath cells flying from to =
    let
        pathDistance ( x1, y1 ) ( x2, y2 ) =
            let
                dx =
                    toFloat <| abs (x1 - x2)

                dy =
                    toFloat <| abs (y1 - y2)
            in
            sqrt ((dx ^ 2) + (dy ^ 2))

        path =
            AStar.findPath
                pathDistance
                (availableSteps cells flying)
                from
                to
    in
    path
        |> Maybe.withDefault []


indexToCellCenterPosition : Int -> Position
indexToCellCenterPosition index =
    let
        x =
            modBy boardWidth index

        y =
            index // boardWidth
    in
    { x = (x * cellSize) + (cellSize // 2)
    , y = (y * cellSize) + (cellSize // 2)
    }


startPosition : Position
startPosition =
    let
        x =
            modBy boardWidth startIndex

        y =
            startIndex // boardWidth
    in
    { x = ((x * cellSize) + (cellSize // 2)) * boardUpscale
    , y = ((y * cellSize) + cellSize) * boardUpscale
    }


findFullPath : Board -> Bool -> List Position
findFullPath board flying =
    let
        cellPositionToPosition : ( Int, Int ) -> Position
        cellPositionToPosition ( x, y ) =
            { x = ((x * cellSize) + (cellSize // 2)) * boardUpscale
            , y = ((y * cellSize) + (cellSize // 2)) * boardUpscale
            }

        fullPath =
            List.foldl
                (\to totalPath ->
                    case List.Extra.last totalPath of
                        Just from ->
                            let
                                path =
                                    findPath board flying from to
                            in
                            if List.isEmpty path then
                                []

                            else
                                totalPath ++ path

                        Nothing ->
                            []
                )
                [ indexToCellPosition startIndex ]
                (List.map indexToCellPosition postIndices ++ [ indexToCellPosition goalIndex ])
    in
    -- Remove start position since enemies start there
    List.tail fullPath |> Maybe.withDefault [] |> List.map cellPositionToPosition


createEnemy : EnemyId -> Int -> LevelInfo -> List Position -> Enemy
createEnemy enemyId spawnTime levelInfo path =
    { position = startPosition
    , path = path
    , id = enemyId
    , hp = levelInfo.hp
    , maxHp = levelInfo.hp
    , damage = levelInfo.damage
    , spawnTime = spawnTime
    , evasion = levelInfo.evasion
    , flying = levelInfo.flying
    , boss = levelInfo.boss
    , effects = []
    , baseSpeed = levelInfo.speed
    , magicImmune = levelInfo.magicImmune
    , dieDelay = dieDelay
    }


addTower : Cell -> GameModel -> Int -> GameModel
addTower cell model towersLeft =
    let
        levelInfo =
            getLevelInfo model.level

        ( seed, towerType ) =
            getTowerType model.seed levelInfo.buildChances

        tower =
            createTower towerType True cell.index

        ( towerAdded, cellWithTower ) =
            addTowerToCell cell model.towerIdCount

        boardWithTower =
            Array.set cell.index cellWithTower model.board

        pathAfterAddingTower =
            not (List.isEmpty (findFullPath boardWithTower False))

        success =
            towerAdded && pathAfterAddingTower

        ( updatedBoard, towerIdCount, towers ) =
            if success then
                ( boardWithTower
                , model.towerIdCount + 1
                , Dict.insert model.towerIdCount tower model.towers
                )

            else
                ( model.board
                , model.towerIdCount
                , model.towers
                )

        state =
            if success then
                Build (towersLeft - 1)

            else
                model.state
    in
    { model
        | board = updatedBoard
        , towerIdCount = towerIdCount
        , towers = towers
        , state = state
        , selected = NothingSelected
        , seed = seed
    }


addTowerToCell : Cell -> TowerId -> ( Bool, Cell )
addTowerToCell cell towerId =
    case cell.cellType of
        Path NoCellObject ->
            ( True, { cell | cellType = Path (CellTower towerId) } )

        Path _ ->
            ( False, cell )

        Grass NoCellObject ->
            ( True, { cell | cellType = Grass (CellTower towerId) } )

        Grass _ ->
            ( False, cell )

        Start ->
            ( False, cell )

        Goal ->
            ( False, cell )

        Post ->
            ( False, cell )


addStoneToCell : CellIndex -> Board -> Board
addStoneToCell cellIndex board =
    let
        newCell : Cell -> Cell
        newCell cell =
            case cell.cellType of
                Path _ ->
                    { cell | cellType = Path Stone }

                Grass _ ->
                    { cell | cellType = Grass Stone }

                Start ->
                    cell

                Goal ->
                    cell

                Post ->
                    cell
    in
    Array.Extra.update cellIndex newCell board


calculateMovement : Int -> Position -> Position -> ( Int, Int )
calculateMovement speed from to =
    let
        deltaX =
            to.x - from.x

        deltaY =
            to.y - from.y
    in
    ( if deltaX > speed then
        1

      else if deltaX < -speed then
        -1

      else
        0
    , if deltaY > speed then
        1

      else if deltaY < -speed then
        -1

      else
        0
    )


slowEffect : Enemy -> Float
slowEffect enemy =
    if enemy.magicImmune then
        1

    else
        enemy.effects
            |> List.map
                (\effect ->
                    case effect.effectType of
                        SlowEffect value ->
                            1 - (toFloat value / 100)

                        _ ->
                            0
                )
            |> Set.fromList
            -- Remove duplicates since same slow effect don't stack
            |> Set.toList
            |> List.product


moveEnemy : Enemy -> Enemy
moveEnemy ({ position } as enemy) =
    let
        toPosition =
            case enemy.path of
                next :: _ ->
                    next

                [] ->
                    position

        ( deltaX, deltaY ) =
            calculateMovement enemy.baseSpeed position toPosition

        speed =
            if enemy.hp > 0 then
                if deltaX /= 0 && deltaY /= 0 then
                    toFloat enemy.baseSpeed * 0.7 * slowEffect enemy

                else
                    toFloat enemy.baseSpeed * slowEffect enemy

            else
                0

        nextPosition : Position
        nextPosition =
            { x = position.x + round (toFloat deltaX * speed)
            , y = position.y + round (toFloat deltaY * speed)
            }

        ( newPosition, path ) =
            if calculateMovement enemy.baseSpeed nextPosition toPosition == ( 0, 0 ) then
                --We have reached the position
                ( toPosition, List.tail enemy.path |> Maybe.withDefault [] )

            else
                ( nextPosition, enemy.path )

        decreaseEffectDuration effect =
            { effect | duration = effect.duration - 1 }
    in
    { enemy
        | position = newPosition
        , path = path
        , effects = enemy.effects |> List.map decreaseEffectDuration |> List.filter (.duration >> (<) 0)
    }


view : Model -> Html Msg
view model =
    case model.savedGameModel of
        Just _ ->
            div [ class "pre-game" ]
                [ text ("Found saved game " ++ Maybe.withDefault "" model.savedTimestamp)
                , button [ onClick LoadSavedGameClicked ] [ text "Load saved game" ]
                , button [ onClick StartNewGameClicked ] [ text "Start new game" ]
                ]

        Nothing ->
            let
                screenSize =
                    if model.windowWidth > 1380 then
                        Large

                    else if model.windowWidth > 1060 then
                        Medium

                    else
                        Small
            in
            Html.map GameMsg (viewGame screenSize model.gameModel)


viewGame : Screen -> GameModel -> Html GameMsg
viewGame screenSize gameModel =
    case screenSize of
        Large ->
            div
                [ class "main" ]
                [ div [ class "left-side" ] [ viewLeftSide gameModel ]
                , div
                    [ class "game", style "min-width" (intToPxString (cellSize * boardWidth)) ]
                    [ viewBoard gameModel ]
                , div [ class "right-side" ] [ viewRightSide gameModel ]
                , viewGameOverlay gameModel
                ]

        Medium ->
            div
                [ class "main" ]
                [ div [ class "menu-medium" ] [ viewLeftSide gameModel, viewRightSide gameModel ]
                , div
                    [ class "game", style "min-width" (intToPxString (cellSize * boardWidth)) ]
                    [ viewBoard gameModel ]
                , viewGameOverlay gameModel
                ]

        Small ->
            div
                [ class "main-small" ]
                [ div
                    [ class "game", style "min-width" (intToPxString (cellSize * boardWidth)) ]
                    [ viewBoard gameModel ]
                , div [ class "menu-small" ]
                    [ viewLeftSide gameModel
                    , viewRightSide gameModel
                    ]
                , viewGameOverlay gameModel
                ]


viewGameOverlay : GameModel -> Html GameMsg
viewGameOverlay model =
    if model.state == GameOver then
        div [ class "game-overlay" ]
            [ h1 [] [ text "Game over!" ]
            , text ("You reached level " ++ String.fromInt model.level)
            ]

    else if model.state == GameCompleted then
        div [ class "game-overlay" ]
            [ h1 [] [ text "Game completed!" ]
            , text ("Your fort survived with " ++ String.fromInt model.hp ++ " hp")
            ]

    else
        text ""


viewSelectedTowerInfo : GameModel -> Tower -> Html GameMsg
viewSelectedTowerInfo model tower =
    let
        ( attackSpeedIncrease, auras ) =
            findAttackSpeedAuras tower model.towers

        speedAuraString =
            auras
                |> List.map (\percent -> String.fromInt percent ++ "%")
                |> String.join ", "
    in
    div []
        ([ div []
            [ span [] [ text ("Tower " ++ tower.name) ] ]
         ]
            ++ [ div [] [ text ("Total damage: " ++ String.fromInt tower.totalDamage) ] ]
            ++ [ div []
                    [ text
                        ("Attack rate: "
                            ++ String.fromInt (tower.rate + attackSpeedIncrease)
                        )
                    ]
               ]
            ++ [ div []
                    [ if String.isEmpty speedAuraString then
                        text ""

                      else
                        text ("Attack speed auras: " ++ speedAuraString)
                    ]
               ]
        )


viewLeftSide : GameModel -> Html GameMsg
viewLeftSide model =
    let
        stateString =
            case model.state of
                Level ->
                    "Level " ++ String.fromInt model.level

                Build towersLeft ->
                    let
                        towerString =
                            if towersLeft == 0 then
                                "Choose tower"

                            else
                                String.fromInt towersLeft ++ " towers left to build"
                    in
                    "Building for level " ++ String.fromInt model.level ++ " (" ++ towerString ++ ")"

                Paused ->
                    "Paused"

                GameOver ->
                    "Game over"

                GameCompleted ->
                    "Game completed"

        selection =
            div [ class "selection-info" ]
                [ case model.selected of
                    TowerSelected towerId ->
                        case Dict.get towerId model.towers of
                            Just tower ->
                                viewSelectedTowerInfo model tower

                            Nothing ->
                                text ""

                    EnemySelected enemyId ->
                        case List.Extra.find (.id >> (==) enemyId) model.enemies of
                            Just enemy ->
                                let
                                    speed =
                                        round (toFloat enemy.baseSpeed * slowEffect enemy)
                                in
                                div []
                                    [ div [] [ text ("Enemy level: " ++ String.fromInt model.level) ]
                                    , div [] [ text ("Hp: (" ++ String.fromInt enemy.hp ++ "/" ++ String.fromInt enemy.maxHp ++ ")") ]
                                    , div []
                                        [ if speed == enemy.baseSpeed then
                                            text ("Speed: " ++ String.fromInt enemy.baseSpeed)

                                          else
                                            text ("Speed: (" ++ String.fromInt speed ++ "/" ++ String.fromInt enemy.baseSpeed ++ ")")
                                        ]
                                    ]

                            Nothing ->
                                text ""

                    NothingSelected ->
                        text ""

                    StoneSelected _ ->
                        div []
                            [ text "Stone" ]
                ]

        infoBlock header info =
            div [ class "info-block" ] [ div [ class "info-block-header" ] [ text header ], info ]
    in
    div []
        [ div [ class "card" ]
            [ infoBlock "Status" (text stateString)
            , infoBlock "Fort HP" (text (String.fromInt model.hp ++ "/100"))
            , infoBlock "Selection" selection
            ]
        , viewLevels model.level
        ]


viewRightSide : GameModel -> Html GameMsg
viewRightSide model =
    let
        ( temporaryTowerTypes, existingTowerTypes ) =
            model.towers |> Dict.values |> List.partition .temporary

        towerTypes towers =
            List.map .towerType towers
    in
    div []
        [ viewTowerInformation (towerTypes temporaryTowerTypes) (towerTypes existingTowerTypes) ]


groupCells : Array a -> Array (Array a)
groupCells array =
    Array.map
        (\i ->
            Array.slice
                (i * boardWidth)
                ((i * boardWidth) + boardWidth)
                array
        )
        (Array.initialize boardHeight identity)


viewBoard : GameModel -> Html GameMsg
viewBoard model =
    let
        cells =
            Array.toList
                (Array.map (viewCellGroup model model.towers) (groupCells model.board))

        visibleEnemies =
            List.filter (.spawnTime >> (==) 0) model.enemies
    in
    div [ class "board" ]
        [ div [ class "cells" ] cells
        , div [ class "enemies" ] (List.map (viewEnemy model.selected) visibleEnemies)
        , div [ class "projectiles" ] (List.map (viewProjectile visibleEnemies) model.projectiles)
        ]


distance : Position -> Position -> Float
distance pos1 pos2 =
    let
        dx =
            toFloat <| abs (pos1.x - pos2.x)

        dy =
            toFloat <| abs (pos1.y - pos2.y)
    in
    sqrt ((dx ^ 2) + (dy ^ 2))


viewProjectile : List Enemy -> Projectile -> Html GameMsg
viewProjectile enemies projectile =
    let
        maybeEnemy =
            List.Extra.find (.id >> (==) projectile.enemyId) enemies
    in
    case maybeEnemy of
        Just enemy ->
            let
                enemyScaledPosition =
                    scaleDownEnemyPosition enemy.position

                width =
                    distance
                        projectile.from
                        enemyScaledPosition
                        |> round
                        |> intToPxString

                ( left, top, angle ) =
                    if projectile.from.x <= enemyScaledPosition.x then
                        ( projectile.from.x
                        , projectile.from.y
                        , atan2
                            -(toFloat (projectile.from.x - enemyScaledPosition.x))
                            (toFloat (projectile.from.y - enemyScaledPosition.y))
                        )

                    else
                        ( enemyScaledPosition.x
                        , enemyScaledPosition.y
                        , atan2
                            -(toFloat (enemyScaledPosition.x - projectile.from.x))
                            (toFloat (enemyScaledPosition.y - projectile.from.y))
                        )
            in
            div []
                [ div
                    [ class "projectile"
                    , style "left" (intToPxString left)
                    , style "top" (intToPxString top)
                    , style "width" width
                    , style "background-color" projectile.color
                    , style "transform" ("rotate(" ++ String.fromFloat (angle - pi / 2) ++ "rad)")
                    ]
                    []
                , if projectile.miss then
                    div
                        [ class "miss-text"
                        , style "left" (intToPxString enemyScaledPosition.x)
                        , style "top" (intToPxString enemyScaledPosition.y)
                        ]
                        [ text "Miss" ]

                  else
                    text ""
                ]

        Nothing ->
            text ""


viewEnemy : Selected -> Enemy -> Html GameMsg
viewEnemy selected enemy =
    let
        enemySelected =
            case selected of
                EnemySelected enemyId ->
                    enemyId == enemy.id

                _ ->
                    False

        hpPercentage =
            (toFloat enemy.hp / toFloat enemy.maxHp) * 100

        enemySize =
            case ( enemy.flying, enemy.boss ) of
                ( False, False ) ->
                    baseEnemySize

                ( False, True ) ->
                    round (baseEnemySize * 1.8)

                ( True, False ) ->
                    round (baseEnemySize * 1.3)

                ( True, True ) ->
                    round (baseEnemySize * 2.5)
    in
    div
        ([ class "enemy"
         , style "width" (intToPxString enemySize)
         , style "height" (intToPxString enemySize)
         , style "left" (intToPxString ((enemy.position.x // boardUpscale) - (enemySize // 2)))
         , style "top" (intToPxString ((enemy.position.y // boardUpscale) - (enemySize // 2)))
         , style "opacity" (String.fromFloat (toFloat enemy.dieDelay / dieDelay))
         , onClick (EnemyClicked enemy)
         ]
            ++ (if enemy.flying then
                    imageAttributes "bat.png" "100%"

                else
                    imageAttributes "ghost.png" "100%"
               )
        )
        ([ div [ class "hp-bar" ]
            [ div
                [ class "hp-bar-inner"
                , style "width" (String.fromFloat hpPercentage ++ "%")
                ]
                []
            ]
         ]
            ++ (if enemySelected then
                    [ div
                        [ class "selection"
                        , style "width" "120%"
                        , style "height" "120%"
                        ]
                        []
                    ]

                else
                    []
               )
        )


viewCellGroup : GameModel -> Towers -> Array Cell -> Html GameMsg
viewCellGroup model towers group =
    div [ class "cell-row" ]
        (Array.toList
            (Array.map (viewCell model towers) group)
        )


viewCell : GameModel -> Towers -> Cell -> Html GameMsg
viewCell model towers cell =
    let
        ( cellClass, cellObject, buildable ) =
            case cell.cellType of
                Path cellObject_ ->
                    ( "cell-path", cellObject_, cellObject_ == NoCellObject )

                Grass cellObject_ ->
                    ( "cell-grass", cellObject_, cellObject_ == NoCellObject )

                Start ->
                    ( "cell-start", NoCellObject, False )

                Goal ->
                    ( "cell-goal", NoCellObject, False )

                Post ->
                    ( "cell-post", NoCellObject, False )

        towerSelected towerId =
            case model.selected of
                TowerSelected selectedTowerId ->
                    towerId == selectedTowerId

                _ ->
                    False

        stoneSelected =
            case model.selected of
                StoneSelected cellIndex ->
                    cellIndex == cell.index

                _ ->
                    False

        content =
            if cell.cellType == Goal then
                [ div ([ class "fort" ] ++ imageAttributes "fort.png" "100%") [] ]

            else if cell.cellType == Start then
                [ div ([ class "cave" ] ++ imageAttributes "cave.png" "100%") [] ]

            else
                []
                    ++ (case cellObject of
                            CellTower towerId ->
                                let
                                    maybeTower =
                                        Dict.get towerId towers
                                in
                                case maybeTower of
                                    Just tower ->
                                        [ viewTower model.state (towerSelected towerId) model.towers towerId tower ]

                                    Nothing ->
                                        []

                            Stone ->
                                [ viewStone model.state stoneSelected cell.index ]

                            NoCellObject ->
                                []

                            Blocked ->
                                []
                       )

        pointer =
            case cellObject of
                CellTower _ ->
                    True

                Stone ->
                    True

                Blocked ->
                    False

                NoCellObject ->
                    False

        onClickAttribute =
            case cellObject of
                CellTower towerId ->
                    [ onClick (TowerClicked towerId) ]

                Stone ->
                    [ onClick (StoneClicked cell.index) ]

                NoCellObject ->
                    [ onClick (CellClicked cell) ]

                Blocked ->
                    []

        hoverEffect =
            case model.state of
                Build towersLeft ->
                    buildable && towersLeft > 0

                _ ->
                    False
    in
    div
        ([ class "cell"
         , class cellClass
         , style "width" (intToPxString cellSize)
         , style "height" (intToPxString cellSize)
         , class
            (if hoverEffect then
                "cell-hover"

             else
                ""
            )
         , class
            (if pointer then
                "pointer"

             else
                ""
            )
         ]
            ++ onClickAttribute
        )
        content


viewStone : GameState -> Bool -> CellIndex -> Html GameMsg
viewStone state selected cellIndex =
    div
        ([ class "stone" ] ++ imageAttributes "stone.png" "100%")
        (if selected then
            [ div
                [ class "selection"
                , style "width" (intToPxString cellSize)
                , style "height" (intToPxString cellSize)
                ]
                []
            , div [ class "action-buttons", actionButtonsPosition cellIndex ]
                [ case state of
                    Build _ ->
                        button
                            [ stopPropagationOn "click" (Json.Decode.succeed ( RemoveStoneButtonClicked cellIndex, True ))
                            ]
                            [ text "Remove" ]

                    _ ->
                        text ""
                ]
            ]

         else
            []
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameModel.state == Level then
        Sub.batch
            [ Time.every (1000 / fps) (always (GameMsg Step))
            , Browser.Events.onResize (\w _ -> GotNewWindowWidth w)
            ]

    else
        Browser.Events.onResize (\w _ -> GotNewWindowWidth w)


main : Program Json.Decode.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
