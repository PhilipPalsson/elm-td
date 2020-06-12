module Main exposing (main)

import AStar
import Array exposing (Array)
import Array.Extra
import Browser
import Constants exposing (boardHeight, boardWidth, buildsPerLevel, cellSize, enemiesPerLevel, enemySize, goalIndex, postIndices, startIndex, stepSize, stoneSize)
import Dict exposing (Dict)
import Dict.Extra
import Helper exposing (intToPxString)
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List.Extra
import Random exposing (Seed, initialSeed, step)
import Set exposing (Set)
import Tower exposing (Tower, TowerId, TowerType, availableUpgrades, createTower, getTowerType, projectileColor, towerCombination, towerTypeString, viewTower, viewTowerInformation)


type GameState
    = Level
    | Build Int


type Selected
    = TowerSelected TowerId
    | EnemySelected EnemyId
    | StoneSelected CellIndex
    | NothingSelected


type alias Projectile =
    { enemyId : EnemyId, from : Position, ttl : Int, color : String }


type alias Projectiles =
    List Projectile


type alias Model =
    { board : Board
    , enemies : List Enemy
    , enemyIdCount : EnemyId
    , state : GameState
    , selected : Selected
    , towerIdCount : TowerId
    , towers : Towers
    , projectiles : Projectiles
    , hp : Int
    , level : Int
    , seed : Seed
    }


type alias EnemyId =
    Int


type alias Towers =
    Dict TowerId Tower


type CellObject
    = CellTower TowerId
    | Stone
    | NoCellObject


type CellType
    = Path CellObject
    | Grass CellObject
    | Start
    | Goal
    | Post


type alias Cell =
    { cellType : CellType, index : CellIndex }


type alias Position =
    { x : Int, y : Int }


type alias Enemy =
    { position : Position
    , path : List Position
    , id : Int
    , hp : Int
    , damage : Int
    , spawnTime : Int
    }


type alias Board =
    Array Cell


type alias CellIndex =
    Int


type Msg
    = StepClicked
    | BuildCellClicked Cell
    | TowerClicked TowerId
    | StoneClicked CellIndex
    | EnemyClicked Enemy
    | RemoveTowerButtonClicked TowerId CellIndex
    | RemoveStoneButtonClicked CellIndex
    | KeepTowerClicked TowerId
    | UpgradeTowerClicked TowerId TowerType


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = initBoard
      , enemies = []
      , enemyIdCount = 0
      , state = Build buildsPerLevel
      , selected = NothingSelected
      , towerIdCount = 0
      , towers = Dict.empty
      , projectiles = []
      , hp = 100
      , level = 0
      , seed = initialSeed 1
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

            else if List.member index postIndices then
                Post

            else if
                List.member index
                    [ 3, 33, 63, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 93, 105, 116, 123, 135, 146, 153, 165, 176, 183, 195, 206, 213, 225, 236, 243, 255, 266, 273, 285, 296, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 345, 375, 405, 435, 465, 495, 525, 526, 527, 528, 529, 530, 531, 532, 533, 534, 535, 536 ]
            then
                Path NoCellObject

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


towerEnemyInteraction : TowerId -> Tower -> ( Towers, List Enemy, Projectiles ) -> ( Towers, List Enemy, Projectiles )
towerEnemyInteraction towerId tower ( towers, enemies, projectiles ) =
    if tower.currentCooldown == 0 then
        let
            dealDamage enemy =
                { enemy | hp = enemy.hp - tower.damage }

            towerPosition =
                indexToCellCenterPosition tower.cellIndex

            enemyInRange : Enemy -> Bool
            enemyInRange enemy =
                distance
                    enemy.position
                    towerPosition
                    < toFloat tower.range

            ( spawned, notSpawned ) =
                List.partition (.spawnTime >> (==) 0) enemies

            ( inRange, outOfRange ) =
                List.partition enemyInRange spawned

            ( targets, outOfTargetCount ) =
                List.Extra.splitAt tower.targets inRange

            afterDamage =
                List.map dealDamage targets

            -- TODO this isn't calculating correctly
            towersAfterDealingDamage =
                Dict.update towerId
                    (Maybe.map (\t -> { t | totalDamage = t.totalDamage + List.length targets }))
                    towers

            towersAfterAddingCooldown =
                Dict.update towerId
                    (Maybe.map
                        (\t ->
                            { t
                                | currentCooldown =
                                    if List.isEmpty targets then
                                        0

                                    else
                                        t.cooldown
                            }
                        )
                    )
                    towersAfterDealingDamage

            newProjectiles =
                List.map
                    (\enemy ->
                        { enemyId = enemy.id
                        , from = towerPosition
                        , ttl = 2
                        , color = projectileColor tower.towerType
                        }
                    )
                    targets

            _ =
                Debug.log "targets" targets

            _ =
                Debug.log "afterDamage" afterDamage

            _ =
                Debug.log "newProjectiles" newProjectiles
        in
        ( towersAfterAddingCooldown
        , afterDamage ++ outOfTargetCount ++ outOfRange ++ notSpawned
        , projectiles ++ newProjectiles
        )

    else
        ( Dict.update towerId
            (Maybe.map (\t -> { t | currentCooldown = t.currentCooldown - 1 }))
            towers
        , enemies
        , projectiles
        )


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
    let
        newModel =
            case msg of
                StepClicked ->
                    let
                        aliveEnemies =
                            model.enemies
                                |> List.filter (.hp >> (<) 0)

                        ( enemyDamage, enemiesMoved ) =
                            moveEnemies aliveEnemies

                        agedProjectiles =
                            model.projectiles
                                |> List.map (\p -> { p | ttl = p.ttl - 1 })
                                |> List.filter (.ttl >> (<) 0)

                        ( towers, enemies, projectiles ) =
                            Dict.foldl
                                towerEnemyInteraction
                                ( model.towers, enemiesMoved, agedProjectiles )
                                model.towers

                        state =
                            if not (List.isEmpty model.enemies) && List.isEmpty enemies then
                                Build buildsPerLevel

                            else
                                model.state
                    in
                    { model
                        | enemies = enemies
                        , towers = towers
                        , projectiles = projectiles
                        , hp = model.hp - enemyDamage
                        , state = state
                    }

                BuildCellClicked cell ->
                    case model.state of
                        Level ->
                            model

                        Build towersLeft ->
                            if towersLeft > 0 then
                                addTower cell model towersLeft

                            else
                                model

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
    in
    ( newModel, Cmd.none )


upgradeTower : Model -> TowerId -> TowerType -> Model
upgradeTower model towerId upgradeTo =
    let
        updatedTowers =
            case Dict.get towerId model.towers of
                Just tower ->
                    List.filter ((/=) tower.towerType) (towerCombination upgradeTo)
                        |> List.map
                            (\towerType ->
                                Dict.Extra.find
                                    (\_ t -> t.towerType == towerType)
                                    model.towers
                            )
                        |> List.filterMap identity
                        |> List.map Tuple.first
                        |> List.foldl (\id towers -> Dict.remove id towers) model.towers
                        |> Dict.update towerId (Maybe.map (.cellIndex >> createTower upgradeTo))

                Nothing ->
                    model.towers
    in
    { model | towers = updatedTowers }


spawnEnemies model =
    List.range 0 (enemiesPerLevel - 1)
        |> List.map (\count -> ( count, count * 5 ))
        |> List.map (\( count, cooldown ) -> createEnemy model.board (model.enemyIdCount + count) cooldown)


keepTower : TowerId -> Model -> Model
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
    in
    { model
        | towers = toKeep
        , board = boardWithStones
        , state = Level
        , enemies = spawnEnemies model
        , enemyIdCount = model.enemyIdCount + enemiesPerLevel
        , level = model.level + 1
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
            notSpawnedEnemies ++ otherEnemies
    in
    ( List.foldl (.damage >> (+)) 0 enemiesReachedGoal
    , List.map (\e -> { e | spawnTime = max 0 (e.spawnTime - 1) }) enemiesLeft
    )


availableSteps : Board -> AStar.Position -> Set AStar.Position
availableSteps board ( x, y ) =
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
            if up /= Nothing || left /= Nothing then
                getCell (index - boardWidth - 1)

            else
                Nothing

        upRight =
            if up /= Nothing || right /= Nothing then
                getCell (index - boardWidth + 1)

            else
                Nothing

        downRight =
            if down /= Nothing || right /= Nothing then
                getCell (index + boardWidth + 1)

            else
                Nothing

        downLeft =
            if down /= Nothing || left /= Nothing then
                getCell (index + boardWidth - 1)

            else
                Nothing

        walkable : Cell -> Maybe Cell
        walkable cell =
            case cell.cellType of
                Path maybeTower ->
                    if maybeTower == NoCellObject then
                        Just cell

                    else
                        Nothing

                Grass maybeTower ->
                    if maybeTower == NoCellObject then
                        Just cell

                    else
                        Nothing

                Start ->
                    Just cell

                Goal ->
                    Just cell

                Post ->
                    Just cell

        indexToCellPosition : Int -> ( Int, Int )
        indexToCellPosition i =
            ( modBy boardWidth i, i // boardWidth )
    in
    [ upLeft, up, upRight, right, downRight, down, downLeft, left ]
        |> List.filterMap identity
        |> List.map (.index >> indexToCellPosition)
        |> Set.fromList


findPath : Array Cell -> Position -> Position -> List Position
findPath cells from to =
    let
        positionToCellPosition : Position -> ( Int, Int )
        positionToCellPosition position =
            ( position.x // cellSize, position.y // cellSize )

        cellPositionToPosition : ( Int, Int ) -> Position
        cellPositionToPosition ( x, y ) =
            { x = (x * cellSize) + (cellSize // 2)
            , y = (y * cellSize) + (cellSize // 2)
            }

        path =
            AStar.findPath
                AStar.straightLineCost
                (availableSteps cells)
                (positionToCellPosition from)
                (positionToCellPosition to)
    in
    path
        |> Maybe.withDefault []
        |> List.map cellPositionToPosition


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


findFullPath : Board -> List Position
findFullPath board =
    let
        fullPath =
            List.foldl
                (\to totalPath ->
                    case List.Extra.last totalPath of
                        Just from ->
                            let
                                path =
                                    findPath board from to
                            in
                            if List.isEmpty path then
                                []

                            else
                                totalPath ++ path

                        Nothing ->
                            []
                )
                [ indexToCellCenterPosition startIndex ]
                (List.map indexToCellCenterPosition postIndices ++ [ indexToCellCenterPosition goalIndex ])
    in
    -- Remove start position since enemies start there
    List.tail fullPath |> Maybe.withDefault []


createEnemy : Board -> EnemyId -> Int -> Enemy
createEnemy board enemyId spawnTime =
    { position = indexToCellCenterPosition startIndex
    , path = findFullPath board
    , id = enemyId
    , hp = 500
    , damage = 2
    , spawnTime = spawnTime
    }


addTower : Cell -> Model -> Int -> Model
addTower cell model towersLeft =
    let
        ( seed, towerType ) =
            getTowerType model.seed

        tower =
            createTower towerType cell.index

        ( towerAdded, cellWithTower ) =
            addTowerToCell cell model.towerIdCount

        boardWithTower =
            Array.set cell.index cellWithTower model.board

        pathAfterAddingTower =
            not (List.isEmpty (findFullPath boardWithTower))

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

        selected =
            if success then
                TowerSelected model.towerIdCount

            else
                NothingSelected
    in
    { model
        | board = updatedBoard
        , towerIdCount = towerIdCount
        , towers = towers
        , state = state
        , selected = selected
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


calculateMovement : Position -> Position -> ( Int, Int )
calculateMovement from to =
    let
        deltaX =
            to.x - from.x

        deltaY =
            to.y - from.y
    in
    ( if deltaX > stepSize then
        1

      else if deltaX < -stepSize then
        -1

      else
        0
    , if deltaY > stepSize then
        1

      else if deltaY < -stepSize then
        -1

      else
        0
    )


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
            calculateMovement position toPosition

        nextPosition : Position
        nextPosition =
            { x = position.x + (deltaX * stepSize)
            , y = position.y + (deltaY * stepSize)
            }

        ( newPosition, path ) =
            if calculateMovement nextPosition toPosition == ( 0, 0 ) then
                --We have reached the position
                ( toPosition, List.tail enemy.path |> Maybe.withDefault [] )

            else
                ( nextPosition, enemy.path )
    in
    { enemy
        | position = newPosition
        , path = path
    }


view : Model -> Html Msg
view model =
    div
        [ class "main" ]
        [ viewSide model
        , div [ class "game" ]
            [ viewBoard model
            , div []
                (if model.state == Level then
                    [ button [ onClick StepClicked ] [ text "Step" ]
                    ]

                 else
                    []
                )
            ]
        ]


viewSelectedTowerInfo : Model -> Tower -> TowerId -> Html Msg
viewSelectedTowerInfo model tower towerId =
    let
        upgrades : List TowerType
        upgrades =
            if model.state == Level then
                availableUpgrades (List.map .towerType (Dict.values model.towers)) tower.towerType

            else
                []
    in
    div []
        ([ text ("Selected tower totalDamage: " ++ String.fromInt tower.totalDamage)
         , if tower.temporary then
            if model.state == Build 0 then
                button [ onClick (KeepTowerClicked towerId) ] [ text "Keep" ]

            else
                text ""

           else
            button [ onClick (RemoveTowerButtonClicked towerId tower.cellIndex) ] [ text "Remove" ]
         ]
            ++ List.map
                (\upgrade ->
                    button
                        [ onClick (UpgradeTowerClicked towerId upgrade) ]
                        [ text (towerTypeString upgrade) ]
                )
                upgrades
        )


viewSide : Model -> Html Msg
viewSide model =
    let
        stateString =
            case model.state of
                Level ->
                    "level " ++ String.fromInt model.level

                Build towersLeft ->
                    "build " ++ String.fromInt towersLeft

        content =
            case model.selected of
                TowerSelected towerId ->
                    case Dict.get towerId model.towers of
                        Just tower ->
                            viewSelectedTowerInfo model tower towerId

                        Nothing ->
                            text "Nothing"

                EnemySelected enemyId ->
                    case List.Extra.find (.id >> (==) enemyId) model.enemies of
                        Just enemy ->
                            text ("Selected enemy hp: " ++ String.fromInt enemy.hp)

                        Nothing ->
                            text "Nothing"

                NothingSelected ->
                    text "Nothing"

                StoneSelected int ->
                    div []
                        [ text ("Selected stone " ++ String.fromInt int)
                        , button [ onClick (RemoveStoneButtonClicked int) ] [ text "Remove" ]
                        ]

        ( temporaryTowerTypes, existingTowerTypes ) =
            model.towers |> Dict.values |> List.partition .temporary

        towerTypes towers =
            List.map .towerType towers
    in
    div [ class "side" ]
        [ text ("State: " ++ stateString)
        , br [] []
        , text ("Hp: " ++ String.fromInt model.hp)
        , br [] []
        , content
        , viewTowerInformation (towerTypes temporaryTowerTypes) (towerTypes existingTowerTypes)
        ]


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


viewBoard : Model -> Html Msg
viewBoard model =
    let
        cells =
            Array.toList
                (Array.map (viewCellGroup model.selected model.towers) (groupCells model.board))

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


viewProjectile : List Enemy -> Projectile -> Html Msg
viewProjectile enemies projectile =
    let
        maybeEnemy =
            List.Extra.find (.id >> (==) projectile.enemyId) enemies
    in
    case maybeEnemy of
        Just enemy ->
            let
                width =
                    distance
                        projectile.from
                        enemy.position
                        |> round
                        |> intToPxString

                ( left, top, angle ) =
                    if projectile.from.x <= enemy.position.x then
                        ( projectile.from.x
                        , projectile.from.y
                        , atan2
                            -(toFloat (projectile.from.x - enemy.position.x))
                            (toFloat (projectile.from.y - enemy.position.y))
                        )

                    else
                        ( enemy.position.x
                        , enemy.position.y
                        , atan2
                            -(toFloat (enemy.position.x - projectile.from.x))
                            (toFloat (enemy.position.y - projectile.from.y))
                        )
            in
            div
                [ class "projectile"
                , style "left" (intToPxString left)
                , style "top" (intToPxString top)
                , style "width" width
                , style "background-color" projectile.color
                , style "transform" ("rotate(" ++ String.fromFloat (angle - pi / 2) ++ "rad)")
                ]
                []

        Nothing ->
            div [] []


viewEnemy : Selected -> Enemy -> Html Msg
viewEnemy selected enemy =
    let
        enemySelected =
            case selected of
                EnemySelected enemyId ->
                    enemyId == enemy.id

                _ ->
                    False
    in
    div
        [ class "enemy"
        , style "width" (intToPxString enemySize)
        , style "height" (intToPxString enemySize)
        , style "left" (intToPxString (enemy.position.x - (enemySize // 2)))
        , style "top" (intToPxString (enemy.position.y - (enemySize // 2)))
        , class
            (if enemySelected then
                "selected"

             else
                ""
            )
        , onClick (EnemyClicked enemy)
        ]
        [ div [ class "enemy-inner" ] []
        ]


viewCellGroup : Selected -> Towers -> Array Cell -> Html Msg
viewCellGroup selected towers group =
    div [ class "cell-row" ]
        (Array.toList
            (Array.map (viewCell selected towers) group)
        )


viewCell : Selected -> Towers -> Cell -> Html Msg
viewCell selected towers cell =
    let
        ( cellClass, cellObject ) =
            case cell.cellType of
                Path cellObject_ ->
                    ( "cell-path", cellObject_ )

                Grass cellObject_ ->
                    ( "cell-grass", cellObject_ )

                Start ->
                    ( "cell-start", NoCellObject )

                Goal ->
                    ( "cell-goal", NoCellObject )

                Post ->
                    ( "cell-post", NoCellObject )

        towerSelected towerId =
            case selected of
                TowerSelected selectedTowerId ->
                    towerId == selectedTowerId

                _ ->
                    False

        stoneSelected =
            case selected of
                StoneSelected cellIndex ->
                    cellIndex == cell.index

                _ ->
                    False
    in
    div
        [ class "cell"
        , class cellClass
        , onClick
            (case cellObject of
                CellTower towerId ->
                    TowerClicked towerId

                Stone ->
                    StoneClicked cell.index

                NoCellObject ->
                    BuildCellClicked cell
            )
        , style "width" (intToPxString cellSize)
        , style "height" (intToPxString cellSize)
        ]
        ([]
            ++ (case cellObject of
                    CellTower towerId ->
                        let
                            maybeTower =
                                Dict.get towerId towers
                        in
                        case maybeTower of
                            Just tower ->
                                [ viewTower (towerSelected towerId) tower ]

                            Nothing ->
                                []

                    Stone ->
                        [ viewStone stoneSelected ]

                    NoCellObject ->
                        []
               )
        )


viewStone : Bool -> Html msg
viewStone selected =
    div
        [ class "stone"
        , style "width" (intToPxString stoneSize)
        , style "height" (intToPxString stoneSize)
        , class
            (if selected then
                "selected"

             else
                ""
            )
        ]
        [ div [ class "stone-inner" ] [] ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
