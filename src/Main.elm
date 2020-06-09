module Main exposing (main)

import AStar
import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List.Extra
import Set exposing (Set)



-- 37 * 37


boardWidth =
    30


boardHeight =
    20


cellSize =
    26


stepSize =
    5


enemySize =
    16


towerSize =
    16


stoneSize =
    16


startIndex =
    3


postIndices =
    [ 303, 326, 86, 75, 525 ]


goalIndex =
    536


type GameState
    = Level
    | Build


type Selected
    = TowerSelected TowerId
    | EnemySelected EnemyId
    | StoneSelected Int
    | NothingSelected


type alias Projectile =
    { enemyId : EnemyId, from : Position, ttl : Int }


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
    }


type alias Tower =
    { damage : Int
    , totalDamage : Int
    , range : Int
    , cellIndex : Int
    , cooldown : Int
    , currentCooldown : Int
    , targets : Int
    }


type alias TowerId =
    Int


type alias EnemyId =
    Int


type alias Towers =
    Dict TowerId Tower


type CellObject
    = CellTower TowerId
    | Stone


type CellType
    = Path (Maybe CellObject)
    | Grass (Maybe CellObject)
    | Start
    | Goal
    | Post


type alias Cell =
    { cellType : CellType, index : Int }


type alias Position =
    { x : Int, y : Int }


type alias Enemy =
    { position : Position
    , path : List Position
    , id : Int
    , hp : Int
    , damage : Int
    }


type alias Board =
    Array Cell


type Msg
    = StepClicked
    | CreateEnemyClicked
    | BuildCellClicked Cell
    | TowerClicked TowerId
    | StoneClicked Int
    | EnemyClicked Enemy


init : ( Model, Cmd Msg )
init =
    ( { board = initBoard
      , enemies = []
      , enemyIdCount = 0
      , state = Build
      , selected = NothingSelected
      , towerIdCount = 0
      , towers = Dict.empty
      , projectiles = []
      , hp = 100
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
                Path Nothing

            else if List.member index [ 1 ] then
                Grass (Just Stone)

            else
                Grass Nothing
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

            ( inRange, outOfRange ) =
                List.partition enemyInRange enemies

            ( targets, outOfTargetCount ) =
                List.Extra.splitAt tower.targets inRange

            afterDamage =
                List.map dealDamage targets
                    |> List.filter (.hp >> (<) 0)

            towersAfterDealingDamage =
                Dict.update towerId
                    (Maybe.map (\t -> { t | totalDamage = t.totalDamage + List.length inRange }))
                    towers

            towersAfterAddingCooldown =
                Dict.update towerId
                    (Maybe.map (\t -> { t | currentCooldown = t.cooldown }))
                    towersAfterDealingDamage

            newProjectiles =
                List.map (\enemy -> { enemyId = enemy.id, from = towerPosition, ttl = 2 }) targets
        in
        ( towersAfterAddingCooldown, afterDamage ++ outOfTargetCount ++ outOfRange, projectiles ++ newProjectiles )

    else
        ( Dict.update towerId
            (Maybe.map (\t -> { t | currentCooldown = t.currentCooldown - 1 }))
            towers
        , enemies
        , projectiles
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                StepClicked ->
                    let
                        ( enemyDamage, enemiesMoved ) =
                            moveEnemies model.enemies

                        agedProjectiles =
                            model.projectiles
                                |> List.map (\p -> { p | ttl = p.ttl - 1 })
                                |> List.filter (.ttl >> (<) 0)

                        ( towerDamageDone, enemiesDamaged, withNewProjectiles ) =
                            Dict.foldl
                                towerEnemyInteraction
                                ( model.towers, enemiesMoved, agedProjectiles )
                                model.towers
                    in
                    { model
                        | enemies = enemiesDamaged
                        , towers = towerDamageDone
                        , projectiles = withNewProjectiles
                        , hp = model.hp - enemyDamage
                    }

                CreateEnemyClicked ->
                    { model | enemies = createEnemy model, enemyIdCount = model.enemyIdCount + 1 }

                BuildCellClicked cell ->
                    addTower cell model

                TowerClicked towerId ->
                    { model | selected = TowerSelected towerId }

                EnemyClicked enemy ->
                    { model | selected = EnemySelected enemy.id }

                StoneClicked cellIndex ->
                    { model | selected = StoneSelected cellIndex }
    in
    ( newModel, Cmd.none )


moveEnemies : List Enemy -> ( Int, List Enemy )
moveEnemies enemies =
    let
        ( enemiesReachedGoal, otherEnemies ) =
            enemies
                |> List.map moveEnemy
                |> List.partition (.path >> List.isEmpty)
    in
    ( List.foldl (.damage >> (+)) 0 enemiesReachedGoal
    , otherEnemies
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
                    if maybeTower == Nothing then
                        Just cell

                    else
                        Nothing

                Grass maybeTower ->
                    if maybeTower == Nothing then
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


createEnemy : Model -> List Enemy
createEnemy model =
    model.enemies
        ++ [ { position = indexToCellCenterPosition startIndex
             , path = findFullPath model.board
             , id = model.enemyIdCount
             , hp = 1000
             , damage = 2
             }
           ]


addTower : Cell -> Model -> Model
addTower cell model =
    let
        tower : Tower
        tower =
            { damage = 1
            , totalDamage = 0
            , range = 100
            , cellIndex = cell.index
            , cooldown = 5
            , currentCooldown = 0
            , targets = 1
            }

        ( towerAdded, cellWithTower ) =
            addTowerToCell cell model.towerIdCount

        boardWithTower =
            Array.set cell.index cellWithTower model.board

        pathAfterAddingTower =
            not (List.isEmpty (findFullPath boardWithTower))

        ( updatedBoard, towerIdCount, towers ) =
            if towerAdded && pathAfterAddingTower then
                ( boardWithTower
                , model.towerIdCount + 1
                , Dict.insert model.towerIdCount tower model.towers
                )

            else
                ( model.board
                , model.towerIdCount
                , model.towers
                )
    in
    { model | board = updatedBoard, towerIdCount = towerIdCount, towers = towers }


addTowerToCell : Cell -> TowerId -> ( Bool, Cell )
addTowerToCell cell towerId =
    case cell.cellType of
        Path Nothing ->
            ( True, { cell | cellType = Path (Just (CellTower towerId)) } )

        Path _ ->
            ( False, cell )

        Grass Nothing ->
            ( True, { cell | cellType = Grass (Just (CellTower towerId)) } )

        Grass _ ->
            ( False, cell )

        Start ->
            ( False, cell )

        Goal ->
            ( False, cell )

        Post ->
            ( False, cell )


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
                [ button [ onClick StepClicked ] [ text "Step" ]
                , button [ onClick CreateEnemyClicked ] [ text "Enemy" ]
                ]
            ]
        ]


viewSide : Model -> Html Msg
viewSide model =
    let
        content =
            case model.selected of
                TowerSelected towerId ->
                    case Dict.get towerId model.towers of
                        Just tower ->
                            "Tower totalDamage: " ++ String.fromInt tower.totalDamage

                        Nothing ->
                            "Nothing"

                EnemySelected enemyId ->
                    case List.Extra.find (.id >> (==) enemyId) model.enemies of
                        Just enemy ->
                            "Enemy hp: " ++ String.fromInt enemy.hp

                        Nothing ->
                            "Nothing"

                NothingSelected ->
                    "Nothing"

                StoneSelected int ->
                    "Stone " ++ String.fromInt int
    in
    div [ class "side" ]
        [ text ("Hp: " ++ String.fromInt model.hp)
        , br [] []
        , text ("Selected: " ++ content)
        ]


intToPxString : Int -> String
intToPxString value =
    String.fromInt value ++ "px"


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
    in
    div [ class "board" ]
        [ div [ class "cells" ] cells
        , div [ class "enemies" ] (List.map (viewEnemy model.selected) model.enemies)
        , div [ class "projectiles" ] (List.map (viewProjectile model.enemies) model.projectiles)
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
        ( cellClass, maybeCellObject ) =
            case cell.cellType of
                Path cellObject ->
                    ( "cell-path", cellObject )

                Grass cellObject ->
                    ( "cell-grass", cellObject )

                Start ->
                    ( "cell-start", Nothing )

                Goal ->
                    ( "cell-goal", Nothing )

                Post ->
                    ( "cell-post", Nothing )

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
            (case maybeCellObject of
                Just (CellTower towerId) ->
                    TowerClicked towerId

                Just Stone ->
                    StoneClicked cell.index

                Nothing ->
                    BuildCellClicked cell
            )
        , style "width" (intToPxString cellSize)
        , style "height" (intToPxString cellSize)
        ]
        ([]
            ++ (case maybeCellObject of
                    Just (CellTower towerId) ->
                        let
                            maybeTower =
                                Dict.get towerId towers
                        in
                        case maybeTower of
                            Just tower ->
                                [ viewTower (towerSelected towerId) tower ]

                            Nothing ->
                                []

                    Just Stone ->
                        [ viewStone stoneSelected ]

                    Nothing ->
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


viewTower : Bool -> Tower -> Html msg
viewTower selected tower =
    div
        [ class "tower"
        , style "width" (intToPxString towerSize)
        , style "height" (intToPxString towerSize)
        , class
            (if selected then
                "selected"

             else
                ""
            )
        ]
        [ div
            [ class "tower-inner"
            ]
            []
        , div
            [ class "tower-range"
            , style "width" (intToPxString (tower.range * 2))
            , style "height" (intToPxString (tower.range * 2))
            , style "top" (intToPxString (towerSize // 2 - ((tower.range * 2) // 2)))
            , style "left" (intToPxString (towerSize // 2 - ((tower.range * 2) // 2)))
            ]
            []
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
