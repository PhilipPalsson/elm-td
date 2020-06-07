module Main exposing (..)

import AStar
import Array exposing (Array)
import Array.Extra
import Browser
import Html exposing (Html, button, div, text)
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
    10


startIndex =
    3


postIndices =
    [ 303, 326, 86, 75, 525 ]


goalIndex =
    536


type Tower
    = Regular


type CellType
    = Path (Maybe Tower)
    | Grass (Maybe Tower)
    | Start
    | Goal
    | Post


type alias Cell =
    { cellType : CellType, index : Int }


type alias Position =
    { x : Int, y : Int }


type alias Enemy =
    { position : Position, path : List Position }


type alias Board =
    Array Cell


type alias Model =
    { board : Board, enemies : List Enemy }


type Msg
    = StepClicked
    | CreateEnemyClicked
    | CellClicked Int


init : ( Model, Cmd Msg )
init =
    ( { board = initBoard
      , enemies = []
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                StepClicked ->
                    { model | enemies = step model.enemies }

                CreateEnemyClicked ->
                    { model | enemies = createEnemy model }

                CellClicked index ->
                    let
                        newBoard =
                            Array.Extra.update index addTower model.board

                        path =
                            findFullPath newBoard
                    in
                    if List.isEmpty path then
                        model

                    else
                        { model | board = newBoard }
    in
    ( newModel, Cmd.none )


step : List Enemy -> List Enemy
step enemies =
    enemies
        |> List.map moveEnemy
        |> List.Extra.filterNot (.path >> List.isEmpty)


availableSteps : Board -> AStar.Position -> Set AStar.Position
availableSteps board ( x, y ) =
    let
        index =
            y * boardWidth + x

        upLeft =
            index - boardWidth - 1

        up =
            upLeft + 1

        upRight =
            up + 1

        right =
            upRight + boardWidth

        downRight =
            right + boardWidth

        down =
            downRight - 1

        downLeft =
            down - 1

        left =
            down - boardWidth

        walkable : Cell -> Bool
        walkable cell =
            case cell.cellType of
                Path maybeTower ->
                    maybeTower == Nothing

                Grass maybeTower ->
                    maybeTower == Nothing

                Start ->
                    True

                Goal ->
                    True

                Post ->
                    True

        indexToCellPosition : Int -> ( Int, Int )
        indexToCellPosition i =
            ( modBy boardWidth i, i // boardWidth )

        wrongRow i =
            AStar.pythagoreanCost (indexToCellPosition i) (indexToCellPosition index) < 2.0
    in
    [ upLeft, up, upRight, right, downRight, down, downLeft, left ]
        |> List.filter wrongRow
        |> List.map (\i -> Array.get i board)
        |> List.filterMap identity
        |> List.filter walkable
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


createEnemy : Model -> List Enemy
createEnemy model =
    model.enemies
        ++ [ { position = indexToCellCenterPosition startIndex
             , path = findFullPath model.board
             }
           ]


addTower : Cell -> Cell
addTower cell =
    case cell.cellType of
        Path Nothing ->
            { cell | cellType = Path (Just Regular) }

        Path _ ->
            cell

        Grass Nothing ->
            { cell | cellType = Grass (Just Regular) }

        Grass _ ->
            cell

        Start ->
            cell

        Goal ->
            cell

        Post ->
            cell


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
        []
        [ viewBoard model
        , div []
            [ button [ onClick StepClicked ] [ text "Step" ]
            , button [ onClick CreateEnemyClicked ] [ text "Enemy" ]
            ]
        ]


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
                (Array.map viewCellGroup (groupCells model.board))
    in
    div [ class "board" ]
        [ div [ class "cells" ] cells
        , div [ class "enemies" ] (List.map viewEnemy model.enemies)
        ]


viewEnemy : Enemy -> Html msg
viewEnemy enemy =
    div
        [ class "enemy"
        , style "width" (intToPxString enemySize)
        , style "height" (intToPxString enemySize)
        , style "left" (intToPxString (enemy.position.x - (enemySize // 2)))
        , style "top" (intToPxString (enemy.position.y - (enemySize // 2)))
        ]
        []


viewCellGroup : Array Cell -> Html Msg
viewCellGroup group =
    div [ class "cell-row" ]
        (Array.toList
            (Array.map viewCell group)
        )


viewCell : Cell -> Html Msg
viewCell cell =
    let
        ( cellClass, tower ) =
            case cell.cellType of
                Path t ->
                    ( "cell-path", t )

                Grass t ->
                    ( "cell-grass", t )

                Start ->
                    ( "cell-start", Nothing )

                Goal ->
                    ( "cell-goal", Nothing )

                Post ->
                    ( "cell-post", Nothing )
    in
    div
        [ class "cell"
        , class cellClass
        , onClick (CellClicked cell.index)
        , style "width" (intToPxString cellSize)
        , style "height" (intToPxString cellSize)
        ]
        ([]
            ++ (case tower of
                    Just t ->
                        viewTower t

                    Nothing ->
                        []
               )
        )


viewTower tower =
    [ div [ class "tower" ] [] ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
