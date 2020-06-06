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


boardWidth =
    30


boardHeight =
    20


cellSize =
    26


stepSize =
    10


enemySize =
    10


startIndex =
    3


postIndices =
    [ 303, 326, 86, 75, 525 ]


goalIndex =
    536



---- MODEL ----


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


type alias BoardPosition =
    ( Int, Int )


type alias Position =
    { x : Int, y : Int }


type alias Enemy =
    { position : Position, path : List BoardPosition }


type alias Board =
    Array Cell


type alias Model =
    { board : Board, enemies : List Enemy }


initBoard : Array Cell
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


cellToBoardPosition : Cell -> BoardPosition
cellToBoardPosition cell =
    ( modBy boardWidth cell.index
    , cell.index // boardWidth
    )


boardPositionToPosition : BoardPosition -> Position
boardPositionToPosition ( x, y ) =
    { x = (x * cellSize) + (cellSize // 2)
    , y = (y * cellSize) + (cellSize // 2)
    }


indexToBoardPosition : Board -> Int -> BoardPosition
indexToBoardPosition board index =
    case Array.get index board of
        Just cell ->
            cellToBoardPosition cell

        Nothing ->
            ( 0, 0 )


init : ( Model, Cmd Msg )
init =
    let
        board =
            initBoard
    in
    ( { board = board
      , enemies = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = CellClicked Int
    | StepClicked
    | CreateEnemyClicked


positionToIndex : BoardPosition -> Int
positionToIndex ( x, y ) =
    y * boardWidth + x


availableSteps : Board -> BoardPosition -> Set BoardPosition
availableSteps board position =
    let
        sameRow index1 index2 =
            index1 // 30 == index2 // 30

        index =
            positionToIndex position

        above =
            index - boardWidth

        right =
            index + 1

        below =
            index + boardWidth

        left =
            index - 1

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
    in
    [ Array.get above board
    , if sameRow right index then
        Array.get right board

      else
        Nothing
    , Array.get below board
    , if sameRow left index then
        Array.get left board

      else
        Nothing
    ]
        |> List.filterMap identity
        |> List.filter walkable
        |> List.map cellToBoardPosition
        |> Set.fromList


findPath : Board -> BoardPosition -> BoardPosition -> List BoardPosition
findPath board from to =
    let
        path =
            AStar.findPath
                AStar.straightLineCost
                (availableSteps board)
                from
                to

        _ =
            Debug.log "from" from

        _ =
            Debug.log "to" to
    in
    path |> Maybe.withDefault []


createEnemy : Board -> Enemy
createEnemy board =
    let
        startPosition =
            indexToBoardPosition board startIndex

        postPositions =
            List.map (indexToBoardPosition board) postIndices ++ [ indexToBoardPosition board goalIndex ]

        addToPath : BoardPosition -> AStar.Path -> AStar.Path
        addToPath to path =
            case List.Extra.last path of
                Just from ->
                    path ++ findPath board from to

                Nothing ->
                    path
    in
    { position = boardPositionToPosition startPosition
    , path = List.foldl addToPath [ startPosition ] postPositions
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClicked index ->
            ( { model | board = Array.Extra.update index addTower model.board }, Cmd.none )

        StepClicked ->
            ( { model
                | enemies =
                    model.enemies
                        |> List.map moveEnemy
                        |> List.Extra.filterNot (.path >> List.isEmpty)
              }
            , Cmd.none
            )

        CreateEnemyClicked ->
            ( { model
                | enemies = model.enemies ++ [ createEnemy model.board ]
              }
            , Cmd.none
            )


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
    ( if deltaX > 5 then
        1

      else if deltaX < -5 then
        -1

      else
        0
    , if deltaY > 5 then
        1

      else if deltaY < -5 then
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
                    boardPositionToPosition next

                [] ->
                    position

        ( deltaX, deltaY ) =
            calculateMovement position toPosition

        newPosition : Position
        newPosition =
            { x = position.x + (deltaX * stepSize)
            , y = position.y + (deltaY * stepSize)
            }

        path =
            if calculateMovement newPosition toPosition == ( 0, 0 ) then
                --We have reached the position
                List.tail enemy.path |> Maybe.withDefault []

            else
                enemy.path
    in
    { enemy
        | position = newPosition
        , path = path
    }


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



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        cells =
            Array.toList
                (Array.map viewCellGroup (groupCells model.board))
    in
    div
        []
        [ div [ class "board" ]
            (cells
                ++ List.map viewEnemy model.enemies
            )
        , div []
            [ button [ onClick StepClicked ] [ text "Step" ]
            , button [ onClick CreateEnemyClicked ] [ text "Enemy" ]
            ]
        ]


intToPxString value =
    String.fromInt value ++ "px"


viewEnemy : Enemy -> Html Msg
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
