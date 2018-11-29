module Charstring.Internal exposing (Charstring, Operation, Point, decode)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charstring.Number as Number
import Deque exposing (Deque)


type alias Charstring =
    List Operation


decode : Decoder Charstring
decode =
    operators initialState


opcode byte =
    if byte == 12 then
        Decode.unsignedInt8
            -- TODO why is this?
            |> Decode.map (\v -> v + 3072)

    else
        Decode.succeed byte


addNumberToDeque : Int -> State -> Decoder (Step State a)
addNumberToDeque byte state =
    Number.decodeHelp byte
        |> Decode.andThen
            (\number ->
                Decode.succeed
                    (Loop
                        { state
                            | numbers = Deque.pushBack number state.numbers
                        }
                    )
            )


decodeOperation : State -> Int -> Decoder (Step State (Result State ( Operation, State )))
decodeOperation state op =
    if op == 14 then
        Decode.succeed (Done (Err state))

    else
        case operator op state of
            Nothing ->
                Decode.succeed (Loop state)

            Just ( operation, newState ) ->
                Decode.succeed (Done (Ok ( operation, newState )))


step : State -> Decoder (Step State (Result State ( Operation, State )))
step state =
    case state.current of
        Just op ->
            decodeOperation state op

        Nothing ->
            Decode.unsignedInt8
                |> Decode.andThen
                    (\byte ->
                        if byte == 28 || byte >= 32 then
                            addNumberToDeque byte state

                        else
                            opcode byte
                                |> Decode.andThen (decodeOperation state)
                    )


operators : State -> Decoder (List Operation)
operators state =
    Decode.loop ( state, [] ) operatorsHelp


operatorsHelp ( state, accum ) =
    Decode.loop state step
        |> Decode.andThen
            (\result ->
                case result of
                    Err newState ->
                        Decode.succeed (Done (List.reverse accum))

                    Ok ( newOperation, newState ) ->
                        Decode.succeed (Loop ( newState, newOperation :: accum ))
            )


type alias Point =
    { x : Int, y : Int }


type Operation
    = CounterMask (List Int)
    | CurveTo Point Point Point
    | HintMask (List Int)
    | HStem Int Int
    | LineTo Point
    | MoveTo Point
    | VStem Int Int
    | Width Int
    | NoOp


type alias State =
    { numbers : Deque Number.Number
    , numStems : Int
    , hstem : Int
    , vstem : Int
    , current : Maybe Int
    , point : Point
    , length : Int
    , local : Maybe Int
    }


initialState : State
initialState =
    { numbers = Deque.empty
    , numStems = 0
    , hstem = 0
    , vstem = 0
    , current = Nothing
    , point = Point 0 0
    , length = 0
    , local = Nothing
    }


popFront1 : Deque Number.Number -> Maybe ( Int, Deque Number.Number )
popFront1 deque =
    case Deque.popFront deque of
        ( Just (Number.Integer v), d ) ->
            Just ( v, d )

        _ ->
            Nothing


popFront2 : Deque Number.Number -> Maybe ( ( Int, Int ), Deque Number.Number )
popFront2 deque =
    popFront1 deque
        |> Maybe.andThen
            (\( first, newState ) ->
                popFront1 newState
                    |> Maybe.andThen
                        (\( second, newerState ) ->
                            Just ( ( first, second ), newerState )
                        )
            )


hstem : State -> Maybe ( Operation, State )
hstem state =
    if (Deque.length state.numbers |> modBy 2) /= 0 then
        case popFront1 state.numbers of
            Nothing ->
                Nothing

            Just ( width, newNumbers ) ->
                Just ( Width width, { state | current = Just 1, numbers = newNumbers } )

    else
        popFront2 state.numbers
            |> Maybe.andThen
                (\( ( y, dy ), newNumbers ) ->
                    let
                        newY =
                            state.hstem + y
                    in
                    Just
                        ( HStem newY (newY + dy)
                        , { state
                            | numStems = state.numStems + 1
                            , hstem = newY + dy
                            , numbers = newNumbers
                            , current =
                                if not (Deque.isEmpty newNumbers) then
                                    Just 1

                                else
                                    Nothing
                          }
                        )
                )


vstem : State -> Maybe ( Operation, State )
vstem state =
    if (Deque.length state.numbers |> modBy 2) /= 0 then
        case popFront1 state.numbers of
            Nothing ->
                Nothing

            Just ( width, newNumbers ) ->
                Just ( Width width, { state | current = Just 3, numbers = newNumbers } )

    else
        popFront2 state.numbers
            |> Maybe.andThen
                (\( ( x, dx ), newNumbers ) ->
                    let
                        newX =
                            state.vstem + x
                    in
                    Just
                        ( HStem newX (newX + dx)
                        , { state
                            | numStems = state.numStems + 1
                            , vstem = newX + dx
                            , numbers = newNumbers
                            , current =
                                if not (Deque.isEmpty newNumbers) then
                                    Just 3

                                else
                                    Nothing
                          }
                        )
                )


moveto state ( dx, dy ) newNumbers =
    let
        newPoint =
            { x = state.point.x + dx, y = state.point.y + dy }
    in
    Just
        ( MoveTo newPoint
        , { state
            | point = newPoint
            , current = Nothing
            , numbers = newNumbers
          }
        )


movetoWidth state op =
    popFront1 state.numbers
        |> Maybe.andThen
            (\( width, newNumbers ) ->
                Just
                    ( Width width
                    , { state
                        | current = Just op
                        , numbers = newNumbers
                      }
                    )
            )


vmoveto : State -> Maybe ( Operation, State )
vmoveto state =
    if Deque.length state.numbers == 2 then
        movetoWidth state 4

    else
        popFront1 state.numbers
            |> Maybe.andThen
                (\( dy, newNumbers ) ->
                    moveto state ( 0, dy ) newNumbers
                )


hmoveto : State -> Maybe ( Operation, State )
hmoveto state =
    if Deque.length state.numbers == 2 then
        movetoWidth state 22

    else
        popFront1 state.numbers
            |> Maybe.andThen
                (\( dx, newNumbers ) ->
                    moveto state ( dx, 0 ) newNumbers
                )


rmoveto : State -> Maybe ( Operation, State )
rmoveto state =
    if Deque.length state.numbers == 3 then
        movetoWidth state 21

    else
        popFront2 state.numbers
            |> Maybe.andThen
                (\( ( dx, dy ), newNumbers ) ->
                    moveto state ( dx, dy ) newNumbers
                )


lineto op state ( dx, dy ) newNumbers =
    let
        newPoint =
            { x = state.point.x + dx, y = state.point.y + dy }
    in
    Just
        ( LineTo newPoint
        , { state
            | numbers = newNumbers
            , point = newPoint
            , current =
                if Deque.isEmpty newNumbers then
                    Nothing

                else
                    Just op
          }
        )


rlineto : Int -> State -> Maybe ( Operation, State )
rlineto op state =
    popFront2 state.numbers
        |> Maybe.andThen
            (\( ( dx, dy ), newNumbers ) ->
                lineto op state ( dx, dy ) newNumbers
            )


linetoX op state =
    popFront1 state.numbers
        |> Maybe.andThen
            (\( dx, newNumbers ) ->
                lineto op state ( dx, 0 ) newNumbers
            )


linetoY op state =
    popFront1 state.numbers
        |> Maybe.andThen
            (\( dy, newNumbers ) ->
                lineto op state ( 0, dy ) newNumbers
            )


hlineto : State -> Maybe ( Operation, State )
hlineto state =
    if ((state.length |> modBy 2) /= 0) == ((Deque.length state.numbers |> modBy 2) /= 0) then
        linetoX 6 state

    else
        linetoY 6 state


vlineto : State -> Maybe ( Operation, State )
vlineto state =
    if ((state.length |> modBy 2) /= 0) == ((Deque.length state.numbers |> modBy 2) /= 0) then
        linetoY 7 state

    else
        linetoX 7 state


rrcurveto : Int -> State -> Maybe ( Operation, State )
rrcurveto op state =
    popFront2 state.numbers
        |> Maybe.andThen
            (\( ( d1, d2 ), s2 ) ->
                let
                    a =
                        { x = state.point.x + d1, y = state.point.y + d2 }
                in
                popFront2 s2
                    |> Maybe.andThen
                        (\( ( d3, d4 ), s3 ) ->
                            let
                                b =
                                    { x = a.x + d3, y = a.y + d4 }
                            in
                            popFront2 s3
                                |> Maybe.andThen
                                    (\( ( d5, d6 ), s4 ) ->
                                        let
                                            newPoint =
                                                { x = b.x + d5, y = b.y + d6 }
                                        in
                                        Just
                                            ( CurveTo a b newPoint
                                            , { state
                                                | numbers = s4
                                                , point = newPoint
                                                , current =
                                                    if Deque.isEmpty s4 then
                                                        Nothing

                                                    else
                                                        Just op
                                              }
                                            )
                                    )
                        )
            )


callsubr : State -> Maybe ( Operation, State )
callsubr state =
    case state.local of
        Just local ->
            popFront1 state.numbers
                |> Maybe.andThen
                    (\( index, newNumbers ) ->
                        -- TODO execute the local subr at index
                        Just ( NoOp, { state | numbers = newNumbers } )
                    )

        Nothing ->
            Nothing


operator : Int -> State -> Maybe ( Operation, State )
operator code state =
    case code of
        1 ->
            hstem state

        18 ->
            hstem state

        3 ->
            vstem state

        23 ->
            vstem state

        4 ->
            vmoveto state

        5 ->
            rlineto 5 state

        6 ->
            hlineto state

        7 ->
            vlineto state

        8 ->
            rrcurveto 8 state

        10 ->
            callsubr state

        11 ->
            Just ( NoOp, state )

        14 ->
            Just ( NoOp, state )

        19 ->
            -- Debug.todo "mask "
            Just ( NoOp, state )

        20 ->
            -- Debug.todo "mask "
            Just ( NoOp, state )

        21 ->
            rmoveto state

        22 ->
            hmoveto state

        29 ->
            Just ( NoOp, state )

        30 ->
            Just ( NoOp, state )

        _ ->
            let
                _ =
                    Debug.log "unkown charstring operator" code
            in
            Nothing
