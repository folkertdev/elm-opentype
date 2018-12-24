module Charstring.Internal exposing (Charstring, Operation(..), Point, Segment, decode, decodeAsParts, decodeWithOptions)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charstring.Number as Number
import Deque exposing (Deque)
import Utils exposing (exactly)


type alias Charstring =
    List Operation


type alias Segment =
    { operator : Int, arguments : List Number.Number }


decodeAsParts : Int -> Decoder (List Segment)
decodeAsParts bytesRemaining =
    Decode.loop ( bytesRemaining, [], [] ) decodeAsPartsHelp


decodeAsPartsHelp ( bytesRemaining, arguments, segments ) =
    if bytesRemaining <= 0 then
        Decode.succeed (Done (List.reverse segments))

    else
        Decode.unsignedInt8
            |> Decode.andThen
                (\byte ->
                    if byte == 28 || byte >= 32 then
                        let
                            size =
                                if byte == 28 then
                                    3

                                else if byte == 255 then
                                    5

                                else if byte >= 32 && byte <= 246 then
                                    1

                                else
                                    2
                        in
                        Number.decodeHelp byte
                            |> Decode.map (\number -> Loop ( bytesRemaining - size, number :: arguments, segments ))

                    else if byte == 12 then
                        Decode.unsignedInt8
                            |> Decode.map (\v -> Loop ( bytesRemaining - 2, [], { operator = v + 3072, arguments = List.reverse arguments } :: segments ))

                    else
                        Decode.succeed (Loop ( bytesRemaining - 1, [], { operator = byte, arguments = List.reverse arguments } :: segments ))
                )


decodeSegment : Int -> Decoder ( Int, Segment )
decodeSegment bytesRemaining =
    Decode.loop ( bytesRemaining, [] ) decodeSegmentHelp


decodeSegmentHelp ( bytesRemaining, arguments ) =
    if bytesRemaining <= 0 then
        Decode.fail

    else
        Decode.unsignedInt8
            |> Decode.andThen
                (\byte ->
                    if byte == 28 || byte >= 32 then
                        let
                            size =
                                if byte == 28 then
                                    3

                                else if byte == 255 then
                                    5

                                else if byte >= 32 && byte <= 246 then
                                    1

                                else
                                    2
                        in
                        Number.decodeHelp byte
                            |> Decode.map (\number -> Loop ( bytesRemaining - size, number :: arguments ))

                    else if byte == 12 then
                        Decode.unsignedInt8
                            |> Decode.map (\v -> Done ( bytesRemaining - 2, { operator = v + 3072, arguments = List.reverse arguments } ))

                    else
                        Decode.succeed (Done ( bytesRemaining - 1, { operator = byte, arguments = List.reverse arguments } ))
                )


decode : Decoder Charstring
decode =
    operators initialState


decodeWithOptions : Int -> { global : Array (List Segment), local : Maybe (Array (List Segment)) } -> Decoder Charstring
decodeWithOptions bytesRemaining { global, local } =
    let
        looper ( rem, state, accum ) =
            if rem <= 0 then
                Decode.succeed (Done (List.reverse accum))

            else
                help rem state
                    |> Decode.map
                        (\( newRem, op, newState ) ->
                            Loop ( newRem, newState, op :: accum )
                        )
    in
    Decode.loop ( bytesRemaining, { initialState | global = global, local = local }, [] ) looper
        |> Decode.map List.concat


help : Int -> State -> Decoder ( Int, List Operation, State )
help bytesRemaining state =
    case state.segments of
        [] ->
            decodeSegment bytesRemaining
                |> Decode.andThen
                    (\( newBytesRemaining, segment ) ->
                        interpretSegment state ( newBytesRemaining, segment, [] )
                    )

        first :: rest ->
            interpretSegment { state | segments = rest } ( bytesRemaining, first, [] )


{-| Interpret a full segment
-}
interpretSegment state ( bytesRemaining, segment, operations ) =
    Decode.loop
        { state =
            { state
                | numbers = List.foldl Deque.pushBack state.numbers segment.arguments
                , current = Nothing
                , length = Deque.length state.numbers + List.length segment.arguments
            }
        , bytesRemaining = bytesRemaining
        , operations = []
        , opcode = segment.operator
        }
        interpretSegmentLoop


interpretSegmentLoop { opcode, bytesRemaining, operations, state } =
    interpretStatementHelp state opcode bytesRemaining
        |> Decode.andThen
            (\( newBytesRemaining, op, newState ) ->
                case newState.current of
                    Nothing ->
                        Decode.succeed (Done ( newBytesRemaining, List.reverse (op :: operations), newState ))

                    Just newOperator ->
                        Decode.succeed
                            (Loop
                                { opcode = newOperator
                                , bytesRemaining = newBytesRemaining
                                , operations = op :: operations
                                , state = newState
                                }
                            )
            )


interpretStatementHelp state opcode newBytesRemaining =
    let
        handleMask result =
            case result of
                Ok Nothing ->
                    let
                        _ =
                            Debug.log "hint mask caused a failure" ()
                    in
                    -- Decode.fail
                    Decode.succeed ( newBytesRemaining, NoOp, state )

                Ok (Just ( op, newState )) ->
                    Decode.succeed ( newBytesRemaining, op, newState )

                Err ( bytesChomped, op, newState ) ->
                    Decode.succeed ( newBytesRemaining - bytesChomped, op, newState )

        clearStack ( a, b, s ) =
            let
                _ =
                    Debug.log "clearing stack" s.numbers
            in
            ( a, b, { s | numbers = Deque.empty } )
    in
    case opcode of
        19 ->
            mask 19 HintMask state
                |> Decode.andThen handleMask

        20 ->
            mask 20 CounterMask state
                |> Decode.andThen handleMask

        _ ->
            case operator opcode state of
                Nothing ->
                    let
                        _ =
                            Debug.log "decodeWithOptionsHelp operator failed" ( opcode, state.numbers )
                    in
                    -- Decode.succeed ( newBytesRemaining, NoOp, { state | segments = List.drop 1 state.segments, current = Nothing } )
                    -- Decode.fail
                    Decode.succeed ( newBytesRemaining, NoOp, { state | numbers = Deque.empty, current = Nothing } )

                Just ( op, newState ) ->
                    Decode.succeed ( newBytesRemaining, op, newState )


opcodeDecoder byte =
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
                let
                    _ =
                        Debug.log "Failed with " (Debug.toString ( op, state ))
                in
                Debug.log "FAILLLL!" Decode.fail

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
                            opcodeDecoder byte
                                |> Decode.andThen
                                    (decodeOperation
                                        { state
                                            | length = Deque.length state.numbers
                                        }
                                    )
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
    | ClosePath
    | NoOp
    | Many (List Operation)


type alias State =
    { numbers : Deque Number.Number
    , numStems : Int
    , hstem : Int
    , vstem : Int
    , current : Maybe Int
    , point : Point
    , length : Int
    , local : Maybe (Array (List Segment))
    , global : Array (List Segment)
    , segments : List Segment
    , subroutines : Array (List Segment)
    , width : Maybe Int
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
    , global = Array.empty
    , segments = []
    , subroutines = Array.empty
    , width = Nothing
    }


popFront1 : Deque Number.Number -> Maybe ( Int, Deque Number.Number )
popFront1 deque =
    case Deque.popFront deque of
        ( Just number, d ) ->
            Just ( Number.toInt number, d )

        _ ->
            Nothing


popBack1 : Deque Number.Number -> Maybe ( Int, Deque Number.Number )
popBack1 deque =
    case Deque.popBack deque of
        ( Just number, d ) ->
            Just ( Number.toInt number, d )

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


call : Array (List Segment) -> Int -> State -> Maybe ( Operation, State )
call subroutines index state =
    let
        size =
            Array.length subroutines

        actualIndex =
            if size < 1240 then
                index + 107

            else if size < 33900 then
                index + 1131

            else
                index + 32768
    in
    case Array.get actualIndex subroutines of
        Just subroutine ->
            Just
                ( NoOp
                , { state
                    | segments = subroutine ++ state.segments
                  }
                )

        Nothing ->
            let
                _ =
                    Debug.log "!!! local call index out of range" ( ( index, actualIndex ), Array.length subroutines )
            in
            Just ( Debug.todo "local call index out of range", state )


callGlobal : Array (List Segment) -> Int -> State -> Maybe ( Operation, State )
callGlobal subroutines index state =
    let
        size =
            Array.length subroutines

        actualIndex =
            if size < 1240 then
                index + 107

            else if size < 33900 then
                index + 1131

            else
                index + 32768
    in
    case Array.get actualIndex subroutines of
        Just subroutine ->
            let
                _ =
                    -- Debug.log "adding global subroutine" ( subroutine, state.numbers )
                    ()
            in
            Just
                ( NoOp
                , { state
                    | segments = subroutine ++ state.segments
                  }
                )

        Nothing ->
            let
                _ =
                    Debug.log "!!! global call index out of range" ( ( index, actualIndex ), Array.length subroutines )
            in
            Just ( NoOp, state )


parseStems : State -> State
parseStems state =
    let
        -- TODO do this properly
        nomimalWidthX =
            0

        width =
            if (Deque.length state.numbers |> modBy 2) /= 0 then
                case state.width of
                    Nothing ->
                        popFront1 state.numbers
                            |> Maybe.map (Tuple.first >> (\v -> v + nomimalWidthX))

                    Just _ ->
                        state.width

            else
                state.width
    in
    { state
        | numStems = state.numStems + Deque.length state.numbers // 2
        , numbers = Deque.empty
        , width = width
    }


mask : Int -> (List Int -> Operation) -> State -> Decoder (Result ( Int, Operation, State ) (Maybe ( Operation, State )))
mask op toOperation state =
    if not (Deque.isEmpty state.numbers) then
        -- handle implicit `vstem` operations
        popFront2 state.numbers
            |> Maybe.andThen
                (\( ( dx, dStem ), newNumbers ) ->
                    let
                        x =
                            state.vstem + dx
                    in
                    Just
                        ( VStem x (x + dStem)
                        , { state
                            | numStems = 1 + state.numStems
                            , vstem = x + dStem
                            , current = Just op
                            , numbers =
                                -- TODO check thisnewNumbers
                                -- Deque.empty
                                newNumbers
                          }
                        )
                )
            |> Ok
            |> Decode.succeed

    else
        let
            newState =
                -- parseStems state
                state

            numBytes =
                (newState.numStems + 7) // 8
        in
        exactly numBytes Decode.unsignedInt8
            |> Decode.andThen
                (\bytes ->
                    Decode.succeed <|
                        Err <|
                            ( numBytes, toOperation bytes, { newState | current = Nothing } )
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
                        ( VStem newX (newX + dx)
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
            { x = state.point.x + dx
            , y = state.point.y + dy
            }
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


origin =
    { x = 0, y = 0 }


callsubr : State -> Maybe ( Operation, State )
callsubr state =
    case state.local of
        Just local ->
            popBack1 state.numbers
                |> Maybe.andThen
                    (\( index, newNumbers ) ->
                        call local index { state | numbers = newNumbers }
                    )

        Nothing ->
            let
                _ =
                    Debug.log "fail callsubr called but no local subrs" ()
            in
            popBack1 state.numbers
                |> Maybe.andThen
                    (\( index, newNumbers ) ->
                        Just ( NoOp, { state | numbers = newNumbers } )
                    )


callgsubr state =
    popBack1 state.numbers
        |> Maybe.andThen
            (\( index, newNumbers ) ->
                callGlobal state.global index { state | numbers = newNumbers }
            )


rcurveline state =
    if Deque.length state.numbers == 2 then
        rlineto 24 state

    else
        rrcurveto 24 state


rlinecurve state =
    if Deque.length state.numbers == 6 then
        rrcurveto 25 state

    else
        rlineto 25 state


vvcurveto state =
    let
        work ax dy dx2 dy2 d newNumbers =
            let
                a =
                    { x = ax, y = state.point.y + dy }

                b =
                    { x = a.x + dx2, y = a.y + dy2 }

                newPoint =
                    { x = b.x, y = b.y + d }
            in
            Just
                ( CurveTo a b newPoint
                , { state
                    | point = newPoint
                    , numbers = newNumbers
                    , current =
                        if Deque.isEmpty newNumbers then
                            Nothing

                        else
                            Just 26
                  }
                )
    in
    (if (Deque.length state.numbers |> modBy 2) /= 0 then
        popFront1 state.numbers
            |> Maybe.andThen
                (\( dx, newNumbers ) ->
                    Just ( state.point.x + dx, newNumbers )
                )

     else
        Just ( state.point.x, state.numbers )
    )
        |> Maybe.andThen
            (\( ax, n1 ) ->
                popFront1 n1
                    |> Maybe.andThen
                        (\( dy, n2 ) ->
                            popFront2 n2
                                |> Maybe.andThen
                                    (\( ( dx2, dy2 ), n3 ) ->
                                        popFront1 n3
                                            |> Maybe.andThen
                                                (\( d, n4 ) ->
                                                    work ax dy dx2 dy2 d n4
                                                )
                                    )
                        )
            )


hhcurveto state =
    (if (Deque.length state.numbers |> modBy 2) /= 0 then
        popFront1 state.numbers
            |> Maybe.andThen
                (\( dy, newNumbers ) ->
                    Just ( state.point.y + dy, newNumbers )
                )

     else
        Just ( state.point.y, state.numbers )
    )
        |> Maybe.andThen
            (\( ay, n1 ) ->
                popFront1 n1
                    |> Maybe.andThen
                        (\( dx, n2 ) ->
                            popFront2 n2
                                |> Maybe.andThen
                                    (\( ( dx2, dy2 ), n3 ) ->
                                        popFront1 n3
                                            |> Maybe.andThen
                                                (\( d, n4 ) ->
                                                    let
                                                        a =
                                                            { x = state.point.x + dx, y = ay }

                                                        b =
                                                            { x = a.x + dx2, y = a.y + dy2 }

                                                        newPoint =
                                                            { x = b.x + d, y = b.y }
                                                    in
                                                    Just
                                                        ( CurveTo a b newPoint
                                                        , { state
                                                            | point = newPoint
                                                            , numbers = n4
                                                            , current =
                                                                if Deque.isEmpty n4 then
                                                                    Nothing

                                                                else
                                                                    Just 27
                                                          }
                                                        )
                                                )
                                    )
                        )
            )


curvetoX op state =
    popFront1 state.numbers
        |> Maybe.andThen
            (\( dx, n1 ) ->
                popFront2 n1
                    |> Maybe.andThen
                        (\( ( dx2, dy2 ), n2 ) ->
                            popFront1 n2
                                |> Maybe.andThen
                                    (\( dy3, n3 ) ->
                                        (if Deque.length n3 == 1 then
                                            popFront1 n3

                                         else
                                            Just ( 0, n3 )
                                        )
                                            |> Maybe.andThen
                                                (\( dx3, n4 ) ->
                                                    let
                                                        a =
                                                            { x = state.point.x + dx, y = state.point.y }

                                                        b =
                                                            { x = a.x + dx2, y = a.y + dy2 }

                                                        newPoint =
                                                            { x = b.x + dx3, y = b.y + dy3 }
                                                    in
                                                    Just
                                                        ( CurveTo a b newPoint
                                                        , { state
                                                            | numbers = n4
                                                            , point = newPoint
                                                            , current =
                                                                if Deque.isEmpty n4 then
                                                                    Nothing

                                                                else
                                                                    Just op
                                                          }
                                                        )
                                                )
                                    )
                        )
            )


curvetoY op state =
    popFront1 state.numbers
        |> Maybe.andThen
            (\( dy, n1 ) ->
                popFront2 n1
                    |> Maybe.andThen
                        (\( ( dx2, dy2 ), n2 ) ->
                            popFront1 n2
                                |> Maybe.andThen
                                    (\( dx3, n3 ) ->
                                        (if Deque.length n3 == 1 then
                                            popFront1 n3

                                         else
                                            Just ( 0, n3 )
                                        )
                                            |> Maybe.andThen
                                                (\( dy3, n4 ) ->
                                                    let
                                                        a =
                                                            { x = state.point.x, y = state.point.y + dy }

                                                        b =
                                                            { x = a.x + dx2, y = a.y + dy2 }

                                                        newPoint =
                                                            { x = b.x + dx3, y = b.y + dy3 }
                                                    in
                                                    Just
                                                        ( CurveTo a b newPoint
                                                        , { state
                                                            | numbers = n4
                                                            , point = newPoint
                                                            , current =
                                                                if Deque.isEmpty n4 then
                                                                    Nothing

                                                                else
                                                                    Just op
                                                          }
                                                        )
                                                )
                                    )
                        )
            )


sizeComparison state =
    let
        n =
            8
    in
    ( (Bitwise.and state.length 0xFE |> modBy n) == 0
    , (Bitwise.and (Deque.length state.numbers) 0xFE |> modBy n) == 0
    )


vhcurveto state =
    {-
       let
           ( eight1, eight2 ) =
               sizeComparison state
       in
       if eight1 == eight2 then
           curvetoY 30 state

       else
           curvetoX 30 state
    -}
    let
        ( _, cursor, curves ) =
            vhGeneralCase state
                |> vhSpecialCase1 state
                |> vhSpecialCase2 state
    in
    Just
        ( Many (List.reverse curves)
        , { state | numbers = Deque.empty, current = Nothing, point = cursor }
        )


vhGeneralCase : State -> ( List Int, Point, List Operation )
vhGeneralCase state =
    let
        size =
            state.length

        countBezier =
            if (size |> modBy 4) == 1 then
                (size - 5) // 4

            else
                size // 4

        looper iteration cursor stack accum =
            if iteration >= countBezier then
                ( stack, cursor, accum )

            else if (iteration |> modBy 2) == 0 then
                case stack of
                    [] ->
                        ( stack, cursor, accum )

                    c0 :: c1 :: c2 :: c3 :: rest ->
                        let
                            p1 =
                                { x = cursor.x + 0, y = cursor.y + c0 }

                            p2 =
                                { x = p1.x + c1, y = p1.y + c2 }

                            p3 =
                                { x = p2.x + c3, y = p2.y + 0 }
                        in
                        looper (iteration + 1) p3 rest (CurveTo p1 p2 p3 :: accum)

                    _ ->
                        Debug.todo "wrong 1"

            else
                case stack of
                    [] ->
                        ( stack, cursor, accum )

                    c0 :: c1 :: c2 :: c3 :: rest ->
                        let
                            p1 =
                                { x = cursor.x + c0, y = cursor.y + 0 }

                            p2 =
                                { x = p1.x + c1, y = p1.y + c2 }

                            p3 =
                                { x = p2.x + 0, y = p2.y + c3 }
                        in
                        looper (iteration + 1) p3 rest (CurveTo p1 p2 p3 :: accum)

                    _ ->
                        Debug.todo "wrong 2"
    in
    looper 0 state.point (List.map Number.toInt <| Deque.toList state.numbers) []


vhSpecialCase1 : State -> ( List Int, Point, List Operation ) -> ( List Int, Point, List Operation )
vhSpecialCase1 state ( stack, cursor, accum ) =
    if (state.length |> modBy 8) == 5 then
        case stack of
            c0 :: c1 :: c2 :: c3 :: c4 :: rest ->
                let
                    p1 =
                        { x = cursor.x + 0, y = cursor.y + c0 }

                    p2 =
                        { x = p1.x + c1, y = p1.y + c2 }

                    p3 =
                        { x = p2.x + c3, y = p2.y + c4 }
                in
                ( rest, p3, CurveTo p1 p2 p3 :: accum )

            _ ->
                Debug.todo "wrong 3"

    else
        ( stack, cursor, accum )


vhSpecialCase2 : State -> ( List Int, Point, List Operation ) -> ( List Int, Point, List Operation )
vhSpecialCase2 state ( stack, cursor, accum ) =
    if (state.length |> modBy 8) == 1 then
        case stack of
            c0 :: c1 :: c2 :: c3 :: c4 :: rest ->
                let
                    p1 =
                        { x = cursor.x + c0, y = cursor.y + 0 }

                    p2 =
                        { x = p1.x + c1, y = p1.y + c2 }

                    p3 =
                        { x = p2.x + c4, y = p2.y + c3 }
                in
                ( rest, p3, CurveTo p1 p2 p3 :: accum )

            _ ->
                Debug.todo "wrong 3"

    else
        ( stack, cursor, accum )


hvcurveto : State -> Maybe ( Operation, State )
hvcurveto state =
    {-
       let
           ( eight1, eight2 ) =
               sizeComparison state
       in
       if eight1 == eight2 then
           curvetoX 31 state

       else
           curvetoY 31 state
    -}
    let
        ( _, cursor, curves ) =
            hvGeneralCase state
                |> hvSpecialCase1 state
                |> hvSpecialCase2 state
    in
    Just
        ( Many (List.reverse curves)
        , { state | numbers = Deque.empty, current = Nothing, point = cursor }
        )


hvGeneralCase : State -> ( List Int, Point, List Operation )
hvGeneralCase state =
    let
        size =
            state.length

        countBezier =
            if (size |> modBy 4) == 1 then
                (size - 5) // 4

            else
                size // 4

        looper iteration cursor stack accum =
            if iteration >= countBezier then
                ( stack, cursor, accum )

            else if (iteration |> modBy 2) == 0 then
                case stack of
                    [] ->
                        ( stack, cursor, accum )

                    c0 :: c1 :: c2 :: c3 :: rest ->
                        let
                            p1 =
                                { x = cursor.x + c0, y = cursor.y + 0 }

                            p2 =
                                { x = p1.x + c1, y = p1.y + c2 }

                            p3 =
                                { x = p2.x + 0, y = p2.y + c3 }
                        in
                        looper (iteration + 1) p3 rest (CurveTo p1 p2 p3 :: accum)

                    _ ->
                        Debug.todo "wrong 1"

            else
                case stack of
                    [] ->
                        ( stack, cursor, accum )

                    c0 :: c1 :: c2 :: c3 :: rest ->
                        let
                            p1 =
                                { x = cursor.x + 0, y = cursor.y + c0 }

                            p2 =
                                { x = p1.x + c1, y = p1.y + c2 }

                            p3 =
                                { x = p2.x + c3, y = p2.y + 0 }
                        in
                        looper (iteration + 1) p3 rest (CurveTo p1 p2 p3 :: accum)

                    _ ->
                        Debug.todo "wrong 2"
    in
    looper 0 state.point (List.map Number.toInt <| Deque.toList state.numbers) []


hvSpecialCase1 : State -> ( List Int, Point, List Operation ) -> ( List Int, Point, List Operation )
hvSpecialCase1 state ( stack, cursor, accum ) =
    if (state.length |> modBy 8) == 5 then
        case stack of
            c0 :: c1 :: c2 :: c3 :: c4 :: rest ->
                let
                    p1 =
                        { x = cursor.x + c0, y = cursor.y }

                    p2 =
                        { x = p1.x + c1, y = p1.y + c2 }

                    p3 =
                        { x = p2.x + c4, y = p2.y + c3 }
                in
                ( rest, p3, CurveTo p1 p2 p3 :: accum )

            _ ->
                Debug.todo "wrong 3"

    else
        ( stack, cursor, accum )


hvSpecialCase2 : State -> ( List Int, Point, List Operation ) -> ( List Int, Point, List Operation )
hvSpecialCase2 state ( stack, cursor, accum ) =
    if (state.length |> modBy 8) == 1 then
        case stack of
            c0 :: c1 :: c2 :: c3 :: c4 :: rest ->
                let
                    p1 =
                        { x = cursor.x, y = cursor.y + c0 }

                    p2 =
                        { x = p1.x + c1, y = p1.y + c2 }

                    p3 =
                        { x = p2.x + c3, y = p2.y + c4 }
                in
                ( rest, p3, CurveTo p1 p2 p3 :: accum )

            _ ->
                Debug.todo "wrong 3"

    else
        ( stack, cursor, accum )


popFront4 deque =
    popFront2 deque
        |> Maybe.andThen
            (\( x, n ) ->
                popFront2 n
                    |> Maybe.andThen
                        (\( y, nn ) ->
                            Just ( ( x, y ), nn )
                        )
            )


hvcurvetoPhase1 state =
    popFront4 state.numbers
        |> Maybe.andThen
            (\( ( ( v1, v2 ), ( v3, v4 ) ), newNumbers ) ->
                let
                    c1x =
                        state.point.x + v1

                    c1y =
                        state.point.y

                    c2x =
                        c1x + v2

                    c2y =
                        c1y + v3

                    y =
                        c2y + v4

                    ( newerNumbers, x ) =
                        case Deque.toList newNumbers of
                            [ internalX ] ->
                                ( Deque.empty, Number.toInt internalX )

                            _ ->
                                ( newNumbers, 0 )

                    p1 =
                        { x = c1x, y = c1y }

                    p2 =
                        { x = c2x, y = c2y }

                    p3 =
                        { x = x, y = y }

                    newState =
                        { state
                            | numbers = newerNumbers
                            , point = p3
                            , current =
                                if Deque.isEmpty newerNumbers then
                                    Nothing

                                else
                                    Just 31
                        }
                in
                if Deque.isEmpty newerNumbers then
                    Just
                        ( CurveTo p1 p2 p3
                        , newState
                        )

                else
                    hvcurvetoPhase2 p3 newState
            )


hvcurvetoPhase2 { x, y } state =
    popFront4 state.numbers
        |> Maybe.andThen
            (\( ( ( v1, v2 ), ( v3, v4 ) ), newNumbers ) ->
                let
                    c1x =
                        x

                    c1y =
                        y + v1

                    c2x =
                        c1x + v2

                    c2y =
                        c1y + v3

                    newX =
                        c2x + v4

                    ( newerNumbers, newY ) =
                        case Deque.toList newNumbers of
                            [ internalX ] ->
                                ( Deque.empty, Number.toInt internalX )

                            _ ->
                                ( newNumbers, 0 )

                    p1 =
                        { x = c1x, y = c1y }

                    p2 =
                        { x = c2x, y = c2y }

                    p3 =
                        { x = newX, y = newY }
                in
                Just
                    ( CurveTo p1 p2 p3
                    , { state
                        | numbers = newerNumbers
                        , point = p3
                        , current =
                            if Deque.isEmpty newerNumbers then
                                Nothing

                            else
                                Just 31
                      }
                    )
            )


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
            {-
               case pop state.subroutines of
                   Nothing ->
                       let
                           _ =
                               Debug.log "error: subroutine not available"
                       in
                       Just ( NoOp, state )

                   Just ( subroutine, rest ) ->
                       Just ( NoOp, { state | stream = subroutine, subroutines = rest } )
            -}
            Just ( NoOp, state )

        14 ->
            Just ( NoOp, { state | numbers = Deque.empty, current = Nothing } )

        -- Just ( ClosePath, initialState )
        19 ->
            -- mask 19 HintMask state
            Nothing

        20 ->
            -- mask 20 CounterMask state
            Nothing

        21 ->
            rmoveto state

        22 ->
            hmoveto state

        24 ->
            rcurveline state

        25 ->
            rlinecurve state

        26 ->
            vvcurveto state

        27 ->
            hhcurveto state

        29 ->
            callgsubr state

        30 ->
            vhcurveto state

        31 ->
            hvcurveto state

        _ ->
            let
                _ =
                    Debug.log "unkown charstring operator" code
            in
            Nothing


pop : Array a -> Maybe ( a, Array a )
pop array =
    case Array.get (Array.length array - 1) array of
        Nothing ->
            Nothing

        Just element ->
            Just ( element, Array.slice 0 -1 array )
