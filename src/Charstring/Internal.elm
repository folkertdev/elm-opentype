module Charstring.Internal exposing (Charstring, Operation(..), Point, Segment, decode, decodeSegments)

{-| The Charstring (CFF) internals


## Big Picture

A charstring is a list of numbers that encode drawing intructions like moveto, lineto, and curveto.

Because operators are normal numbers, we have to differentiate them from the arguments (which are also numbers).
This is done with a shifting scheme (in Charstring.Number)

The arguments come first and are pushed onto a stack (or really a dequeue, we mostly use first in first out).
When an operator is found, the arguments and the operator are bundled into a `Segment`.

A tricky thing is that while most operators only take these arguments, the masks can also chomp some bytes after
the operator token. This means that we have to decode segment-by-segment.

[spec]: https://www.adobe.com/content/dam/acom/en/devnet/font/pdfs/5177.Type2.pdf

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Charstring.Number as Number exposing (Number)
import Utils exposing (exactly)


type alias Charstring =
    List Operation


type alias Segment =
    { operator : Int, arguments : List Number.Number }


{-| Decode a charstring as a list of segments

This may give a problem when there are masks in a subroutine.
The bytes after the mask operator (that contain the masking bits) are added as an argument to the next instruction

-}
decodeSegments : Int -> Decoder (List Segment)
decodeSegments bytesRemaining =
    Decode.loop ( bytesRemaining, [] ) decodeSegmentsHelp


decodeSegmentsHelp : ( Int, List Segment ) -> Decoder (Step ( Int, List Segment ) (List Segment))
decodeSegmentsHelp ( bytesRemaining, segments ) =
    if bytesRemaining <= 0 then
        Decode.succeed (Done (List.reverse segments))

    else
        Decode.loop ( bytesRemaining, [] ) decodeSegmentHelp
            |> Decode.map (\( newBytesRemaining, segment ) -> Loop ( newBytesRemaining, segment :: segments ))


{-| The size of a Number in bytes

The size of a full number is known when reading its first byte

-}
numberByteSize : Int -> Int
numberByteSize byte =
    if byte == 28 then
        3

    else if byte == 255 then
        5

    else if byte >= 32 && byte <= 246 then
        1

    else
        2


{-| Decode a single segment up to and including the operator token
-}
decodeSegment : Int -> Decoder ( Int, Segment )
decodeSegment bytesRemaining =
    Decode.loop ( bytesRemaining, [] ) decodeSegmentHelp


decodeSegmentHelp : ( Int, List Number ) -> Decoder (Step ( Int, List Number ) ( Int, Segment ))
decodeSegmentHelp ( bytesRemaining, arguments ) =
    if bytesRemaining <= 0 then
        Decode.fail

    else
        Decode.unsignedInt8
            |> Decode.andThen
                (\byte ->
                    if isNumberByte byte then
                        Number.decodeHelp byte
                            |> Decode.map (\number -> Loop ( bytesRemaining - numberByteSize byte, number :: arguments ))

                    else if byte == 12 then
                        Decode.unsignedInt8
                            |> Decode.map (\v -> Done ( bytesRemaining - 2, { operator = v + 3072, arguments = List.reverse arguments } ))

                    else
                        Decode.succeed (Done ( bytesRemaining - 1, { operator = byte, arguments = List.reverse arguments } ))
                )


decode : Int -> { global : Array (List Segment), local : Maybe (Array (List Segment)) } -> Decoder Charstring
decode bytesRemaining { global, local } =
    Decode.loop ( bytesRemaining, { initialState | global = global, local = local }, [] ) decodeHelp
        |> Decode.map List.concat


decodeHelp : ( Int, State, List (List Operation) ) -> Decoder (Step ( Int, State, List (List Operation) ) (List (List Operation)))
decodeHelp ( bytesRemaining, state, accum ) =
    if bytesRemaining <= 0 then
        Decode.succeed (Done (List.reverse accum))

    else
        let
            addOperations ( newRemainder, operations, newState ) =
                Loop ( newRemainder, newState, operations :: accum )
        in
        case state.segments of
            [] ->
                decodeSegment bytesRemaining
                    |> Decode.andThen
                        (\( newBytesRemaining, segment ) ->
                            interpretSegment state ( newBytesRemaining, segment, [] )
                        )
                    |> Decode.map addOperations

            first :: rest ->
                interpretSegment { state | segments = rest } ( bytesRemaining, first, [] )
                    |> Decode.map addOperations


{-| Interpret a full segment to actually produce drawing instructions

Also counts the number of stems and reads the masking bytes correctly.

-}
interpretSegment : State -> ( Int, Segment, List Operation ) -> Decoder ( Int, List Operation, State )
interpretSegment state ( bytesRemaining, segment, operations ) =
    Decode.loop
        { state =
            { state
                | numbers = state.numbers ++ List.map Number.toInt segment.arguments
                , current = Nothing
                , length = List.length state.numbers + List.length segment.arguments
            }
        , bytesRemaining = bytesRemaining
        , operations = []
        , opcode = segment.operator
        }
        interpretSegmentLoop


type alias LoopState =
    { opcode : Int, bytesRemaining : Int, operations : List Operation, state : State }


interpretSegmentLoop : LoopState -> Decoder (Step LoopState ( Int, List Operation, State ))
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
                    Decode.succeed ( newBytesRemaining, NoOp, { state | numbers = [], current = Nothing } )

                Just ( op, newState ) ->
                    Decode.succeed ( newBytesRemaining, op, newState )


addNumberToList : Int -> State -> Decoder (Step State a)
addNumberToList byte state =
    Number.decodeHelp byte
        |> Decode.andThen
            (\number ->
                Decode.succeed
                    (Loop
                        { state
                            | numbers = state.numbers ++ [ Number.toInt number ]
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


{-| Decode an opcode.

If the opcode bytes is 12, then another byte needs to be decoded:
12 is a prefix for some more operations.

Otherwise we're done and can just succeed

-}
decodeOpcode : Int -> Decoder Int
decodeOpcode byte =
    if byte == 12 then
        Decode.unsignedInt8
            |> Decode.map (\v -> v + 3072)

    else
        Decode.succeed byte


{-| Is the current byte (the start of) a number?
-}
isNumberByte : Int -> Bool
isNumberByte byte =
    byte == 28 || byte >= 32


step : State -> Decoder (Step State (Result State ( Operation, State )))
step state =
    case state.current of
        Just op ->
            decodeOperation state op

        Nothing ->
            Decode.unsignedInt8
                |> Decode.andThen
                    (\byte ->
                        if isNumberByte byte then
                            addNumberToList byte state

                        else
                            decodeOpcode byte
                                |> Decode.andThen
                                    (decodeOperation
                                        { state
                                            | length = List.length state.numbers
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
    { numbers : List Int
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
    { numbers = []
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


mask : Int -> (List Int -> Operation) -> State -> Decoder (Result ( Int, Operation, State ) (Maybe ( Operation, State )))
mask op toOperation state =
    if not (List.isEmpty state.numbers) then
        -- handle implicit `vstem` operations
        case state.numbers of
            dx :: dStem :: rest ->
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
                        , numbers = rest
                      }
                    )
                    |> Ok
                    |> Decode.succeed

            _ ->
                Decode.succeed (Ok Nothing)

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
    if odd (List.length state.numbers) then
        case state.numbers of
            width :: rest ->
                Just ( Width width, { state | current = Just 1, numbers = rest } )

            _ ->
                Nothing

    else
        case state.numbers of
            y :: dy :: rest ->
                let
                    newY =
                        state.hstem + y
                in
                Just
                    ( HStem newY (newY + dy)
                    , { state
                        | numStems = state.numStems + 1
                        , hstem = newY + dy
                        , numbers = rest
                        , current =
                            if not (List.isEmpty rest) then
                                Just 1

                            else
                                Nothing
                      }
                    )

            _ ->
                Nothing


vstem : State -> Maybe ( Operation, State )
vstem state =
    if odd (List.length state.numbers) then
        case state.numbers of
            width :: rest ->
                Just ( Width width, { state | current = Just 3, numbers = rest } )

            _ ->
                Nothing

    else
        case state.numbers of
            x :: dx :: rest ->
                let
                    newX =
                        state.vstem + x
                in
                Just
                    ( VStem newX (newX + dx)
                    , { state
                        | numStems = state.numStems + 1
                        , vstem = newX + dx
                        , numbers = rest
                        , current =
                            if not (List.isEmpty rest) then
                                Just 3

                            else
                                Nothing
                      }
                    )

            _ ->
                Nothing


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
    case state.numbers of
        width :: rest ->
            Just
                ( Width width
                , { state
                    | current = Just op
                    , numbers = rest
                  }
                )

        _ ->
            Nothing


vmoveto : State -> Maybe ( Operation, State )
vmoveto state =
    if List.length state.numbers == 2 then
        movetoWidth state 4

    else
        case state.numbers of
            dy :: rest ->
                moveto state ( 0, dy ) rest

            _ ->
                Nothing


hmoveto : State -> Maybe ( Operation, State )
hmoveto state =
    if List.length state.numbers == 2 then
        movetoWidth state 22

    else
        case state.numbers of
            dx :: rest ->
                moveto state ( dx, 0 ) rest

            _ ->
                Nothing


rmoveto : State -> Maybe ( Operation, State )
rmoveto state =
    if List.length state.numbers == 3 then
        movetoWidth state 21

    else
        case state.numbers of
            dx :: dy :: rest ->
                moveto state ( dx, dy ) rest

            _ ->
                Nothing


lineto : Int -> State -> ( Int, Int ) -> List Int -> ( Operation, State )
lineto op state ( dx, dy ) newNumbers =
    let
        newPoint =
            { x = state.point.x + dx
            , y = state.point.y + dy
            }
    in
    ( LineTo newPoint
    , { state
        | numbers = newNumbers
        , point = newPoint
        , current =
            if List.isEmpty newNumbers then
                Nothing

            else
                Just op
      }
    )


rlineto : Int -> State -> Maybe ( Operation, State )
rlineto op state =
    case state.numbers of
        dx :: dy :: rest ->
            lineto op state ( dx, dy ) rest
                |> Just

        _ ->
            Nothing


linetoX op state =
    case state.numbers of
        dx :: rest ->
            lineto op state ( dx, 0 ) rest
                |> Just

        _ ->
            Nothing


linetoY op state =
    case state.numbers of
        dy :: rest ->
            lineto op state ( 0, dy ) rest
                |> Just

        _ ->
            Nothing


hlineto : State -> Maybe ( Operation, State )
hlineto state =
    if odd state.length == odd (List.length state.numbers) then
        linetoX 6 state

    else
        linetoY 6 state


vlineto : State -> Maybe ( Operation, State )
vlineto state =
    if odd state.length == odd (List.length state.numbers) then
        linetoY 7 state

    else
        linetoX 7 state


{-| Call a local subroutine
-}
callsubr : State -> Maybe ( Operation, State )
callsubr state =
    case state.local of
        Just local ->
            -- important difference with other operators is that the calls pop from the bottom/back of the stack
            unSnoc state.numbers
                |> Maybe.andThen
                    (\( index, previous ) ->
                        call local index { state | numbers = previous }
                    )

        Nothing ->
            let
                _ =
                    Debug.log "fail callsubr called but no local subrs" ()
            in
            unSnoc state.numbers
                |> Maybe.andThen
                    (\( index, previous ) ->
                        Just ( NoOp, { state | numbers = previous } )
                    )


{-| Call a global subroutine
-}
callgsubr : State -> Maybe ( Operation, State )
callgsubr state =
    unSnoc state.numbers
        |> Maybe.andThen
            (\( index, newNumbers ) ->
                call state.global index { state | numbers = newNumbers }
            )


rcurveline state =
    if List.length state.numbers == 2 then
        rlineto 24 state

    else
        rrcurveto 24 state


rlinecurve state =
    if List.length state.numbers == 6 then
        rrcurveto 25 state

    else
        rlineto 25 state


curveto cursor dx dy dx2 dy2 dx3 dy3 =
    let
        a =
            { x = cursor.x + dx, y = cursor.y + dy }

        b =
            { x = a.x + dx2, y = a.y + dy2 }

        newPoint =
            { x = b.x + dx3, y = b.y + dy3 }
    in
    ( newPoint, CurveTo a b newPoint )


curvetoHelper op state rest ( newPoint, operation ) =
    Just
        ( operation
        , { state
            | point = newPoint
            , numbers = rest
            , current =
                if List.isEmpty rest then
                    Nothing

                else
                    Just op
          }
        )


rrcurveto : Int -> State -> Maybe ( Operation, State )
rrcurveto op state =
    case state.numbers of
        dx :: dy :: dx2 :: dy2 :: dx3 :: dy3 :: rest ->
            curveto state.point dx dy dx2 dy2 dx3 dy3
                |> curvetoHelper op state rest

        _ ->
            Nothing


vvcurveto state =
    if odd (List.length state.numbers) then
        case state.numbers of
            dx :: dy :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx dy dx2 dy2 0 d
                    |> curvetoHelper 26 state rest

            _ ->
                Nothing

    else
        case state.numbers of
            dy :: dx2 :: dy2 :: d :: rest ->
                curveto state.point 0 dy dx2 dy2 0 d
                    |> curvetoHelper 26 state rest

            _ ->
                Nothing


hhcurveto state =
    if odd (List.length state.numbers) then
        case state.numbers of
            dy :: dx :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx dy dx2 dy2 d 0
                    |> curvetoHelper 27 state rest

            _ ->
                Nothing

    else
        case state.numbers of
            dx :: dx2 :: dy2 :: d :: rest ->
                curveto state.point dx 0 dx2 dy2 d 0
                    |> curvetoHelper 27 state rest

            _ ->
                Nothing


vhcurveto state =
    let
        ( _, cursor, curves ) =
            vhGeneralCase state
                |> vhSpecialCase1 state
                |> vhSpecialCase2 state
    in
    Just
        ( Many (List.reverse curves)
        , { state | numbers = [], current = Nothing, point = cursor }
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
                        -- impossible
                        ( stack, cursor, accum )

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
                        -- impossible
                        ( stack, cursor, accum )
    in
    looper 0 state.point state.numbers []


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
                -- impossible
                ( stack, cursor, accum )

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
                -- impossible
                ( stack, cursor, accum )

    else
        ( stack, cursor, accum )


hvcurveto : State -> Maybe ( Operation, State )
hvcurveto state =
    let
        ( _, cursor, curves ) =
            hvGeneralCase state
                |> hvSpecialCase1 state
                |> hvSpecialCase2 state
    in
    Just
        ( Many (List.reverse curves)
        , { state | numbers = [], current = Nothing, point = cursor }
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
                        -- impossible
                        ( stack, cursor, accum )

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
                        -- impossible
                        ( stack, cursor, accum )
    in
    looper 0 state.point state.numbers []


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
                -- impossible
                ( stack, cursor, accum )

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
                -- impossible
                ( stack, cursor, accum )

    else
        ( stack, cursor, accum )


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
            Just ( NoOp, { state | numbers = [], current = Nothing } )

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


even : Int -> Bool
even x =
    (x |> modBy 2) == 0


odd : Int -> Bool
odd x =
    (x |> modBy 2) /= 0


last : List a -> Maybe a
last list =
    case list of
        [ x ] ->
            Just x

        x :: xs ->
            last xs

        [] ->
            Nothing


unSnoc : List a -> Maybe ( a, List a )
unSnoc list =
    let
        go remainder accum =
            case remainder of
                [ x ] ->
                    Just ( x, List.reverse accum )

                [] ->
                    Nothing

                x :: xs ->
                    go xs (x :: accum)
    in
    go list []
