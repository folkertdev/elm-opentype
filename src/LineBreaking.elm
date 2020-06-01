module LineBreaking exposing (x)

-- https://github.com/bramstein/typeset/blob/619fadd8b66db6b32ec810a2a51035e772042223/src/linebreak.js

import Array exposing (Array)


x =
    5


type alias Options =
    { demerits : Demerits
    , tolerance : Float
    }


type alias Demerits =
    { line : Float, flagged : Float, fitness : Float }


defaultDemerits =
    Demerits 10 100 3000


defaultOptions =
    { demerits = defaultDemerits, tolerance = 2 }


type Item
    = IBox Box
    | IGlue Glue
    | IPenalty Penalty


type alias Box =
    { width : Float }


type alias Glue =
    { width : Float, stretchability : Float, shrinkability : Float }


type alias Penalty =
    { p : Float, width : Float, flagged : Bool }


getWidth : Item -> Float
getWidth item =
    case item of
        IBox r ->
            r.width

        IGlue r ->
            r.width

        IPenalty r ->
            r.width


infinity : Float
infinity =
    10000


minusInfinity : Float
minusInfinity =
    -10000


type alias Sum =
    { width : Float
    , stretch : Float
    , shrink : Float
    }


emptySum : Sum
emptySum =
    Sum 0 0 0


{-| Add width, stretch and shrink values from the current
break point up to the next box or forced penalty.
-}
computeSum : Array Item -> Int -> Sum -> Sum
computeSum nodes breakPointIndex sum =
    let
        go i result =
            if i < Array.length nodes then
                case Array.get i nodes of
                    Just (IGlue glue) ->
                        go (i + 1)
                            { result
                                | width = result.width + glue.width
                                , stretch = result.stretch + glue.stretch
                                , shrink = result.shrink + glue.shrink
                            }

                    Just (IBox _) ->
                        result

                    Just (IPenalty r) ->
                        if r.p == -infinity && i > breakPointIndex then
                            result

                        else
                            go (i + 1) result

            else
                result
    in
    go 0 sum


computeCost start end nodes active currentLine sum =
    let
        width =
            (sum.width - active.totals.width) + penaltyEnd

        penaltyEnd =
            case Array.get end nodes of
                Just ((IPenalty _) as item) ->
                    getWidth item

                _ ->
                    0

        lineLength =
            if currentLine < lineLengths.length then
                Array.get (currentLine - 1) lineLengths

            else
                Array.get (lineLengths.length - 1) lineLengths
    in
    if width < lineLength then
        let
            stretch =
                sum.stretch - active.totals.stretch
        in
        if stretch > 0 then
            (lineLength - width) / stretch

        else
            infinity

    else if width > lineLength then
        let
            shrink =
                sum.shrink - active.totals.shrink
        in
        if shrink > 0 then
            (lineLength - width) / shrink

        else
            infinity

    else
        0


breakpoint position demerits ratio line fitnessClass totals previous =
    { position = position
    , demerits = demerits
    , ratio = ratio
    , line = line
    , fitnessClass = fitnessClass
    , totals =
        case totals of
            Just t ->
                t

            Nothing ->
                { width = 0, stretch = 0, shrink = 0 }
    , previous = previous
    }



-- Main loop


{-| Calculate the fitness class for this candidate active node.
-}
calculateFitnessClass ratio =
    if ratio < -0.5 then
        0

    else if ratio <= 0.5 then
        1

    else if ratio <= 1 then
        2

    else
        3


applyPenalty : Item -> Options -> Float -> Float
applyPenalty item options badness =
    case item of
        IPenalty penalty ->
            -- Positive penalty
            if penalty.p >= 0 then
                (options.demerits.line + badness) ^ 2 + penalty.p ^ 2
                -- Negative penalty but not a forced break

            else if penalty.p == -infinity then
                (options.demerits.line + badness) ^ 2 - penalty.p ^ 2
                -- All other cases

            else
                (options.demerits.line + badness) ^ 2

        _ ->
            (options.demerits.line + badness) ^ 2


getPenalty item =
    case item of
        IPenalty penalty ->
            Just penalty.p

        _ ->
            Nothing


defaultCandidates =
    Array.fromList
        [ { demerits = infinity }
        , { demerits = infinity }
        , { demerits = infinity }
        , { demerits = infinity }
        ]


mainLoop options node index nodes =
    let
        goOuter ({ remainingNodes, seenNodes } as accum) =
            case remainingNodes of
                first :: rest ->
                    outerLoop { accum | remainingNodes = rest }

                [] ->
                    accum

        goInner ({ remainingNodes, seenNodes, candidates } as accum) =
            case remainingNodes of
                first :: rest ->
                    case innerLoop node index nodes options first { accum | remainingNodes = rest } of
                        Ok done ->
                            done

                        Err newAccum ->
                            goInner newAccum

                [] ->
                    ( List.reverse seenNodes ++ remainingNodes, candidates )

        finalize = 
            let tmpSum = computeSum index

                folder candidate (fitnessClass, active) = 
                    if candidate.demerits < infinity then 
                        let newNode = breakpoint (index, candidate.demerits, candidate.ratio, candidate.active.data.line + 1, fitnessClass, tmpSum, candidate.active))
                        in
                            ()
                    else
                        ()
						
            in
                Array.foldl folder (0, activeNodes) candidates

    in
    Debug.todo "todo"


innerLoop node index nodes options current ({ remainingNodes, seenNodes, candidates } as accumulator) =
    let
        currentLine =
            current.line + 1

        ratio =
            computeCost current.position index current currentLine

        keepCurrent =
            if ratio < -1 || getPenalty node == Just -infinity then
                False

            else
                True

        -- Add flagged penalty if both nodes are flagged
        addFlaggedPenalty : Float -> Float
        addFlaggedPenalty demerits =
            case node of
                IPenalty penalty ->
                    case Array.get current.position nodes of
                        Just (IPenalty otherPenalty) ->
                            if penalty.flagged && otherPenalty.flagged then
                                demerits + options.demerits.flagged

                            else
                                demerits

                        _ ->
                            demerits

                _ ->
                    demerits

        -- Add a fitness penalty to the demerits if the fitness classes of two adjacent lines
        -- differ too much.
        addDifferencePenalty : Float -> Float -> Float
        addDifferencePenalty currentClass demerits =
            if abs (currentClass - current.fitnessClass) > 1 then
                demerits + options.demerits.fitness

            else
                demerits

        newCandidates =
            if -1 <= ratio && ratio <= options.tolerance then
                let
                    badness =
                        100 * abs ratio ^ 3

                    currentClass =
                        calculateFitnessClass ratio

                    newDemerits =
                        applyPenalty current options badness
                            |> addFlaggedPenalty
                            |> addDifferencePenalty currentClass
                            |> (\demerits -> demerits + current.demerits)
                in
                case Array.get currentClass candidates of
                    Nothing ->
                        candidates

                    Just currentClassCandidate ->
                        if newDemerits < currentClassCandidate.demerits then
                            Array.set currentClass { active = keepCurrent, demerits = newDemerits, ratio = ratio } candidates

                        else
                            candidates

            else
                candidates

        break =
            --  Stop iterating through active nodes to insert new candidate active nodes in the active list
            --  before moving on to the active nodes for the next line.
            --  TODO: The Knuth and Plass paper suggests a conditional for currentLine < j0. This means paragraphs
            --  with identical line lengths will not be sorted by line number. Find out if that is a desirable outcome.
            --  For now I left this out, as it only adds minimal overhead to the algorithm and keeping the active node
            --  list sorted has a higher priority.
            current.line >= currrentLine
    in
    if break then
        Ok <|
            ( if keepCurrent then
                List.reverse seenNodes ++ [ current ] ++ remainingNodes

              else
                List.reverse seenNodes ++ remainingNodes
            , newCandidates
            )

    else
        Err <|
            { seenNodes =
                if keepCurrent then
                    current :: seenNodes

                else
                    seenNodes
            , remainingNodes = remainingNodes
            , candidates = newCandidates
            }
