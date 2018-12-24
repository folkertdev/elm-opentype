module Render exposing (convert, convertHelp)

import Charstring.Internal as Charstring exposing (Charstring, Operation(..))
import LowLevel.Command as LowLevel exposing (DrawTo, MoveTo)
import SubPath exposing (SubPath)


asFloats ( x, y ) =
    ( toFloat x, toFloat y )


convert : List Operation -> List SubPath
convert operations =
    case operations of
        (MoveTo point) :: rest ->
            convertHelp rest (LowLevel.MoveTo (asFloats ( point.x, point.y ))) [] []

        _ :: rest ->
            convert rest

        [] ->
            Debug.log "operation skipped in convert" []



{-


   M205,69 C309,98 251,69 205,69 L355,154 C336,434 368,413 398,362 C118,314 205,434 299,434 C151,69 118,108 118,187

-}


convertHelp : List Operation -> MoveTo -> List DrawTo -> List SubPath -> List SubPath
convertHelp operations moveto drawtos subpaths =
    case operations of
        [] ->
            let
                last =
                    SubPath.with moveto (List.reverse (LowLevel.ClosePath :: drawtos))
            in
            List.reverse (last :: subpaths)

        first :: rest ->
            case first of
                Many list ->
                    convertHelp (list ++ rest) moveto drawtos subpaths

                MoveTo point ->
                    convertHelp rest (LowLevel.MoveTo (asFloats ( point.x, point.y ))) [] (SubPath.with moveto (List.reverse drawtos) :: subpaths)

                CurveTo p1 p2 p3 ->
                    let
                        argument =
                            ( asFloats ( p1.x, p1.y ), asFloats ( p2.x, p2.y ), asFloats ( p3.x, p3.y ) )
                    in
                    convertHelp rest moveto (LowLevel.CurveTo [ argument ] :: drawtos) subpaths

                LineTo p ->
                    convertHelp rest moveto (LowLevel.LineTo [ asFloats ( p.x, p.y ) ] :: drawtos) subpaths

                CounterMask _ ->
                    convertHelp rest moveto drawtos subpaths

                HintMask _ ->
                    convertHelp rest moveto drawtos subpaths

                HStem y dy ->
                    convertHelp rest moveto drawtos subpaths

                VStem x dx ->
                    convertHelp rest moveto drawtos subpaths

                Width w ->
                    convertHelp rest moveto drawtos subpaths

                ClosePath ->
                    -- convertHelp rest moveto (LowLevel.ClosePath :: drawtos) subpaths
                    convertHelp rest moveto drawtos subpaths

                NoOp ->
                    convertHelp rest moveto drawtos subpaths
