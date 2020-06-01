module TextLayout exposing (width)

{-| Module docs
-}


type alias SizeInformation =
    { lineHeight : Int, characterWidth : Char -> Int }


{-| Width of a string in font space
-}
width : String -> Int
width string =
    0
