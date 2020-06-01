module FontDimensions exposing (Dimensions, dimensions)

import Array exposing (Array)
import Dict exposing (Dict)
import Font exposing (Font)
import Tables.CharacterMapping exposing (CharacterMapping)


type alias Dimensions =
    { lineHeight : Int, unitsPerEm : Int, defaultWidth : Int, characterWidths : Dict Char Int }


dimensions : Font -> Maybe Dimensions
dimensions font =
    let
        cmap =
            Font.characterMapping font

        header =
            Font.header font

        -- subs = Maybe.andThen Font.glyphSubstitution font
        advanceWidths =
            Font.advanceWidths font
                |> Maybe.withDefault Array.empty
    in
    Maybe.map2 (dimensionsHelper advanceWidths) cmap header


dimensionsHelper advanceWidths cmap header =
    let
        { characterWidths, defaultWidth } =
            getCharacterWidths cmap advanceWidths

        unitsPerEm =
            header.unitsPerEm

        lineHeight =
            header.boundingBox.yMax - header.boundingBox.yMin
    in
    { characterWidths = characterWidths
    , defaultWidth = defaultWidth
    , unitsPerEm = unitsPerEm
    , lineHeight = lineHeight
    }


getCharacterWidth : { defaultWidth : Int, characterWidths : Dict Char Int } -> Char -> Int
getCharacterWidth { defaultWidth, characterWidths } char =
    Dict.get char characterWidths
        |> Maybe.withDefault defaultWidth


getCharacterWidths : CharacterMapping -> Array Int -> { defaultWidth : Int, characterWidths : Dict Char Int }
getCharacterWidths cmap advanceWidths =
    let
        glyphIndex character =
            Tables.CharacterMapping.getGlyphIndex cmap 0 character

        widthForCharacter character =
            Array.get (glyphIndex character) advanceWidths
                |> Maybe.withDefault 0

        defaultWidth =
            widthForCharacter '\u{0000}'

        folder index accum =
            let
                character : Char
                character =
                    Char.fromCode index

                width =
                    widthForCharacter character
            in
            if width == 0 || width == defaultWidth then
                accum

            else
                Dict.insert character width accum
    in
    { defaultWidth = defaultWidth
    , characterWidths = List.foldl folder Dict.empty (List.range 0 (65536 - 1))
    }
