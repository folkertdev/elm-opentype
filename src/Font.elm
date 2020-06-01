module Font exposing (Font, advanceWidths, characterMapping, compactFontFormat, glyphSubstitution, header)

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import CompactFontFormat
import Decode.Opentype exposing (Tag(..))
import Dict exposing (Dict)
import Tables.CharacterMapping
import Tables.GlyphSubstitution
import Tables.Header
import Tables.HorizontalHeader
import Tables.HorizontalMetrics
import Tables.MaximumProfile
import Tables.Naming
import Tables.OS2

type alias Font = { tables : Dict String Bytes } 



{-| Get the table with the given tag (as a `String`) and decode it with the given decoder
-}
getTable : String -> Decoder a -> Dict String Bytes -> Maybe a
getTable name decoder tables =
    case Dict.get name tables of
        Nothing ->
            Nothing

        Just buffer ->
            Decode.decode decoder buffer


{-| Get the table with the given tag (as a `String`) and decode it with the given decoder

The decoder also gets passed the `Bytes` of the table, for extra flexibility.

-}
getTableWithBytes : String -> (Bytes -> Decoder a) -> Dict String Bytes -> Maybe a
getTableWithBytes name decoder tables =
    case Dict.get name tables of
        Nothing ->
            Nothing

        Just buffer ->
            Decode.decode (decoder buffer) buffer


header : Font -> Maybe Tables.Header.Header
header font =
    getTable "head" Tables.Header.decode font.tables


maximumProfile : Font -> Maybe Tables.MaximumProfile.MaximumProfile
maximumProfile font =
    getTable "maxp" Tables.MaximumProfile.decode font.tables


characterMapping : Font -> Maybe Tables.CharacterMapping.CharacterMapping
characterMapping font =
    getTableWithBytes "cmap" Tables.CharacterMapping.decode font.tables


glyphSubstitution : Font -> Maybe Tables.GlyphSubstitution.GlyphSubstitution
glyphSubstitution font =
    getTableWithBytes "GSUB" Tables.GlyphSubstitution.decode font.tables


compactFontFormat : Font -> Maybe CompactFontFormat.CompactFontSet
compactFontFormat font =
    getTable "CFF " CompactFontFormat.decodeCompactFontSet font.tables


horizontalHeader : Font -> Maybe Tables.HorizontalHeader.HorizontalHeader
horizontalHeader font =
    getTable "hhea" Tables.HorizontalHeader.decode font.tables


horizontalMetrics : { numberOfGlyphs : Int, numberOfHMetrics : Int } -> Font -> Maybe Tables.HorizontalMetrics.HorizontalMetrics
horizontalMetrics counts font =
    getTable "hmtx" (Tables.HorizontalMetrics.decode counts) font.tables


advanceWidths : Font -> Maybe (Array Int)
advanceWidths font =
    let
        nGlyphs =
            maximumProfile font
                |> Maybe.map Tables.MaximumProfile.numberOfGlyphs

        nHMetrics =
            horizontalHeader font
                |> Maybe.map .numberOfHMetrics
    in
    Maybe.map2
        (\numberOfGlyphs numberOfHMetrics ->
            horizontalMetrics { numberOfGlyphs = numberOfGlyphs, numberOfHMetrics = numberOfHMetrics } font
                |> Maybe.map (Array.map .advanceWidth << .horizontalMetrics)
        )
        nGlyphs
        nHMetrics
        |> Maybe.andThen identity



