module Tables.GlyphSubstitution exposing (GlyphSubstitution, decode)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder)
import Decode.Extra exposing (andMap)
import Decode.Opentype exposing (Tag, int16, offset16, offset32, tag, uint16)


type alias GlyphSubstitution =
    { header : Header
    , scriptList : ScriptList
    , featureList : FeatureList
    , lookupList : Array Int

    --, singleSubstitution : SingleSubstitution
    }


decode bytes =
    decodeHeader
        |> Decode.andThen
            (\header ->
                let
                    -- scripts
                    scriptList =
                        Decode.Extra.dropLeft header.scriptListOffset bytes
                            |> Maybe.andThen (Decode.decode decodeScriptList)
                            |> Debug.log "script list"

                    scriptTable { offset } =
                        Decode.Extra.dropLeft (header.scriptListOffset + offset) bytes
                            |> Maybe.andThen (Decode.decode decodeScriptTable)

                    scriptTables =
                        Maybe.map (Array.map scriptTable) scriptList
                            |> Debug.log "scripttables"

                    -- features
                    featureList =
                        Decode.Extra.dropLeft header.featureListOffset bytes
                            |> Maybe.andThen (Decode.decode decodeFeatureList)

                    featureTable : { a | offset : Int } -> Maybe (Array Int)
                    featureTable { offset } =
                        Decode.Extra.dropLeft (header.featureListOffset + offset) bytes
                            |> Maybe.andThen (Decode.decode decodeFeatureTable)

                    featureTables =
                        Maybe.map (Array.map featureTable) featureList
                            |> Debug.log "feature tables"

                    -- lookup
                    lookupList : Maybe (Array Int)
                    lookupList =
                        Decode.Extra.dropLeft header.lookupListOffset bytes
                            |> Maybe.andThen (Decode.decode decodeLookupList)

                    lookupTable : Int -> Maybe LookupTable
                    lookupTable offset =
                        Decode.Extra.dropLeft (header.lookupListOffset + offset) bytes
                            |> Maybe.andThen (Decode.decode decodeLookupTable)

                    lookupTables =
                        Maybe.map (Array.map lookupTable) lookupList
                            |> Debug.log "lookup tables"
                in
                Maybe.map3 (GlyphSubstitution header) scriptList featureList lookupList
                    |> Decode.Extra.fromMaybe
            )



-- |> andMap decodeSingleSubstitution


type alias Header =
    { version : Decode.Opentype.Version
    , scriptListOffset : Int
    , featureListOffset : Int
    , lookupListOffset : Int
    , featureVariationOffset : Maybe Int
    }


decodeHeader : Decoder Header
decodeHeader =
    Decode.Opentype.version
        |> Decode.andThen
            (\version ->
                Decode.succeed (Header version)
                    |> andMap offset16
                    |> andMap offset16
                    |> andMap offset16
                    |> andMap
                        (if version.minor == 1 then
                            Decode.map Just offset32

                         else
                            Decode.succeed Nothing
                        )
            )


type SingleSubstitution
    = SingleSubstiution1 { coverageOffset : Int, deltaGlyphID : Int }
    | SingleSubstiution2 { coverageOffset : Int, glyphCount : Int, subsituteGlyphIDs : Array Int }


decodeSingleSubstitution =
    uint16
        |> Decode.andThen
            (\format ->
                case format of
                    1 ->
                        Decode.map2 (\coverageOffset deltaGlyphID -> SingleSubstiution1 { coverageOffset = coverageOffset, deltaGlyphID = deltaGlyphID })
                            offset16
                            int16

                    _ ->
                        Decode.fail
            )



-- Tag with Offset


type alias TagWithOffset =
    { tag : Tag, offset : Int }


decodeTagWithOffset =
    Decode.succeed TagWithOffset
        |> andMap tag
        |> andMap offset16



-- Script List


type alias ScriptList =
    Array TagWithOffset


decodeScriptList =
    Decode.Extra.sizedArray uint16 decodeTagWithOffset



-- Script Table


type alias ScriptTable =
    { defaultLangSys : Int, langSysRecords : Array TagWithOffset }


decodeScriptTable =
    Decode.succeed ScriptTable
        |> andMap offset16
        |> andMap (Decode.Extra.sizedArray uint16 decodeTagWithOffset)



-- Feature List


type alias FeatureList =
    Array TagWithOffset


decodeFeatureList =
    Decode.Extra.sizedArray uint16 decodeTagWithOffset



-- Feature table


decodeFeatureTable : Decoder (Array Int)
decodeFeatureTable =
    Decode.succeed (\_ k -> k)
        |> andMap offset16
        |> andMap (Decode.Extra.sizedArray uint16 uint16)



-- Lookup List


decodeLookupList =
    Decode.Extra.sizedArray uint16 offset16



-- Lookup table


type alias LookupTable =
    { lookupType : Int, lookupFlag : Int, subtableOffsets : Array Int, markFilteringSet : Maybe Int }


decodeLookupTable : Decoder LookupTable
decodeLookupTable =
    Decode.map2
        (\lookupType lookupFlag ->
            let
                useMarkFilteringSet =
                    Bitwise.and 0x10 lookupFlag == 1
            in
            case useMarkFilteringSet of
                True ->
                    Decode.map2 (\subtableOffsets markFilteringSet -> LookupTable lookupType lookupFlag subtableOffsets (Just markFilteringSet))
                        (Decode.Extra.sizedArray uint16 offset16)
                        uint16

                False ->
                    Decode.map (\subtableOffsets -> LookupTable lookupType lookupFlag subtableOffsets Nothing)
                        (Decode.Extra.sizedArray uint16 offset16)
        )
        uint16
        uint16
        |> Decode.andThen identity
