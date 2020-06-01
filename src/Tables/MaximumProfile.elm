module Tables.MaximumProfile exposing (MaximumProfile, decode, numberOfGlyphs)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Decode.Extra exposing (andMap)


type MaximumProfile
    = V0_5 MaximumProfileV0_5
    | V1_0 MaximumProfileV1_0


numberOfGlyphs : MaximumProfile -> Int
numberOfGlyphs profile =
    case profile of
        V0_5 { numGlyphs } ->
            numGlyphs

        V1_0 { numGlyphs } ->
            numGlyphs


type alias MaximumProfileV0_5 =
    { numGlyphs : Int
    }


type alias MaximumProfileV1_0 =
    { numGlyphs : Int
    , maxPoints : Int
    , maxContours : Int
    , maxCompositePoints : Int
    , maxCompositeContours : Int
    , maxZones : Int
    , maxTwilightPoints : Int
    , maxStorage : Int
    , maxFunctionDefs : Int
    , maxInstructionDefs : Int
    , maxStackElements : Int
    , maxSizeOfInstructions : Int
    , maxComponentElements : Int
    , maxComponentDepth : Int
    }


decode =
    let
        version0_5 =
            Decode.succeed MaximumProfileV0_5
                |> andMap (Decode.unsignedInt16 BE)

        version1_0 =
            Decode.succeed MaximumProfileV1_0
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
                |> andMap (Decode.unsignedInt16 BE)
    in
    fixed
        |> Decode.andThen
            (\version ->
                case version of
                    0x00010000 ->
                        Decode.map V1_0 version1_0

                    0x5000 ->
                        Decode.map V0_5 version0_5

                    _ ->
                        Debug.log ("fail in maxp, version is " ++ String.fromInt version) Decode.fail
            )


fixed =
    Decode.signedInt32 BE
