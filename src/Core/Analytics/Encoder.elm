module Core.Analytics.Encoder exposing (..)

{-| This module exposes the default JSON encoder for analytics telemetry

# Encoders
@docs encodeAction, encodeModel


-}

import Json.Encode as JE
import Core.Analytics.Parser as CAP exposing (Thing(..))
import Parser as P exposing (run)

{-| encodeAction takes a string and encodes it to JSON of the following format

    -- for example the user gives a custom type DefInit as input
    encodeAction <| Debug.toString DefInit 
    -- { "type": "DefInit", "payload":"null" }

**NOTE** : only custom types can be encoded, others will result in an error JSON
    -- for example if I try to encode Int 5 
    encodeAction <| Debug.toString 5
    -- it will result in
    -- { "type": "ActionTypeError", "payload": "Please change Msg type to Custom Data type in Elm app" }

-}

encodeAction : String -> JE.Value
encodeAction actionString =
    let
        parsedAction = run CAP.parse actionString 
    in
        case parsedAction of
            Ok action ->
                case action of    
                    Custom name [] ->
                        JE.object 
                            [ ( "type", JE.string name )
                            , ( "payload", JE.null)
                            ]
                    
                    Custom name [val] ->
                        JE.object 
                            [ ( "type", JE.string name )
                            , ( "payload", CAP.encodeFlat val )
                            ]

                    Custom name args ->
                        JE.object
                            [ ( "type", JE.string name )
                            , ( "payload", args |> JE.list CAP.encodeFlat)
                            ]

                    _ ->
                        JE.object
                            [ ( "type", JE.string "ActionTypeError")
                            , ( "payload", JE.string "Please change Msg type to Custom Data type in Elm app")
                            ]
            Err lst ->
                JE.object
                    [ ( "type", JE.string "ActionParseError")
                    , ( "payload", JE.string (P.deadEndsToString lst))
                    ]

{-| encodeModel uses Analytics.Parser's encodeFlat function

**NOTE** : If an error is encountered during parsing following JSON is returned
    --  { "type": "ModelParseError", "value": "ModelParseError" <RawString> }
-}

encodeModel : String -> JE.Value
encodeModel modelString =
    let 
        parsedModel = run CAP.parse modelString
    in
        case parsedModel of
            Ok model ->
                CAP.encodeFlat model
            Err lst ->
                JE.object
                    [ ( "type", JE.string "ModelParseError")
                    , ( "value", JE.string (P.deadEndsToString (Debug.log "ModelParseError" lst)))
                    ]

