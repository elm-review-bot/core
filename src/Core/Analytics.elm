module Core.Analytics exposing (generateJson,sendOutLog)

{-| This module exposes the function which sends the messages and the new models.

@docs generateJson, sendOutLog
-}

import Json.Encode as JE
import Core.Analytics.Encoder as AE


-- port analytics : JE.Value -> Cmd msg

{-| The function which takes model and action and encodes it into JSON

**NOTE** : This function uses Debug.toString and may prevent you from compiling using --optimize flag
-}

generateJson : action -> model -> JE.Value
generateJson msg model =
    JE.object 
        [ ( "action", AE.encodeAction (Debug.toString msg) )
        , ( "model", AE.encodeModel (Debug.toString model) )
        ]


{-| The command that takes the model, action and send it over the port "analytics"
-}

sendOutLog : action -> model -> (JE.Value -> Cmd msg )-> Cmd msg
sendOutLog msg model analyticsPort=
    (generateJson msg model |> analyticsPort)


