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

generateJson : Maybe model -> action -> model -> JE.Value
generateJson preModel_ msg postModel =
    let
        (actionType,payload) = AE.encodeAction (Debug.toString msg)
    in
    case preModel_ of
        Just preModel ->
            JE.object 
                [ actionType 
                , payload
                , ( "preState", AE.encodeModel (Debug.toString preModel) )
                , ( "postState", AE.encodeModel (Debug.toString postModel))
                ]
        Nothing ->
            JE.object 
                [ actionType
                , payload
                , ( "preState", JE.null )
                , ( "postState", AE.encodeModel (Debug.toString postModel))
                ]

{-| The command that takes the model, action and send it over the port "analytics"
-}

sendOutLog : Maybe model -> action -> model -> (JE.Value -> Cmd msg )-> Cmd msg
sendOutLog preModel msg postModel analyticsPort=
    (generateJson preModel msg postModel |> analyticsPort)


