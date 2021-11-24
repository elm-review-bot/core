module Core.Prompt exposing (Prompt, PromptType(..), show)

{-| This module gives the default styled prompts for feedback.

# Definitions
@docs Prompt, PromptType

# View
@docs show

-}


import Html exposing (Html)
import Html.Attributes as HA

{-| A prompt can be of one of the following types
1. *Success Prompt* : green in color 
1. *Danger Prompt* : red in color 
1. *Information Prompt* : blue in color 
-}


type PromptType =
    PromptSuccess
    | PromptDanger
    | PromptInfo

{-| A prompt is a message string with prompt type 
    
    -- A basic example of prompt
    Prompt ("You have successfully added the prompt module", PromptSuccess)
-}

type alias Prompt =
    (String, PromptType)

promptClass : PromptType -> String
promptClass promptType =
    case promptType of
        PromptSuccess ->
            "prompt--success"
        PromptDanger ->
            "prompt--danger"
        PromptInfo ->
            "prompt--info"

{-| Show function shows the prompt on the UI using default styles.
    -- add the following line to your view function's child elements to see a basic prompt
    show <| Prompt ("You have successfully added the prompt module", PromptSuccess)
-}

show : Prompt -> Html msg
show (prompt_text,promptType) =
    Html.div
        [ HA.class (promptClass promptType) ]
        [ Html.text prompt_text ]
