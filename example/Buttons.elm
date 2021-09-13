port module Buttons exposing (..)

import Browser 
-- import Random exposing (int)
import Html exposing (Html,button, div,text)
import Html.Attributes as HA
import Html.Events exposing(onClick)
import Core as ULE
-- import Random as R
-- import Graph as G
import Dict exposing (Dict)
import Json.Encode as JE

port analytics : JE.Value -> Cmd msg

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model = 
    {
        value : Int
    ,   msg : Maybe Msg
    ,   test : List Int
    ,   testTuple : (Int,Int)
    ,   testBool : Bool
    ,   testChar : Char
    ,   testString : String
    ,   testResultOK : Result String String
    ,   testResultErr : Result String String
    -- ,   testGraph : G.Graph Int ()
    ,   testDict : Dict String Int
    }

setFresh : Msg -> Bool
setFresh msg =
    case msg of 
        Init _ ->
            True
        _ ->
            False



init : () -> (Model,Cmd Msg)
init _ = 
    (   {
            value = 0
        ,   msg = Nothing
        ,   test = [1,2,3,4]
        ,   testTuple = (5,7)
        ,   testBool = True
        ,   testChar = 'a'
        ,   testString = "He'l'lo"
        ,   testResultOK = Ok "Hello"
        ,   testResultErr = Err "World"
        -- ,   testGraph = G.fromNodeLabelsAndEdgePairs [1,2,3] [(0,1), (0,2)]
        ,   testDict = Dict.fromList [("a",65), ("b",66), ("c",67)]
        }
    , Cmd.none )

type Msg = 
    Increment Int 
    | Decrement
    | Init Int
    | SomeMessage String Int
    | SomeMessage2 (String,Int,Int)




update : Msg -> Model -> (Model,Cmd Msg)
update msg model = 
    case msg of 
        Increment _ ->
            ({ model | value = model.value + 1, msg = Just msg}, Cmd.none )
        Decrement ->
            ({ model | value = model.value - 1, msg = Just msg}, Cmd.none )
        Init i ->
            ({ model | value = i, msg = Just msg}, Cmd.none )
        _ ->
            ( {model | msg = Just msg}, Cmd.none)

view: Model -> Html Msg
view model =
  div [ HA.class "experiment-container" ]
    [ div 
        [HA.class "feedback-container"] 
        [ Html.div [HA.class "prompt--success"] [text (String.fromInt model.value)]
        , Html.div [HA.class "prompt--danger"] [text (String.fromInt model.value)] 
        , Html.div [HA.class "prompt--info"] [text (String.fromInt model.value)] 
        ]
    , div 
        [ HA.class "observables-container"]
        [ Html.h1 [ HA.style "font-size" "-webkit-xxx-large"] [text (String.fromInt model.value)] 
        -- , Html.div [] [text "sometext"]
        -- , Html.div [] [text "sometext"]
        -- , Html.div [] [text "sometext"]
        ]
    , div
        [ HA.class "controls-container"]
        [ button [ onClick Decrement, HA.class "button__action--primary" ] [ text "-" ]
        , button [ onClick (Increment 5), HA.class "button__action--primary" ] [ text "+" ]
        -- , button [ onClick (SomeMessage "TEST1" 7), HA.class "button__action--primary" ] [text "TESTER"]
        -- , button [ onClick (SomeMessage2 ("TEST1",7,8)), HA.class "button__action--primary" ] [text "TESTER"]
        ]
    ]


main = 
    Browser.element
    { init = ULE.init logger analytics init
    , view = ULE.view view
    , update = ULE.update logger analytics update setFresh Nothing Nothing
    , subscriptions = ULE.subscriptions subscriptions
    }

logger model =
    { value = 0
    , msg = model.msg
    , test = model.test
    , testTuple = model.testTuple
    , testBool = model.testBool
    , testChar = model.testChar
    , testString = model.testString
    , testResultOK = model.testResultOK
    , testResultErr = model.testResultErr
    -- , testGraph = model.testGraph
    , testDict = model.testDict
    -- , testNodes = G.nodes model.testGraph |> List.map (\n -> n.label)
    -- , testEdges = G.edges model.testGraph

    }