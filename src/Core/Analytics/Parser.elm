module Core.Analytics.Parser exposing
    ( toJson
    , parse
    , Thing(..)
    , encode
    , encodeFlat
    )

{-| This library is inspired from [f0i/DebugToJson](https://package.elm-lang.org/packages/f0i/debug-to-json/1.0.6)
Convert Debug.toString output to JSON

# Definition
@docs Thing

# Parser
@docs parse

# Convert to JSON
@docs toJson

# Ecoders
@docs encode, encodeFlat


-}

import Json.Encode as E
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , chompIf
        , end
        , float
        , getChompedString
        , int
        , lazy
        , loop
        , map
        , oneOf
        , run
        , spaces
        , succeed
        , symbol
        , variable
        )
import Set

{-| The thing is an internal data structure to which the elm data types are parsed from the string
This is exposed so that user can write custom JSON encoder specific to his usecase
-}

type Thing
    = Obj (List ( String, Thing ))
    | Dct (List ( Thing, Thing ))
    | Arr (List Thing)
    | Set (List Thing)
    | Str String
    | MayBe (Maybe Thing) 
    | Reslt (Result Thing Thing) 
    | Custom String (List Thing)
    | Lst (List Thing)
    | Tpl (List Thing)
    | NumInt Int
    | NumFloat Float
    | Boolean Bool
    | Chr String
    | Fun
    | Intern
    | UnitType




-- JSON


{-| Convert output from Debug.toString to JSON
-}
toJson : String -> Result (List Parser.DeadEnd) E.Value
toJson val =
    val |> run parse |> Result.map encode



-- Typed JSON Encoder

{-| Typed JSON encoder for Thing (parsed Debug.toString) output
-}

encode : Thing -> E.Value
encode thing =
    case thing of
        Obj kvs ->
            E.object
                [ ( "type", E.string "Record" )
                , ( "value", List.map (\( k, v ) -> ( k, encode v )) kvs |> E.object )
                ]
        Dct kvs ->
            E.object
                [ ( "type", E.string "Dict" )
                , ( "value", 
                        List.map
                            (\( k, v ) ->
                                ( case k of
                                    Str s ->
                                        s

                                    _ ->
                                        E.encode 0 (encode k)
                                , encode v
                                )
                            ) kvs
                        |> E.object
                    )
                ]

        Arr vals ->
            E.object 
                [ ( "type", E.string "Array" )
                , ( "value", E.list encode vals )
                ]
            

        Set vals ->
            E.object 
                [ ( "type", E.string "Set" )
                , ( "value", E.list encode vals )
                ]

        Str s ->
            E.string s

        Custom name [] ->
            E.object 
                [ ( "type", E.string "Custom" )
                , ( "name", E.string name)
                , ( "value", E.null )
                ]

        Custom name [val] ->
            E.object 
                [ ( "type", E.string "Custom" )
                , ( "name", E.string name)
                , ( "value", encode val )
                ]

        Custom name args ->
            E.object 
                [ ( "type", E.string "Custom" )
                , ( "name", E.string name)
                , ( "value", E.list encode args )
                ]

        Lst vals ->
            E.object 
                [ ( "type", E.string "List" )
                , ( "value", E.list encode vals )
                ]

        Tpl vals ->
            E.object 
                [ ( "type", E.string "Tuple" )
                , ( "value", E.list encode vals )
                ]

        NumInt n ->
            E.int n

        NumFloat n ->
            E.float n

        Fun ->
            E.object 
                [ ( "type", E.string "<function>" )
                , ( "value", E.null )
                ]

        Intern ->
            E.object 
                [ ( "type", E.string "<internals>" )
                , ( "value", E.null )
                ]

        MayBe val ->
            case val of 
                Nothing ->
                    E.object 
                        [ ( "type", E.string "Maybe" )
                        , ( "name", E.string "Nothing")
                        , ( "value", E.null )
                        ]
                Just v ->
                    E.object 
                        [ ( "type", E.string "Maybe" )
                        , ( "name", E.string "Just")
                        , ( "value", encode v )
                        ]

        Boolean val ->
            E.bool val

        Chr c ->
            E.object 
                [ ( "type", E.string "Char" )
                , ( "value", E.string c )
                ]

        Reslt (Ok val) ->
            E.object 
                [ ( "type", E.string "Result" )
                , ( "name", E.string "Ok")
                , ( "value", encode val )
                ]
        Reslt (Err val) ->
            E.object 
                [ ( "type", E.string "Result" )
                , ( "name", E.string "Err")
                , ( "value", encode val )
                ]
        UnitType ->
            E.object 
                [ ( "type", E.string "Unit" )
                , ( "value", E.null )
                ]


-- Flat JSON Encoder

{-| Converts Elm Debug string to Flat JSON 
(does not include type information in the encoded data)
-}

encodeFlat : Thing -> E.Value
encodeFlat thing =
    case thing of
        Obj kvs ->
            List.map (\( k, v ) -> ( k, encodeFlat v )) kvs |> E.object 

        Dct kvs ->
            List.map
                (\( k, v ) ->
                    ( case k of
                        Str s ->
                            s

                        _ ->
                            E.encode 0 (encodeFlat k)
                    , encodeFlat v
                    )
                ) kvs
            |> E.object


        Arr vals ->
            E.list encodeFlat vals
            

        Set vals ->
            E.list encodeFlat vals 

        Str s ->
            E.string s

        Custom name [] ->
            E.object 
                [ ( "name", E.string name)
                , ( "value", E.null )
                ]

        Custom name [val] ->
            E.object 
                [ ( "name", E.string name)
                , ( "value", encodeFlat val )
                ]

        Custom name args ->
            E.object 
                [ ( "name", E.string name)
                , ( "value", E.list encodeFlat args )
                ]

        Lst vals ->
            E.list encodeFlat vals 

        Tpl vals ->
            E.list encodeFlat vals

        NumInt n ->
            E.int n

        NumFloat n ->
            E.float n

        Fun ->
            E.string "<function>" 

        Intern ->
            E.string "<internals>" 

        MayBe val ->
            case val of 
                Nothing ->
                    E.object 
                        [ ( "name", E.string "Nothing")
                        , ( "value", E.null )
                        ]
                Just v ->
                    E.object 
                        [ ( "name", E.string "Just")
                        , ( "value", encodeFlat v )
                        ]

        Boolean val ->
            E.bool val

        Chr c ->
            E.string c

        Reslt (Ok val) ->
            E.object 
                [ ( "name", E.string "Ok")
                , ( "value", encodeFlat val )
                ]
        Reslt (Err val) ->
            E.object 
                [ ( "name", E.string "Err")
                , ( "value", encodeFlat val )
                ]
        UnitType ->
            E.string "<unit>"


-- PARSER
{-| this parser only parses core data types, 
Package specific data types built using Custom types (like IntDict) may not be easily human readable

-}

parse : Parser Thing
parse =
    succeed identity
        |= parseThing
        |. end


parseThing : Parser Thing
parseThing =
    succeed identity
        |= oneOf
            [ parseDct
            , parseArr
            , parseSet
            , parseObj
            , parseString
            , parseLst
            , parseTpl
            , parseMaybe
            , parseResult
            , parseBool
            , parseCustom
            , parseNumberFloat
            , parseNumberInt
            , parseChar
            , parseFun
            , parseIntern
            ]
        |. spaces
        |. oneOf [ symbol ",", symbol "" ]
        |. spaces


parseObj : Parser Thing
parseObj =
    succeed Obj
        |. spaces
        |. symbol "{"
        |. spaces
        |= list parseKeyValue
        |. spaces
        |. symbol "}"
        |. spaces


parseDct : Parser Thing
parseDct =
    succeed Dct
        |. spaces
        |. symbol "Dict.fromList ["
        |. spaces
        |= list parseDictKeyValue
        |. spaces
        |. symbol "]"
        |. spaces


parseArr : Parser Thing
parseArr =
    succeed Arr
        |. spaces
        |. symbol "Array.fromList ["
        |. spaces
        |= lazy (\_ -> list parseThing)
        |. spaces
        |. symbol "]"
        |. spaces


parseSet : Parser Thing
parseSet =
    succeed Set
        |. spaces
        |. symbol "Set.fromList ["
        |. spaces
        |= lazy (\_ -> list parseThing)
        |. spaces
        |. symbol "]"
        |. spaces

parseMaybe : Parser Thing
parseMaybe =
    succeed MayBe
        |= oneOf 
            [ parseNothing
            , parseJust
            ]


parseNothing : Parser (Maybe Thing)
parseNothing = 
    succeed Nothing
        |. spaces
        |. symbol "Nothing"
        |. spaces

parseJust : Parser (Maybe Thing)
parseJust =
    succeed Just
        |. spaces
        |. symbol "Just"
        |. spaces
        |= oneOf 
            [ succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ ->  parseThing)
                |. spaces
                |. symbol ")" 
            , succeed identity
                |= lazy (\_ ->  parseThing)
            ]
        |. spaces


parseResult : Parser Thing
parseResult =
    succeed Reslt 
        |= oneOf
            [ parseOk
            , parseErr
            ]

parseOk : Parser (Result Thing Thing)
parseOk =
    succeed Ok
        |. spaces
        |. symbol "Ok"
        |. spaces
        |= oneOf 
            [ succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ ->  parseThing)
                |. spaces
                |. symbol ")" 
            , succeed identity
                |= lazy (\_ ->  parseThing)
            ]
        |. spaces


parseErr : Parser (Result Thing Thing)
parseErr =
    succeed Err
        |. spaces
        |. symbol "Err"
        |. spaces
        |= oneOf 
            [ succeed identity
                |. symbol "("
                |. spaces
                |= lazy (\_ ->  parseThing)
                |. spaces
                |. symbol ")" 
            , succeed identity
                |= lazy (\_ ->  parseThing)
            ]
        |. spaces

parseBool : Parser Thing
parseBool =
    succeed Boolean
        |. spaces
        |= oneOf
            [ map (\_ -> True) (symbol "True")
            , map (\_ -> False) (symbol "False")
            ]
        |. spaces


parseCustom : Parser Thing
parseCustom =
    succeed Custom
        |. spaces
        |= upperVar
        |. spaces
        |= lazy (\_ -> list parseThing)
        |. spaces


parseLst : Parser Thing
parseLst =
    succeed Lst
        |. spaces
        |. symbol "["
        |. spaces
        |= lazy (\_ -> list parseThing)
        |. symbol "]"
        |. spaces


parseTpl : Parser Thing
parseTpl =
    succeed identity
        |. spaces
        |. symbol "("
        |. spaces
        |= map (\lst -> case lst of
                            [] ->
                                (UnitType)
                            [val] ->
                                ( identity val)
                            _ ->
                                 (Tpl lst)) 
                (lazy (\_ -> list parseThing))
        |. symbol ")"
        |. spaces


parseNumberFloat =
    succeed NumFloat
        |= oneOf
            [ succeed negate
                |. symbol "-"
                |= float
            , float
            ]


parseNumberInt =
    succeed NumInt
        |= oneOf
            [ succeed negate
                |. symbol "-"
                |= int
            , int
            ]


parseFun =
    succeed Fun
        |. symbol "<function>"


parseIntern =
    succeed Intern
        |. symbol "<internals>"


upperVar =
    variable { start = Char.isUpper, inner = \c -> Char.isAlphaNum c || c == '.' || c == '_', reserved = Set.empty }


lowerVar =
    variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '.' || c == '_', reserved = Set.empty }


list : Parser a -> Parser (List a)
list parser =
    loop [] (listHelp parser)


listHelp : Parser a -> List a -> Parser (Step (List a) (List a))
listHelp parser acc =
    oneOf
        [ succeed (\val -> Loop (val :: acc))
            |= parser
        , succeed ()
            |> map (\_ -> Done (List.reverse acc))
        ]


parseKeyValue : Parser ( String, Thing )
parseKeyValue =
    succeed Tuple.pair
        |. spaces
        |= lowerVar
        |. spaces
        |. symbol "="
        |. spaces
        |= lazy (\_ -> parseThing)
        |. spaces
        |. oneOf [ symbol ",", symbol "" ]
        |. spaces


parseDictKeyValue : Parser ( Thing, Thing )
parseDictKeyValue =
    succeed Tuple.pair
        |. spaces
        |. symbol "("
        |. spaces
        |= lazy (\_ -> parseThing)
        |. spaces
        |= lazy (\_ -> parseThing)
        |. spaces
        |. symbol ")"
        |. spaces
        |. oneOf [ symbol ",", symbol "" ]
        |. spaces


notQuote : Parser String
notQuote =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> c /= '"')


escapedChar : Parser String
escapedChar =
    succeed
        (\c ->
            case c of
                "t" ->
                    "\t"

                "n" ->
                    "\n"

                "\"" ->
                    "\""

                "\\" ->
                    "\\"

                _ ->
                    "\\" ++ c
        )
        |= getChompedString
            (succeed ()
                |. symbol "\\"
                |. chompIf (\_ -> True)
            )


repeat : Parser String -> Parser String
repeat parser =
    loop "" (repeatHelp parser)


repeatHelp : Parser String -> String -> Parser (Step String String)
repeatHelp parser acc =
    oneOf
        [ succeed (\val -> Loop (acc ++ val))
            |= parser
        , succeed ()
            |> map (\_ -> Done acc)
        ]


parseString : Parser Thing
parseString =
    succeed Str
        |. symbol "\""
        |= repeat (oneOf [ escapedChar, notQuote ])
        |. symbol "\""

parseChar : Parser Thing
parseChar =
    succeed identity
        |. spaces
        |. symbol "'"
        |. Parser.chompWhile (\c -> c /= '\'')
        |. symbol "'"
        |. spaces
        |> getChompedString
        |> Parser.andThen (\s -> if String.length s == 3 then (succeed (Chr (String.slice 1 2 s))) else Parser.problem s)