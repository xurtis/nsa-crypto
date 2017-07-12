-- Data structures
module Data exposing (..)

import Crypto exposing (..)
import Dict exposing (Dict, get)
import Keyboard
import Char
import Maybe exposing (withDefault)
import String
import Array exposing (Array)
import List exposing (map)
import Random exposing (Generator)

-- Program Flags
type alias Flags =
    -- Message to decode
    { message : String
    -- Hash of message
    , hash : String
    -- Salt for hashing the message
    , salt : String
    }

-- Model
type alias Model =
    -- Currentlu Selected character
    { selected : Char
    -- Mapping of characters to solution
    , charMap : Dict Char Char
    -- The message to decode
    , message : List String
    -- The hash to match
    , check : String
    -- Salt for hashing the message
    , salt : String
    -- The hash of the current decode
    , hash : String
    }

messageSubstitute : MaskStep -> List String -> List String
messageSubstitute step message =
    message
    |> String.join " "
    |> substitute step
    |> String.words


-- Lookup a character from a model
lookup : Model -> Char -> Char
lookup model c =
    Dict.get c model.charMap |> Maybe.withDefault c

-- Decode the message with the current substitutions
decode : Model -> String
decode model =
    model.message
    |> String.join " "
    |> String.toList
    |> map (lookup model)
    |> String.fromList

-- Initial model
init : Flags -> (Model, Cmd Msg)
init flags  =
    ( Model 
        'A' 
        initMap 
        (flags.message |> String.toUpper |> String.words) 
        flags.hash 
        flags.salt
        "" 
    , newMask GenMask
    )

-- Initial Map
initMap : Dict Char Char
initMap =
    alphas
    |> List.map (\k -> (k, '-'))
    |> Dict.fromList

-- Messages and updates
type Msg 
    = SelectChar Char
    | SetMap Char
    | Hash String
    | GenMask (Array Char, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        SelectChar c ->
            ( {model | selected = c}
            , Cmd.none
            )
        SetMap c ->
            let
                newModel =
                    { model 
                    | charMap =
                        Dict.insert model.selected c
                            model.charMap
                    }
            in
                ( newModel
                , newModel |> decode |> (++) newModel.salt |> sha256
                )
        Hash h ->
            ( { model | hash = h }
            , Cmd.none
            )
        GenMask step ->
            ( { model | message = messageSubstitute step model.message}
            , stepMask GenMask step 
            )

-- Get a hash of the current decode
decodeCheck : Model -> Cmd Msg
decodeCheck model =
    model |> decode |> sha256

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions msg =
    let
        pressMessage a = 
            let
                char = (Char.toUpper (Char.fromCode a))
            in 
                if (List.member char alphas) then
                    SetMap char
                else
                    SetMap '-'
    in
        Sub.batch
            [ Keyboard.presses pressMessage
            , Crypto.sha_result Hash
            ]

