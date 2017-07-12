-- Data structures
module Data exposing (..)

import Sha exposing (..)
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

-- Alphabetical characters
alphas = String.toList (String.toUpper "abcdefghijklmnopqrstuvwxyz")
numAlphas = List.length alphas

swapLetters : Array Char -> Int -> Int -> Array Char
swapLetters chars a b =
    let
        letterA = Array.get a chars |> Maybe.withDefault '?'
        letterB = Array.get b chars |> Maybe.withDefault '?'
    in
        chars |> Array.set a letterB |> Array.set b letterA

-- String shuffling generator
genMask : Array Char -> Int -> Generator (Array Char, Int)
genMask mask next = 
    let 
        alphaInt = Random.int 0 (numAlphas - 1)
        fromInt i =
            ( swapLetters mask next i
            , next + 1
            )
    in
        Random.map fromInt alphaInt 

-- Apply a substitution on the message
substitute : List Char -> String -> String
substitute mask text =
    let
        zip a b =
            case (List.head a) of
                Nothing -> []
                Just a_head ->
                    case (List.head b) of
                        Nothing -> []
                        Just b_head -> 
                            (a_head, b_head) :: zip 
                                (Maybe.withDefault [] (List.tail a))
                                (Maybe.withDefault [] (List.tail b))
        maskDict = zip alphas mask |> Dict.fromList
        subChar c = Dict.get c maskDict |> Maybe.withDefault c
    in
        map subChar (String.toList text) |> String.fromList

messageSubstitute : Array Char -> List String -> List String
messageSubstitute mask message =
    message
    |> String.join " "
    |> substitute (Array.toList mask)
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
    , Random.generate GenMask (genMask (Array.fromList alphas) 0)
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
        GenMask (mask, next) ->
            if (next >= numAlphas) then
                ({ model | message = messageSubstitute mask model.message}, Cmd.none)
            else 
                (model, Random.generate GenMask (genMask mask next))

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
            , Sha.sha_result Hash
            ]

