port module Crypto exposing (..)

import Random exposing (Generator)
import Array exposing (Array)
import List exposing (map)
import Dict
import String
import Bitwise exposing (and, shiftRightBy)

-- Hashing

-- Port for sending out data to be hashed
port sha256 : String -> Cmd msg

-- Port for listening to hashed messages
port sha_result : (String -> msg) -> Sub msg

-- Monoalphabetic Substitution
type alias MaskStep = (Array Char, Int)

-- Alphabetical characters
alphas = String.toList (String.toUpper "abcdefghijklmnopqrstuvwxyz")
numAlphas = List.length alphas

-- Letter shuffling
swapLetters : Array Char -> Int -> Int -> Array Char
swapLetters chars a b =
    let
        letterA = Array.get a chars |> Maybe.withDefault '?'
        letterB = Array.get b chars |> Maybe.withDefault '?'
    in
        chars |> Array.set a letterB |> Array.set b letterA

-- String shuffling generator
genMask : MaskStep -> Generator MaskStep
genMask step = 
    let 
        (mask, next) = step
        alphaInt = Random.int 0 (numAlphas - 1)
        fromInt i =
            ( swapLetters mask next i
            , next + 1
            )
    in
        Random.map fromInt alphaInt 

-- Continue to next step of mask generation
stepMask : (MaskStep -> msg) -> MaskStep -> Cmd msg
stepMask message step =
    let
        (_, count) = step
    in
        if (count >= numAlphas) then
            Cmd.none
        else
            Random.generate message (genMask step)

-- Start generating a mash
newMask : (MaskStep -> msg) -> Cmd msg
newMask message = 
    stepMask message (Array.fromList alphas, 0)

-- Apply a substitution mask on the message
substitute : MaskStep -> String -> String
substitute step text =
    let
        (mask, _) = step
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
        maskDict = mask |> Array.toList |> zip alphas |> Dict.fromList
        subChar c = Dict.get c maskDict |> Maybe.withDefault c
    in
        map subChar (String.toList text) |> String.fromList

-- Encode an int as hex
hexchars : Array Char
hexchars = 
    "0123456789abcdef"
    |> String.toList
    |> Array.fromList

hexEncode : Int -> String
hexEncode value =
    let
        tail = and value 15
        char = Array.get tail hexchars |> Maybe.withDefault '0'
        init = shiftRightBy 4 value
    in
        if (value /= 0) then
            char |> String.fromChar |> (++) (hexEncode init)
        else
            ""

-- Generate a salt
genSalt : (String -> msg) -> Cmd msg
genSalt message =
    let
        saltGenerator =
            Random.map hexEncode (Random.int Random.minInt Random.maxInt)
    in
        Random.generate message saltGenerator
