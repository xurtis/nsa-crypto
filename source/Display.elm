-- Layout and Display
module Display exposing (..)

import Data exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick)
import List exposing (map)
import String
import Dict
import Maybe exposing (withDefault)

-- View
view : Model -> Html Msg
view model =
    div []
        [ titleBar "Security Engineering NSA App"
        , div [ class "container" ]
            [ successPanel model
            , messagePanel model
            , mappingPanel model
            -- , statusPanel model
            ]
        ]


-- Title Bar
titleBar : String -> Html Msg
titleBar title = 
    nav [ class "navbar", class "navbar-default" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ span [ class "navbar-brand" ]
                    [ text title ]
                ]
            ]
        ]

-- Seccess panel
successPanel : Model -> Html Msg
successPanel model = 
    if (model.hash == model.check) then 
        div [ class "alert", class "alert-success" ]
            [ Html.p [] 
                [ Html.b [] [text "Successfully decoded message." ]
                ]
            , Html.p [ class "decode-text" ] [ decode model |> text ]
            ]
    else
        div [ class "alert", class "alert-warning" ]
            [ Html.p [] 
                [ Html.b [] [text "Message not yet decoded." ]
                ]
            ]
    

-- Bootstrap panel
panel : String -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
panel title attr content = 
    div ([ class "panel", class "panel-default" ] ++ attr)
        [ div [ class "panel-heading" ] [ text title ]
        , div [ class "panel-body"] content
        ]

-- Message Panel
messagePanel : Model -> Html Msg
messagePanel model =
    panel "Message" [ class "message" ]
        (List.map (wordDisplay model) model.message)


-- Display a single word
wordDisplay : Model -> String -> Html Msg
wordDisplay model word =
    div [ class "word" ]
        (List.map (byChar model letterDisplay) (String.toList word))

-- Display a single letter
letterDisplay : (Char -> String) -> (Char -> List (Attribute Msg)) -> Char -> Html Msg
letterDisplay lookup selected c =
    div ([ class "letter", onClick (SelectChar c) ] ++ selected c)
        [ div [ class "mapped" ] [ text (lookup c) ]
        , div [ class "ciphered" ] [ text (String.fromChar c) ]
        ]

-- Mapping Panel
mappingPanel : Model -> Html Msg
mappingPanel model =
    panel "Solution" [ class "mapping" ]
        (List.map (byChar model charMapping) (Dict.keys model.charMap))

-- Single character mapping
charMapping : (Char -> String) -> (Char -> List (Attribute Msg)) -> Char -> Html Msg
charMapping lookup selected c =
    div ([ class "col-sm-2", onClick (SelectChar c) ] ++ selected c)
        [ div [ class "input-group" ]
            [ span [ class "input-group-btn"]
                [ button 
                    [ class "btn", class "btn-default", type_ "button"] 
                    [ text (String.fromChar c) ]
                ]
            , input [ type_ "text", class "form-control", value (lookup c) ] []
            ]
        ]

-- Apply char info to functions
byChar : Model -> ((Char -> String) -> (Char -> List (Attribute Msg)) -> a) -> a
byChar model function =
    function (charLookup model) (charSelected model)

-- Look up a character from the model
charLookup : Model -> Char -> String
charLookup model c =
    lookup model c |> String.fromChar

-- Get a selected attribute if given char is selected
charSelected : Model -> Char -> List (Attribute Msg)
charSelected model c = 
    if (model.selected == c) then [ class "selected" ] else []

-- Show status of hash and salt
statusPanel : Model -> Html Msg
statusPanel model = 
    panel "Status" [] 
        [ Html.h3 [] [ text "Current Hash" ]
        , Html.p [ class "decode-text" ] [ text model.hash ]
        , Html.h3 [] [ text "Salt" ]
        , Html.p [ class "decode-text" ] [ text model.salt ]
        ]
