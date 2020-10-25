module Form03 exposing (main)

import Browser
-- import Html exposing (..)
import Html exposing (Html, div, input, text)
-- import Html.Attributes exposing (..)
import Html.Attributes exposing (type_, placeholder, value, style)
import Html.Events exposing (onInput)
import String exposing (length)
import Char exposing (isUpper, isLower, isDigit)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" "" ""



-- UPDATE


type Msg
  = Name String
  | Age String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Age age ->
      { model | age = age }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "age" "Age" model.age Age
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if model.name == "" then
    div [ style "color" "red" ] [ text "Name empty!" ]
  else
  if model.age == "" then
    div [ style "color" "red" ] [ text "Age empty!" ]
  else
  if String.toInt(model.age) == Nothing then
    div [ style "color" "red" ] [ text "Age malformed!" ]
  else
  if model.password == "" then
    div [ style "color" "red" ] [ text "Password empty!" ]
  else
  if model.password /= model.passwordAgain then
    div [ style "color" "red" ] [ text "Passwords do not match!" ]
  else
  if length model.password < 8 then
    div [ style "color" "#880" ] [ text "Password too short." ]
  else
  if not (String.any isUpper model.password) then
    div [ style "color" "#880" ] [ text "Password missing upper-case chars." ]
  else
  if not (String.any isLower model.password) then
    div [ style "color" "#880" ] [ text "Password missing lower-case chars." ]
  else
  if not (String.any isDigit model.password) then
    div [ style "color" "#880" ] [ text "Password missing digit chars." ]
  else
    div [ style "color" "green" ] [ text "OK" ]
