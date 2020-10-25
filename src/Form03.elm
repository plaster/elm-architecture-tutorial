module Form03 exposing (main)

import Browser
-- import Html exposing (..)
import Html exposing (Html, ul, li, div, input, text)
-- import Html.Attributes exposing (..)
import Html.Attributes exposing (type_, placeholder, value, style)
import Html.Events exposing (onInput)
import String exposing (length)
import Char exposing (isUpper, isLower, isDigit)
import List exposing (map)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , age : String
  , password : String
  , passwordAgain : String
  , lastModel : LastModel
  }

type LastModel = LastModel (Maybe Model)


init : Model
init =
  Model "" "" "" "" (LastModel Nothing)



-- UPDATE


type Msg
  = Name String
  | Age String
  | Password String
  | PasswordAgain String
  | Submit Model


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

    Submit lastModel ->
      { model | lastModel = LastModel (Just lastModel) }



-- VIEW


view : Model -> Html Msg
view model =
  ul []
    (
    [ viewInput "text" "Name" model.name Name
    , viewInput "age" "Age" model.age Age
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    ] ++
    viewValidation model
    )


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  li []
    [ input [ type_ t, placeholder p, value v, onInput toMsg ] []
    ]

validationE : Model -> List String
validationE model =
  []
  ++ (if model.name == ""
  then [ "Name empty!" ]
  else [])
  ++ (if model.age == ""
  then [ "Age empty!" ]
  else if String.toInt(model.age) == Nothing
  then [ "Age malformed!" ]
  else [])
  ++ (if model.password == ""
  then [ "Password empty!" ]
  else if model.passwordAgain == ""
  then [ "Please re-enter password!" ]
  else if model.password /= model.passwordAgain
  then [ "Passwords do not match!" ]
  else [])

validationW : Model -> List String
validationW model =
  []
  ++ (if length model.password < 8
  then [ "Password too short." ]
  else [])
  ++ (if not (String.any isUpper model.password)
  then [ "Password missing upper-case chars." ]
  else [])
  ++ (if not (String.any isLower model.password)
  then [ "Password missing lower-case chars." ]
  else [])
  ++ (if not (String.any isDigit model.password)
  then [ "Password missing digit chars." ]
  else [])

viewValidation : Model -> List (Html msg)
viewValidation model =
  case validationE model of
    [] ->
      case validationW model of
      [] ->
        [ viewOk "OK" ]
      warnings -> map viewW warnings
    errors -> map viewE errors

viewW : String -> Html msg
viewW warning =
  li [ style "color" "#880" ] [ text warning ]

viewE : String -> Html msg
viewE warning =
  li [ style "color" "red" ] [ text warning ]

viewOk : String -> Html msg
viewOk ok =
  li [ style "color" "green" ] [ text ok ]
