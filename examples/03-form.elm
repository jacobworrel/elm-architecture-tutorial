import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String exposing (..)
import Char exposing (isUpper)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : String
  , ready : Bool
  }


init : Model
init =
  Model "" "" "" "" False



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Validate


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = age }

    Validate ->
      { model | ready = True }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Age" model.age Age
    , button [ onClick Validate ] [text "Submit"]
    , viewValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation { password, passwordAgain, ready } =
  let
    (color, message) =
      if ready then
        if String.length password < 8 then
          ("red", "Password must be at least 8 characters long.")
        else if String.any isUpper password == False then
          ("red", "Password must contain upper case letter.")
        else if password /= passwordAgain then
          ("red", "Passwords do not match!")
        else
          ("green", "OK")
      else
        ("", "") -- renders empty div
  in
    div [ style "color" color ] [ text message ]