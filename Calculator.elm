module Calculator where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (..)
import Maybe exposing (..)
import Json.Encode exposing (string)
import Json.Decode as Json
import Signal exposing (Signal, Address)
import String exposing (toFloat)

-- MODEL

type alias Stack = List Float

type alias Model =
    { stack : Stack
    , field : String
    }


init : Model
init =
    { stack = []
    , field = ""
    }


-- UPDATE

type Action
    = Clear
    | Pop
    | Push
    | UpdateField String
    | Add
    | Subtract


update : Action -> Model -> Model
update action model =
  case action of
    Clear -> init

    UpdateField str -> 
      { model | field <- str }
      

    Push -> 
      { model |
          stack <- pushTextToStack model.field model.stack ,
          field <- ""
      }

    Pop ->
      { model | stack <- withDefault [] (List.tail model.stack) }

    Add -> 
      { model | 
          stack <- op2 (+) model.field model.stack ,
          field <- ""
      }

    Subtract ->
      { model | 
          stack <- op2 (-) model.field model.stack ,
          field <- ""
      }

pushTextToStack : String -> Stack -> Stack
pushTextToStack text stack =
  case text |> String.toFloat of
    Ok number -> number :: stack
    Err _ -> stack

op2 : (Float -> Float -> Float) -> String -> Stack -> Stack
op2 f text stack =
  case (String.toFloat text, stack) of
    (Ok number, val :: rest) -> (f val number) :: rest

    (Err _, val1 :: val2 :: rest) -> (f val2 val1) :: rest

    _ -> stack

-- VIEW

view : Address Action -> Model -> Html
view address model =
  div [class "calculator"]
    [ stackView model
    , input [id "number_entry"
            , name "number"
            , value model.field
            , type' "number"
            , property "inputmode" (string "numeric")
            , placeholder "number"
            , autofocus True
            , on "input" targetValue (Signal.message address << UpdateField)
            , onEnter address Push] [ ]
    , button [ onClick address Clear ] [ text "Clear" ]
    , button [ onClick address Add ] [ text "+"] 
    , button [ onClick address Subtract ] [ text "-"] 
    , button [ onClick address Pop ] [ text "Del"]     
    ]

stackView : Model -> Html
stackView model =
  div [] (model.stack |> take 5 |> reverse |> List.map stackEntryView )

onEnter: Address a -> a -> Attribute
onEnter address value =
  on "keydown"
    (Json.customDecoder keyCode is13)
    (\_ -> Signal.message address value)

is13: Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

stackEntryView : Float -> Html
stackEntryView a =
  div [class "stack"] [a |> toString |> text]
