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
    | Duplicate
    | Swap
    | UpdateField String
    | Add
    | Subtract
    | Multiply
    | Divide


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

    Duplicate -> 
      { model | stack <- withDefault model.stack (List.head model.stack |> Maybe.map (pushToStack model.stack)) } 

    Swap ->
      { model | stack <- swap model.stack }

    Add -> op2 (+) model

    Subtract -> op2 (-) model

    Multiply -> op2 (*) model

    Divide -> op2 (/) model

pushTextToStack : String -> Stack -> Stack
pushTextToStack text stack =
  case text |> String.toFloat of
    Ok number -> number :: stack
    Err _     -> stack

pushToStack : Stack -> Float -> Stack
pushToStack stack number =
  number :: stack

swap : Stack -> Stack
swap stack =
  case stack of
    val1 :: val2 :: rest -> val2 :: val1 :: rest
    _                    -> stack

op2 : (Float -> Float -> Float) -> Model -> Model
op2 f model =
  let stack = model.stack
      field = model.field
      maybeNumber = String.toFloat field
      newStack =
        case (maybeNumber, stack) of
          (Ok number, val :: rest)      -> (f val number) :: rest
          (Err _, val1 :: val2 :: rest) -> (f val2 val1) :: rest
          _                             -> stack
      newField =
        case maybeNumber of
          Ok _  -> ""
          Err _ -> field
  in
    { model | 
      stack <- newStack
    , field <- newField
    }

-- VIEW

view : Address Action -> Model -> Html
view address model =
  div [class "calculator"]
      [ stackView model
      , controls address model
      ]

stackView : Model -> Html
stackView model =
  div [class "stack"] (model.stack |> take 5 |> reverse |> List.map stackEntryView )

controls : Address Action -> Model -> Html
controls address model =
  div [ class "controls"]
      [ numberEntry address model
      , buttons address model
      ]

numberEntry : Address Action -> Model -> Html
numberEntry address model =
  div [ class "entry"]
      [ input [ id "number_entry"
              , name "number"
              , value model.field
              , type' "number"
              , property "inputmode" (string "numeric")
              , placeholder "number"
              , autofocus True
              , on "input" targetValue (Signal.message address << UpdateField)
              , onEnter address Push] [ ]
      ]

buttons : Address Action -> Model -> Html
buttons address model = 
  div [ class "buttons"]
      [ div [ id "buttons_row1"] 
            [ button [ onClick address Clear ] [ text "Clr" ]
            , button [ onClick address Pop ] [ text "Del"]
            , button [ onClick address Duplicate ] [ text "Dup"]
            , button [ onClick address Swap ] [ text "Swp"]
            ]
      , div [ id "buttons_row2"] 
            [ button [ onClick address Add ] [ text "+"] 
            , button [ onClick address Subtract ] [ text "-"] 
            , button [ onClick address Multiply ] [ text "*"] 
            , button [ onClick address Divide ] [ text "/"] 
           ]
      ]

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
  div [ class "stack_number"] [a |> toString |> text]
