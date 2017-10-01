module Main exposing (main)

import Combine exposing (parse)
import Html exposing (Html, div, input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onInput)
import Tree exposing (Tree(..), tree)
import Show exposing (showError, showTree)

{-
TODO:
collapse trees whose children are only leaves and sum of text len < 50
allow brackets? config?
-}

type alias Model = String

initial : Model
initial = "(a (b c))"

update : Msg -> Model -> Model
update msg model =
  case msg of
    Expr newExpr -> newExpr

type Msg
  = Expr String

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = initial, view = view, update = update }

collapse : (e -> a) -> (r -> a) -> (Result e r) -> a
collapse fe fr res =
    case res of
        Err e -> fe e
        Ok r -> fr r

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder initial, onInput Expr ] []
    , show model
    ]

show : String -> Html a
show expr =
    expr
      |> parse tree
      |> collapse showError showTree
