module Show exposing(showError, showTree)
import Combine exposing (ParseErr, ParseOk)
import Html exposing (Html, Attribute, text, div, p)
import Html.Attributes exposing (style)
import Array exposing (Array)
import Tree exposing (Tree(..), tree)

alm = "#d3d7cf"
btr = "#c4a000"
org = "#ce5c00"
chc = "#8f5902"
cha = "#4e9a06"
sky = "#204a87"
plm = "#5c3566"
red = "#a40000"

colors : Array String
colors = Array.fromList [ btr, org, chc, cha, sky, plm, red ]

color : Int -> (String, String)
color i = Array.get (i % (Array.length colors)) colors
    |> Maybe.withDefault alm
    |> \s -> ("color", s)

showError : ParseErr s -> Html a
showError (_, { input, position }, errs) =
    let
        remMsg = "remaining input: "  ++ input
        posMsg = "pos: " ++ toString position
        errMsgs = remMsg :: posMsg :: errs |> List.map (\s -> p [] [ text s ])
    in
        div [] errMsgs

showTree : ParseOk s (Tree String) -> Html a
showTree (_, _, tree) =
    div [ style [ ("background-color", alm)] ] (show 0 tree)

indentedText : Int -> String -> Html a
indentedText i s =
    div [ style [ ("text-indent", (toString (i * 30)) ++ "px")
                , color i
                ] ]
        [ text s ]

show : Int -> Tree String -> List (Html a)
show d tree =
    let
        indent = indentedText d
    in
    case tree of
        Val s -> [ indent s ]
        Node s ->
            s
            |> List.map (show (d + 1))
            |> List.concat
            |> between (indent "(") (indent ")")

between : a -> a -> List a -> List a
between head last list =
    [head] ++ list ++ [last]
