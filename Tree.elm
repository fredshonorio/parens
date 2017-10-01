module Tree exposing (Tree(..), tree)
import Combine exposing (..)

type Tree a
    = Val a
    | Node (List (Tree a))


-- the quoted string allows parens inside
quotedStr : Parser s String
quotedStr = fail "nyi"

separator : Parser s String
separator = whitespace

anything : Parser s String
anything = regex "[^\\s^\\(^\\)]+"

value : Parser s String
value = String.concat <$> many (quotedStr <|> anything)

val : Parser s (Tree String)
val = Val <$> anything

tree : Parser s (Tree String)
tree =
    let
        tree_ () = val <|> tree
        trees = Node <$> sepBy separator (lazy tree_)
    in
        parens (whitespace *> trees <* whitespace)
