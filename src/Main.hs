{-# LANGUAGE OverloadedStrings #-}

module Main where

import CMark
import Data.Text (pack, unpack)
import Data.Text.Internal (Text)
import Data.Maybe (fromJust)

get_type :: Node -> NodeType
get_type (Node _ t _) = t

get_children :: Node -> [Node]
get_children (Node _ _ n) = n

is_heading :: NodeType -> Bool
is_heading (HEADING _) = True
is_heading _ = False

get_level :: NodeType -> Int
get_level (HEADING l) = l
get_level nt = error ("NodeType " ++ (show nt) ++ " is not HEADING type")

filter_headings :: [Node] -> [Node]
filter_headings n = filter (\x -> (is_heading (get_type x)) == True) n

restructure_nodes :: [Int] -> Int -> [Int]
restructure_nodes [] _ = []
restructure_nodes [x] l = if x > l then [x] else []
restructure_nodes (x:xs) l
  | x > l = x:(restructure_nodes xs x)
  | x <= l = restructure_nodes xs l

get_text :: Node -> Text
get_text (Node _ (TEXT s) _) = s

make_list :: [Node] -> Node
make_list n = Node Nothing (LIST (ListAttributes {listType = ORDERED_LIST, listTight = True, listStart = 1, listDelim = PERIOD_DELIM})) (map (\x -> make_node x Nothing) n)

make_node :: Node -> Maybe Node -> Node
make_node (Node _ t c) Nothing = Node Nothing ITEM [Node Nothing PARAGRAPH [Node Nothing (TEXT (get_text (c !! 0))) []]]
make_node (Node _ t c) n = Node Nothing ITEM [Node Nothing PARAGRAPH [Node Nothing (TEXT (get_text (c !! 0))) []], (fromJust n)]

slice :: [Node] -> Int -> [Node]
slice [] l = []
slice [x] l = if (get_level (get_type x)) > l then [x] else []
slice (x:xs) l
  | level_x > l   = x:(slice xs l)
  | level_x <= l  = []
  where level_x = get_level (get_type x)

reverse_slice :: [Node] -> Int -> [Node]
reverse_slice x l = drop (length (slice x l)) x

make_list' :: [Node] -> [Node]
make_list' [] = []
make_list' [x] = [make_node x Nothing]
make_list' (x:y:xs)
  | level_x == level_y  = (make_node x Nothing):(make_list' (y:xs))
  | level_x < level_y   = (make_node x (Just (make_list_node (make_list' (slice (y:xs) level_x))))):(make_list' (reverse_slice (y:xs) level_x))
  | level_x > level_y   = error "Unexpected condition"
    where level_x = get_level (get_type x)
          level_y = get_level (get_type y)

make_list_node :: [Node] -> Node
make_list_node n = Node Nothing (LIST (ListAttributes {listType = ORDERED_LIST, listTight = True, listStart = 1, listDelim = PERIOD_DELIM})) n

main :: IO ()
main = do
  md <- readFile "README.test.md"
  let md_nodes = get_children (commonmarkToNode [] (pack md))
  let levels = (map (\x -> get_level (get_type x)) (filter_headings md_nodes))
  putStrLn (unpack (nodeToCommonmark [] Nothing (make_list_node (make_list' (filter_headings md_nodes)))))
