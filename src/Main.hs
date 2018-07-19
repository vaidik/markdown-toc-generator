{-# LANGUAGE OverloadedStrings #-}

module Main where

import CMark --(commonmarkToNode, Node, NodeType)
import Data.Text (pack, unpack)
import Data.Text.Internal (Text)

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

-- slice :: [Int] -> Int -> [Int]
-- slice [] _ = []
-- slice [x] l = if x > l then x else []
-- slice (x:xs) = if x > l then x:(slice xs l) else 

restructure_nodes :: [Int] -> Int -> [Int]
restructure_nodes [] _ = []
restructure_nodes [x] l = if x > l then [x] else []
restructure_nodes (x:xs) l
  | x > l = x:(restructure_nodes xs x)
  | x <= l = restructure_nodes xs l

get_text :: Node -> Text
get_text (Node _ (TEXT s) _) = s

make_item :: Node -> Node
make_item (Node _ t c) = Node Nothing ITEM [Node Nothing PARAGRAPH [Node Nothing (TEXT (get_text (c !! 0))) []]]

main :: IO ()
main = do
  md <- readFile "README.test.md"
  result <- readFile "README.result.md"
  let md_nodes = commonmarkToNode [] (pack md)
  putStrLn (show md_nodes)
  putStrLn md
  putStrLn "\n"
  putStrLn (show (get_children (commonmarkToNode [] (pack result))))
  let levels = (map (\x -> get_level (get_type x)) (filter_headings (get_children md_nodes)))
  putStrLn (show levels)
  putStrLn (show (restructure_nodes levels 1))
  putStrLn (show (restructure_nodes levels 2))

  putStrLn (show (make_item ((get_children md_nodes) !! 0)))
