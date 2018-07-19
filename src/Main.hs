{-# LANGUAGE OverloadedStrings #-}

module Main where

import CMark --(commonmarkToNode, Node, NodeType)
import Data.Text (pack)

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

main :: IO ()
main = do
  md <- readFile "README.test.md"
  let md_nodes = commonmarkToNode [] (pack md)
  putStrLn (show md_nodes)
  putStrLn md
  let levels = (map (\x -> get_level (get_type x)) (filter_headings (get_children md_nodes)))
  putStrLn (show levels)
