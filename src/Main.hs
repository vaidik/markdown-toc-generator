{-# LANGUAGE OverloadedStrings #-}

module Main where

import CMark
import Data.Text (pack, unpack)
import Data.Text.Internal (Text)
import Data.Maybe (fromJust)

getType :: Node -> NodeType
getType (Node _ t _) = t

getChildren :: Node -> [Node]
getChildren (Node _ _ n) = n

isHeading :: NodeType -> Bool
isHeading (HEADING _) = True
isHeading _ = False

getLevel :: NodeType -> Int
getLevel (HEADING l) = l
getLevel nt = error ("NodeType " ++ show nt ++ " is not HEADING type")

filterHeadings :: [Node] -> [Node]
filterHeadings = filter $ isHeading . getType

getText :: Node -> Text
getText (Node _ (TEXT s) _) = s

makeNode :: Node -> Maybe Node -> Node
makeNode (Node _ t c) Nothing = Node Nothing ITEM [Node Nothing PARAGRAPH [Node Nothing (TEXT (getText (head c))) []]]
makeNode (Node _ t c) n = Node Nothing ITEM [Node Nothing PARAGRAPH [Node Nothing (TEXT (getText (head c))) []], fromJust n]

slice :: [Node] -> Int -> [Node]
slice [] l = []
slice [x] l = [x | getLevel (getType x) > l]
slice (x:xs) l
  | level_x > l   = x : slice xs l
  | level_x <= l  = []
  where level_x = getLevel (getType x)

reverseSlice :: [Node] -> Int -> [Node]
reverseSlice x l = drop (length (slice x l)) x

makeList' :: [Node] -> [Node]
makeList' [] = []
makeList' [x] = [makeNode x Nothing]
makeList' (x:y:xs)
  | level_x == level_y  = makeNode x Nothing : makeList' (y:xs)
  | level_x < level_y   = makeNode x (Just (makeListNode (makeList' (slice (y : xs) level_x)))) : makeList' (reverseSlice (y : xs) level_x)
  | level_x > level_y   = error "Unexpected condition"
    where level_x = getLevel (getType x)
          level_y = getLevel (getType y)

makeListNode :: [Node] -> Node
makeListNode = Node Nothing (LIST (ListAttributes {listType = ORDERED_LIST, listTight = True, listStart = 1, listDelim = PERIOD_DELIM}))

main :: IO ()
main = do
  md <- readFile "README.test.md"
  let mdNodes = getChildren (commonmarkToNode [] (pack md))
  putStrLn (unpack (nodeToCommonmark [] Nothing (makeListNode (makeList' (filterHeadings mdNodes)))))
