{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import CMark
import Data.Text (pack, strip, toLower, unpack)
import Data.Text.Internal (Text)
import Data.Maybe (fromJust, isJust, maybeToList)
import System.Environment (getArgs)
import System.Console.Docopt
import System.IO
import System.IO.Strict as S
import Text.Regex

tocHeadingText :: String
tocHeadingText = "Table of Contents"

tocStartComment :: String
tocStartComment = "<!-- TOC -->"

tocEndComment :: String
tocEndComment = "<!-- TOC END -->"

tocSplitRegex :: Regex
tocSplitRegex = mkRegex (tocStartComment ++ "(\n|.)+" ++ tocEndComment ++ "\n\n")

getType :: Node -> NodeType
getType (Node _ t _) = t

getChildren :: Node -> [Node]
getChildren (Node _ _ n) = n

isHeading :: Node -> Bool
isHeading t@(Node _ (HEADING _) _) = unpack (getText t) /= tocHeadingText
isHeading _ = False

getLevel :: NodeType -> Int
getLevel (HEADING l) = l
getLevel nt = error ("NodeType " ++ show nt ++ " is not HEADING type")

filterHeadings :: [Node] -> [Node]
filterHeadings = filter isHeading

getText :: Node -> Text
getText (Node _ (TEXT s) _) = s
getText (Node _ (HEADING _) [x]) = getText x
getText _ = error "Unexpected node. Works only with HEADING or TEXT nodes."

makeTextNode :: Text -> Node
makeTextNode s = Node Nothing (TEXT s) []

removePunc :: String -> String
removePunc = filter (not . (`elem` (",.?!-:;\"\'" :: String)))

replaceChar :: Char -> Char -> String -> String
replaceChar o n xs = [ if x == o then n else x | x <- xs ]

makeLinkNode :: Text -> Node
makeLinkNode t = Node Nothing (LINK u t)
    [ makeTextNode t ]
  where u = pack ('#' : (replaceChar ' ' '-' . removePunc) (unpack (toLower . strip $ t)))

makeNode :: Node -> Maybe Node -> Node
makeNode (Node _ _ c) n = Node Nothing ITEM
    (Node Nothing PARAGRAPH [ makeLinkNode t ] : maybeToList n)
  where t = getText (head c)

slice :: [Node] -> Int -> [Node]
slice [] l = []
slice [x] l = [x | getLevel (getType x) > l]
slice (x:xs) l
  | level_x > l   = x : slice xs l
  | level_x <= l  = []
  where level_x = getLevel (getType x)

reverseSlice :: [Node] -> Int -> [Node]
reverseSlice x l = drop (length (slice x l)) x

makeList' :: Bool -> [Node] -> [Node]
makeList' _ [] = []
makeList' _ [x] = [makeNode x Nothing]
makeList' useBullets (x:y:xs)
  | level_x == level_y  = makeNode x Nothing : makeList' useBullets (y : xs)
  | level_x < level_y   = makeNode x nestedList
                            : makeList' useBullets (reverseSlice (y : xs) level_x)
  | level_x > level_y   = error ("Unexpected heading: Heading "
                            ++ "'" ++ unpack (getText y) ++ "' "
                            ++ "is perhaps incorrectly used in the current "
                            ++ "heading tree")
    where level_x     = getLevel (getType x)
          level_y     = getLevel (getType y)
          nestedList  = Just (makeListNode useBullets (makeList' useBullets (slice (y : xs) level_x)))

makeListNode :: Bool -> [Node] -> Node
makeListNode isUnordered = Node Nothing listAttributes
  where listType = if isUnordered then BULLET_LIST else ORDERED_LIST
        listAttributes = LIST (ListAttributes
          { listType = listType
          , listTight = True
          , listStart = 1
          , listDelim = PERIOD_DELIM })

addTOCHeading :: [Node] -> [Node]
addTOCHeading c = Node Nothing (HEADING 2) [
  Node Nothing (TEXT (pack tocHeadingText)) [] ] : c

-- TODO: use the string as it is and do not escape characters
appendMD :: String -> [Node] -> [Node]
appendMD md c = c ++ getChildren (commonmarkToNode [] (pack md))

prepWritableNode :: [Node] -> [Node] -> [Node]
prepWritableNode on nn = if getType (head on) == HEADING 1
                           then [head on]
                                  ++ getPreTOCNodes (tail on)
                                  ++ [ Node Nothing (HTML_BLOCK (pack tocStartComment)) [] ]
                                  ++ nn
                                  ++ [ Node Nothing (HTML_BLOCK (pack tocEndComment)) [] ]
                                  ++ getPostTOCNodes (tail on)
                           else nn ++ on

getPreTOCNodes :: [Node] -> [Node]
getPreTOCNodes [] = []
getPreTOCNodes [x@(Node _ (HEADING 1) _)] = [x]
getPreTOCNodes [_] = []
getPreTOCNodes (x@(Node _ tx _):xs)
  | tx == HEADING 2 = []
  | otherwise       = x : getPreTOCNodes xs

getPostTOCNodes n = drop (length $ getPreTOCNodes n) n

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  -- CLI options
  fileName <- args `getArgOrExit` argument "file"
  let withHeading = args `isPresent` longOption "with-heading"
  let append = getArg args (longOption "append")
  let noFirstH1 = args `isPresent` longOption "no-first-h1"
  let useBullets = args `isPresent` longOption "bullets"
  let writeToFile = args `isPresent` longOption "write"

  rawMD <- S.readFile fileName

  let sections = splitRegex tocSplitRegex rawMD
  let md = unwords sections

  let children = getChildren $ commonmarkToNode [] (pack md)
  let headingNodes = filterHeadings children

  let filteredHeadingNodes = if noFirstH1
                               then tail headingNodes
                               else headingNodes

  let listNode = makeListNode useBullets (makeList' useBullets filteredHeadingNodes)

  let prepChildren = (if writeToFile then prepWritableNode children else id)
                       . (if withHeading then addTOCHeading else id)
                       . maybe id appendMD append

  let documentNode = Node Nothing DOCUMENT (prepChildren [listNode])
  let newMD = unpack (nodeToCommonmark [] Nothing documentNode)

  if writeToFile
    then writeFile fileName newMD
    else putStrLn newMD
