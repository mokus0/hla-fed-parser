module Text.FedFile.TreeUtils where

import Data.Tree
import Data.Maybe

-- a handy little path component constructor, useful in expressions like:
-- getSubTreeAtPath [className `is` "foo", className `is` "bar"]
prop `is` val = \x -> prop x == val

-- another handy path thingy.  The above example shortens to:
-- getSubTreeAtPath (className `are` ["foo", "bar"])
prop `are` vals = map (prop `is`) vals

getSubTreesAtPath :: [a -> Bool] -> Forest a -> [Tree a]
getSubTreesAtPath []     ts = []
getSubTreesAtPath [p]    ts = [t | t@(Node l _) <- ts, p l]
getSubTreesAtPath (p:ps) ts = concatMap (getSubTreesAtPath ps) [ts | t@(Node l ts) <- ts, p l]

getNodesAtPath :: [a -> Bool] -> Forest a -> [a]
getNodesAtPath p = fmap rootLabel . getSubTreesAtPath p

getNodeAtPath :: [a -> Bool] -> Forest a -> Maybe a
getNodeAtPath p = listToMaybe . getNodesAtPath p