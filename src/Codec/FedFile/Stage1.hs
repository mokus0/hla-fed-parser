{-# LANGUAGE
        ViewPatterns
  #-}
{-# OPTIONS_GHC
        -fno-warn-overlapping-patterns
  #-}
-- |Stage 1; read an s-expression and tag lists with their basic
-- "kinds" (basically, interpret each list as a fragment of a FED based
-- on the atom the list starts with).  Stage 2 will be to translate this
-- tagged structure to a more user-friendly fed file representation
module Codec.FedFile.Stage1 where

import Text.SExpr
import Control.Monad.RWS

-- |Intermediate structure used when translating from a raw SExpr
data FedList a
    = FED [FedList a]
    | FederationName String
    | FederationVersion String
    | Spaces [FedList a]
    | Space String [FedList a]
    | Dimension String
    | Objects [FedList a]
    | Class String [FedList a]
    | Attr String Delivery Order [String]
    | Interactions [FedList a]
    | Interaction String Delivery Order [String] [FedList a]
    | Parameter String
    | Unparsed [a]
    deriving (Eq, Show)

fromFED                   (FED x) = Just x
fromFED               _ = Nothing

fromFederationName        (FederationName x) = Just x
fromFederationName    _ = Nothing

fromFederationVersion     (FederationVersion x) = Just x
fromFederationVersion _ = Nothing

fromSpaces                (Spaces x) = Just x
fromSpaces            _ = Nothing

fromSpace                 (Space x y) = Just (x,y)
fromSpace             _ = Nothing

fromDimension             (Dimension x) = Just x
fromDimension         _ = Nothing

fromObjects               (Objects x) = Just x
fromObjects           _ = Nothing

fromClass                 (Class x y) = Just (x,y)
fromClass             _ = Nothing

fromAttr                  (Attr a b c d) = Just (a,b,c,d)
fromAttr              _ = Nothing

fromInteractions          (Interactions x) = Just x
fromInteractions      _ = Nothing

fromInteraction           (Interaction n d o ss xs) = Just (n,d,o,ss,xs)
fromInteraction       _ = Nothing

fromParameter             (Parameter x) = Just x
fromParameter         _ = Nothing

fromUnparsed              (Unparsed x) = Just x
fromUnparsed          _ = Nothing

instance Functor FedList where
    fmap f (FED xs)                     = FED (map (fmap f) xs)
    fmap f (FederationName n)           = FederationName n
    fmap f (FederationVersion v)        = FederationVersion v
    fmap f (Spaces xs)                  = Spaces (map (fmap f) xs)
    fmap f (Space s xs)                 = Space s (map (fmap f) xs)
    fmap f (Dimension d)                = Dimension d
    fmap f (Objects xs)                 = Objects (map (fmap f) xs)
    fmap f (Class c xs)                 = Class c (map (fmap f) xs)
    fmap f (Attr a d o ss)              = Attr a d o ss
    fmap f (Interactions is)            = Interactions (map (fmap f) is)
    fmap f (Interaction n d o ss xs)    = Interaction n d o ss (map (fmap f) xs)
    fmap f (Parameter p)                = Parameter p
    fmap f (Unparsed u)                 = Unparsed (fmap f u)

data Delivery = Reliable | BestEffort
    deriving (Eq, Ord, Bounded, Enum)
-- to allow "show" output to be used as literal values:
reliable = Reliable
best_effort = BestEffort

data Order = Receive | Timestamp
    deriving (Eq, Ord, Bounded, Enum)
-- to allow "show" output to be used as literal values:
receive = Receive
timestamp = Timestamp

instance Read Delivery where
    readsPrec _ (splitAt 8  -> ("reliable",    rest)) = [(reliable,    rest)]
    readsPrec _ (splitAt 11 -> ("best_effort", rest)) = [(best_effort, rest)]
    readsPrec _ other = []

instance Show Delivery where
    showsPrec _ Reliable    = showString "reliable"
    showsPrec _ BestEffort  = showString "best_effort"

instance Read Order where
    readsPrec _ (splitAt 7 -> ("receive",   rest)) = [(receive,   rest)]
    readsPrec _ (splitAt 9 -> ("timestamp", rest)) = [(timestamp, rest)]
    readsPrec _ other = []

instance Show Order where
    showsPrec _ Receive   = showString "receive"
    showsPrec _ Timestamp = showString "timestamp"

breakMaybe :: (a -> Maybe b) -> (a -> Maybe c) -> [a] -> Maybe ([b],[c])
breakMaybe p q = start id
    where
        start ps ((p -> Just x):xs) = start (ps . (x:)) xs
        start ps xs = end ps id xs
        
        end ps qs [] = Just (ps [], qs [])
        end ps qs ((q -> Just x) : xs) = end ps (qs . (x:)) xs
        end ps qs _ = Nothing

listToFedList :: [SExpr FedList String] -> FedList (SExpr FedList String)
listToFedList ((fromAtom -> Just "FED") : (mapM fromList -> Just rest))     = FED rest
listToFedList (mapM fromAtom -> Just ["Federation", name])                  = FederationName name
listToFedList (mapM fromAtom -> Just ["FEDversion", vers])                  = FederationVersion vers
listToFedList ( (fromAtom -> Just "spaces")
              : (mapM fromList -> Just rest))                               = Spaces rest
listToFedList ( (fromAtom -> Just "space")
              : (fromAtom -> Just name)
              : (mapM fromList -> Just dims))                               = Space name dims
listToFedList (mapM fromAtom -> Just ["dimension", name])                   = Dimension name
listToFedList ( (fromAtom -> Just "objects") 
              : (mapM fromList -> Just rest))                               = Objects rest
listToFedList ( (fromAtom -> Just "class")
              : (fromAtom -> Just name)
              : (mapM fromList -> Just rest))                               = Class name rest
listToFedList (mapM fromAtom -> Just ("attribute":name:mode:ord:rest))      = Attr name (read mode) (read ord) rest
listToFedList ( (fromAtom -> Just "interactions")
              : (mapM fromList -> Just rest))                               = Interactions rest
listToFedList ( (fromAtom -> Just "class")
              : (fromAtom -> Just name)
              : (fromAtom -> Just mode)
              : (fromAtom -> Just ord)
              : (breakMaybe fromAtom fromList -> Just (spaces, rest))    
              )                                                             = Interaction name (read mode) (read ord) spaces rest
listToFedList (mapM fromAtom -> Just ["parameter", name])                   = Parameter name
listToFedList x                                                             = Unparsed x

fedListToList :: FedList (SExpr [] String) -> [SExpr [] String]
fedListToList (FED xs)                  = (atom "FED" : map (list . fedListToList) xs)
fedListToList (FederationName n)        = [atom "Federation", atom n]
fedListToList (FederationVersion v)     = [atom "FEDversion", atom v]
fedListToList (Spaces xs)               = (atom "spaces": map (list . fedListToList) xs)
fedListToList (Space s xs)              = (atom "space": atom s : map (list . fedListToList) xs)
fedListToList (Dimension d)             = [atom "dimension", atom d]
fedListToList (Objects xs)              = (atom "objects": map (list . fedListToList) xs)
fedListToList (Class c xs)              = (atom "class" : atom c : map (list . fedListToList) xs)
fedListToList (Attr a d o ss)           = (atom "attribute" : atom a : atom (show d) : atom (show o) : map atom ss)
fedListToList (Interactions is)         = (atom "interactions" : map (list . fedListToList) is)
fedListToList (Interaction n d o ss xs) = (atom "class" : atom (show d) : atom (show o) : map atom ss ++ map (list . fedListToList) xs)
fedListToList (Parameter p)             = [atom "parameter", atom p]
fedListToList (Unparsed u)              = u


readFedSExpr :: String -> SExpr FedList String
readFedSExpr = lmap listToFedList . readSExprString

readFedSExprFromFile :: FilePath -> IO (SExpr FedList String)
readFedSExprFromFile file = fmap readFedSExpr (readFile file)

showFedSExpr :: SExpr FedList String -> String
showFedSExpr = advancedString . lmap fedListToList

writeFedSExprToFile :: FilePath -> SExpr FedList String -> IO ()
writeFedSExprToFile f = writeFile f . showFedSExpr

testStage1 = readFedSExprFromFile "fom_v4.1_16JUNE2008.fed"