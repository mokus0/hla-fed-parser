{-# LANGUAGE
        ViewPatterns, FlexibleContexts, RecordWildCards
  #-}
{-# OPTIONS_GHC
        -fno-warn-overlapping-patterns
  #-}
-- |Stage 2; extract fed data a tagged s-expr-like structure into a 
-- user-friendly format.  Essentially, consists of collapsing a list
-- of cotuples to a tuple of lists, somewhat like Data.Either.partitionEithers.
module Codec.FedFile.Stage2 where

import Text.SExpr
import Codec.FedFile.Stage1
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error ({- instance Error e => Monad (Either e) -})
import Data.List
import Data.Maybe
import Text.Printf
import qualified Data.Map as M

data Fed = Fed
    { fedName           :: String
    , fedVersion        :: String
    , fedSpaces         :: M.Map String [String]
    , fedClasses        :: M.Map String [FedClass]
    , fedInteractions   :: M.Map String [FedInteraction]
    } deriving (Eq, Show)

data FedClass = FedClass
    { className         :: String
    , classParents      :: [String]
    , classAttributes   :: M.Map String Attribute
    } deriving (Eq, Show)

data FedInteraction = FedInteraction
    { interactionName   :: String
    , interactionParents:: [String]
    , interactionSpaces :: [String]
    , interactionParams :: [String]
    } deriving (Eq, Show)

data Attribute = Attribute
    { attrName          :: String
    , attrDelivery      :: Delivery
    , attrOrder         :: Order
    , attrSpaces        :: [String]
    } deriving (Eq, Show)

limit e n 
    | n < 0     = error "limit: negative length"
    | otherwise = go n
    where
        go _        []  = []
        go 0         _  = e
        go (n+1) (x:xs) = x : go n xs

limitStr = limit "..."

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe p xs = case partition (isJust . p) xs of
    (y, n) -> (map (fromJust . p) y, n)

extract :: MonadState [s] m => (s -> Maybe a) -> m [a]
extract p = do
    (accept, reject) <- liftM (partitionMaybe p) get
    put reject
    return accept

-- Unpack a list of items that must all pass the given coercion.  On failure,
-- uses the (very short) string parameter and the "Show s" instance to produce
-- a failure message.
assertOnly  :: (Show s, Monad m) => String -> (s -> Maybe a) -> [s] -> m [a]
assertOnly cxt p xs = flip evalStateT xs $ do
    xs <- extract p
    assertEmpty cxt
    return xs

-- Assert that the local state contains no items.  On failure,
-- uses the (very short) string parameter and the "Show s" instance to produce
-- a failure message.
assertEmpty :: (Show s, MonadState [s] m) => String -> m ()
assertEmpty cxt = do
    rest <- get
    when (not (null rest)) $ fail $
        printf "\"%s\" element has %d unparsed items: \n%s" 
            cxt (length rest) (unlines (map (('\t' :) . limitStr 200 . show) rest))

fedListToFed :: Monad m => [FedList (SExpr FedList String)] -> m Fed
fedListToFed fedList = flip evalStateT fedList $ do
    -- A fairly ad-hoc monad.  Essentially a state monad with "fail" mapped to
    -- "error" and a convenience function "extract" which matches part of the
    -- state and removes the matched portion.
    --
    -- The conversion progresses by pulling things out of the state as it
    -- constructs the Fed record.  At the end, if the state is not empty
    -- (indicating unrecognized data in the s-expression) it will fail with
    -- (hopefully) a useful message.
    -- 
    -- Sub-lists are processed by the same basic process using local state monads.
    -- The most common operations on sublists are handled by the 'assertOnly'
    -- and 'assertEmpty' functions.
    
    names <- extract fromFederationName
    let nNames = length names
        name = head names
    when (nNames /= 1) $ fail (printf "Fed has %d Federation elements" nNames)
    
    versions <- extract fromFederationVersion
    let nVersions = length versions
        version = head versions
    when (nVersions /= 1) $ fail (printf "Fed has %d Federation elements" nVersions)
    
    -- extract "spaces" element
    spaces <- extract fromSpaces
    -- extract "space" elements from "spaces" element
    spaces <- assertOnly "spaces" fromSpace (concat spaces)
    -- extract "dimension" elements inside each "space" element
    spaces <- sequence
        [ do
            dims <- assertOnly ("space " ++ space) fromDimension dims
            return (space, dims)
        | (space, dims) <- spaces
        ]
    
    objects <- extract fromObjects
    
    -- turn the class heirarchy right-side-out
    let invertClass :: (Show t, Monad m) => [String] -> (String, [FedList t]) -> m [(String, [FedClass])]
        invertClass parents (name, clsContent) = flip evalStateT clsContent $ do
            -- get attributes
            attributes <- extract fromAttr
            let self = FedClass name parents $ M.fromList 
                        [ (attrName, Attribute attrName delivery order spaces)
                        | (attrName, delivery, order, spaces) <- attributes
                        ]
            
            -- get subclasses
            subClasses <- extract fromClass
            subClasses <- mapM (invertClass (name : parents)) subClasses
            
            assertEmpty ("class " ++ name)
            
            return ((name, [self]) : concat subClasses)
    
    -- extract root class(es)
    classes <- assertOnly "objects" fromClass (concat objects)
    classes <- sequence
        [ invertClass [] rootClass
        | rootClass <- classes
        ]
    
    let invertInteraction :: (Show t, Monad m) => [String] -> (String, Delivery, Order, [String], [FedList t]) -> m [(String, [FedInteraction])]
        invertInteraction parents (name, mode, ord, spaces, content) = flip evalStateT content $ do
            -- get parameters
            params <- extract fromParameter
            let self = FedInteraction name parents spaces params
            
            -- get subclasses
            subClasses <- extract fromInteraction
            subClasses <- mapM (invertInteraction (name : parents)) subClasses
            
            assertEmpty ("class " ++ name)
            
            return ((name, [self]) : concat subClasses)
    
    interactions <- extract fromInteractions
    interactions <- assertOnly "interactions" fromInteraction (concat interactions)
    interactions <- sequence
        [ invertInteraction [] rootClass
        | rootClass <- interactions
        ]
    
    assertEmpty "FED"
    return Fed 
        { fedName           = name
        , fedVersion        = version
        , fedSpaces         = M.fromList spaces
        , fedClasses        = M.fromListWith (++) (concat classes)
        , fedInteractions   = M.fromListWith (++) (concat interactions)
        }

fedSExprToFed (fromList -> Just (FED fedItems)) = fedListToFed fedItems
fedSExprToFed (fromAtom -> Just atom) = error ("atom at top level of fed file: " ++ atom)

readFed :: String -> Fed
readFed f = case fedSExprToFed (readFedSExpr f) of
    Left err    -> error err
    Right ok    -> ok

readFedFromFile :: FilePath -> IO Fed
readFedFromFile file = fmap readFed (readFile file)

fedToFedSExpr :: Fed -> SExpr FedList String
fedToFedSExpr Fed{..} = list $ FED
    [ FederationName fedName
    , FederationVersion fedVersion
    , Spaces        [ Space s (map Dimension dims)
                    | (s, dims) <- M.assocs fedSpaces
                    ]
    , Objects       writeMe
    , Interactions  writeMe
    ] where writeMe = error "fedToFedSExpr: write me!"

showFed :: Fed -> String
showFed = showFedSExpr . fedToFedSExpr

writeFedToFile :: FilePath -> Fed -> IO ()
writeFedToFile f = writeFile f . showFed

testStage2 = readFedFromFile "fom_v4.1_16JUNE2008.fed"
