module Codec.FedFile
    ( Fed(..)
    , FedClass(..)
    , FedInteraction(..)
    , Attribute(..)
    , Delivery(..)
    , Order(..)
    
    , readFed
    , showFed
    , readFedFromFile
    , writeFedToFile
    
    , module Codec.FedFile
    ) where

import Codec.FedFile.Stage1
import Codec.FedFile.Stage2
import Data.Maybe
import Data.Tree
import qualified Data.Map as M

classQName       cls = className       cls : classParents       cls
interactionQName cls = interactionName cls : interactionParents cls

classParent = listToMaybe . classParents
interactionParent = listToMaybe . interactionParents

classHeirarchy = classHeirarchyWith className
classHeirarchyWith op fed = unfoldForest f (rootClasses fed)
    where
        f qName@(c:parents) = (op . fromJust . getClass fed $ qName, subClasses fed qName)

interactionHeirarchy = interactionHeirarchyWith interactionName
interactionHeirarchyWith op fed = unfoldForest f (rootInteractions fed)
    where
        f qName@(c:parents) = (op . fromJust . getInteraction fed $ qName, subInteractions fed qName)

lookupClass fed c = M.findWithDefault [] c (fedClasses fed)

getClass fed qName@(c:parents)
    = listToMaybe 
    . filter (\cls -> classParents cls == parents) 
    $ lookupClass fed c

lookupInteraction fed c = M.findWithDefault [] c (fedInteractions fed)

getInteraction fed qName@(c:parents)
    = listToMaybe 
    . filter (\cls -> interactionParents cls == parents) 
    $ lookupInteraction fed c

rootClasses fed   = fmap classQName $ subClasses' classParents [] (fedClasses fed)
subClasses  fed c = fmap classQName $ subClasses' classParents c  (fedClasses fed)

rootInteractions fed   = fmap interactionQName $ subClasses' interactionParents [] (fedInteractions fed)
subInteractions  fed i = fmap interactionQName $ subClasses' interactionParents i  (fedInteractions fed)

subClasses' getParent parent classes =
    [ c
    | (name, cs) <- M.assocs classes
    , c <- cs
    , getParent c == parent
    ]

