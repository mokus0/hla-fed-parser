module Text.FedFile
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
    
    , module Text.FedFile
    ) where

import Text.FedFile.Stage1
import Text.FedFile.Stage2
import Text.FedFile.TreeUtils
import Data.Maybe
import Data.Tree
import qualified Data.Map as M

classQName :: FedClass -> [String]
classQName cls = reverse (className cls : classParents cls)

interactionQName :: FedInteraction -> [String]
interactionQName cls = reverse (interactionName cls : interactionParents cls)

classParent :: FedClass -> Maybe String
classParent = listToMaybe . classParents

interactionParent :: FedInteraction -> Maybe String
interactionParent = listToMaybe . interactionParents

classHeirarchy :: Fed -> Forest String
classHeirarchy = classHeirarchyWith className

classHeirarchyWith :: (FedClass -> a) -> Fed -> Forest a
classHeirarchyWith op fed = map (fmap op) (fedClasses fed)

interactionHeirarchy :: Fed -> Forest String
interactionHeirarchy = interactionHeirarchyWith interactionName

interactionHeirarchyWith :: (FedInteraction -> a) -> Fed -> Forest a
interactionHeirarchyWith op fed = map (fmap op) (fedInteractions fed)

lookupClass :: Fed -> String -> [FedClass]
lookupClass fed c = M.findWithDefault [] c (fedClassesByName fed)

getClass :: Fed -> [String] -> Maybe FedClass
getClass fed qName = getNodeAtPath (className `are` qName) (fedClasses fed)

lookupInteraction :: Fed -> String -> [FedInteraction]
lookupInteraction fed c = M.findWithDefault [] c (fedInteractionsByName fed)

getInteraction :: Fed -> [String] -> Maybe FedInteraction
getInteraction fed qName = getNodeAtPath (interactionName `are` qName) (fedInteractions fed)

rootClasses :: Fed -> [FedClass]
rootClasses fed   = map rootLabel $ fedClasses fed

subClasses :: Fed -> FedClass -> [FedClass]
subClasses  fed c 
    = map rootLabel . concatMap subForest 
    $ getSubTreesAtPath (className `are` classQName c) (fedClasses fed)

rootInteractions :: Fed -> [FedInteraction]
rootInteractions fed = map rootLabel $ fedInteractions fed

subInteractions :: Fed -> FedInteraction -> [FedInteraction]
subInteractions  fed c 
    = map rootLabel . concatMap subForest 
    $ getSubTreesAtPath (interactionName `are` interactionQName c) (fedInteractions fed)

findClasses :: Fed -> (FedClass -> Bool) -> [FedClass]
findClasses fed p = filter p . concatMap flatten $ fedClasses fed

findInteractions  :: Fed -> (FedInteraction -> Bool) -> [FedInteraction]
findInteractions fed p = filter p . concatMap flatten $ fedInteractions fed
