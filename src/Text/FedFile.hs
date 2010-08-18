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

-- |Get the qualified name of an object class.  A class's qualified name is
-- the class's simple name appended to its parent's qualified name.
classQName :: FedClass -> [String]
classQName cls = reverse (className cls : classParents cls)

-- |Get the qualified name of an interaction class.  A class's qualified name is
-- the class's simple name appended to its parent's qualified name.
interactionQName :: FedInteraction -> [String]
interactionQName cls = reverse (interactionName cls : interactionParents cls)

-- |Get the parent, if there is one, of an object class.
classParent :: FedClass -> Maybe String
classParent = listToMaybe . classParents

-- |Get the parent, if there is one, of an interaction class.
interactionParent :: FedInteraction -> Maybe String
interactionParent = listToMaybe . interactionParents

-- |Create a 'Forest' matching the structure of the object class hierarchy, with
-- each node labeled by the name of the corresponding class.  Primarily
-- useful for preparing input to 'drawForest'.
classHeirarchy :: Fed -> Forest String
classHeirarchy = classHeirarchyWith className

-- |Like 'classHeirarchy' but with a user-provided function to compute the node labels.
classHeirarchyWith :: (FedClass -> a) -> Fed -> Forest a
classHeirarchyWith op fed = map (fmap op) (fedClasses fed)

-- |Create a 'Forest' matching the structure of the interaction hierarchy, with
-- each node labeled by the name of the corresponding interaction.  Primarily
-- useful for preparing input to 'drawForest'.
interactionHeirarchy :: Fed -> Forest String
interactionHeirarchy = interactionHeirarchyWith interactionName

-- |Like 'interactionHeirarchy' but with a user-provided function to compute the node labels.
interactionHeirarchyWith :: (FedInteraction -> a) -> Fed -> Forest a
interactionHeirarchyWith op fed = map (fmap op) (fedInteractions fed)

-- |Get the object class(es) from the given 'Fed' with a given name
lookupClass :: Fed -> String -> [FedClass]
lookupClass fed c = M.findWithDefault [] c (fedClassesByName fed)

-- |Get the object class (if it exists) from the given 'Fed' with a given qualified name
getClass :: Fed -> [String] -> Maybe FedClass
getClass fed qName = getNodeAtPath (className `are` qName) (fedClasses fed)

-- |Get the interaction(s) from the given 'Fed' with a given name.
lookupInteraction :: Fed -> String -> [FedInteraction]
lookupInteraction fed c = M.findWithDefault [] c (fedInteractionsByName fed)

-- |Get the interaction (if it exists) from the given 'Fed' with a given qualified name
getInteraction :: Fed -> [String] -> Maybe FedInteraction
getInteraction fed qName = getNodeAtPath (interactionName `are` qName) (fedInteractions fed)

-- |Get the root object class(es) of a 'Fed'.  As far as I know, there must always
-- be exactly one, named \"ObjectRoot\".
rootClasses :: Fed -> [FedClass]
rootClasses fed   = map rootLabel $ fedClasses fed

-- |Get all the subclasses of an object class in the given 'Fed'.
subClasses :: Fed -> FedClass -> [FedClass]
subClasses  fed c 
    = map rootLabel . concatMap subForest 
    $ getSubTreesAtPath (className `are` classQName c) (fedClasses fed)

-- |Get the root interaction(s) of a 'Fed'.  As far as I know, there must always
-- be exactly one, named \"InteractionRoot\".
rootInteractions :: Fed -> [FedInteraction]
rootInteractions fed = map rootLabel $ fedInteractions fed

-- |Get all the subclasses of an interaction class in the given 'Fed'.
subInteractions :: Fed -> FedInteraction -> [FedInteraction]
subInteractions  fed c 
    = map rootLabel . concatMap subForest 
    $ getSubTreesAtPath (interactionName `are` interactionQName c) (fedInteractions fed)

-- |Find all object classes satisfying some predicate
findClasses :: Fed -> (FedClass -> Bool) -> [FedClass]
findClasses fed p = filter p . concatMap flatten $ fedClasses fed

-- |Predicate selecting qualified names of the standard object or interaction 
-- classes that are included just to make the RTI happy - the root, the
-- RTIprivate subtree (generally empty), and the Manager subtree
isReservedQName :: [String] -> Bool
isReservedQName i = case take 2 i of
    [_] -> True
    [_, "Manager"] -> True
    [_, "RTIprivate"] -> True
    _ -> False

-- |Predicate selecting the standard object classes that are included just to make the 
-- RTI happy - the ObjectRoot, the RTIprivate subtree (generally empty),
-- and the Manager subtree
isStandardClass :: FedClass -> Bool
isStandardClass = isStandardQName . classQName

-- |Gets all FOM classes (the ones for which isStandardClass returns False)
fomClasses :: Fed -> [FedClass]
fomClasses fed = findClasses fed (not . isStandardClass)

-- |Find all interactions in a FOM satisfying some predicate
findInteractions  :: Fed -> (FedInteraction -> Bool) -> [FedInteraction]
findInteractions fed p = filter p . concatMap flatten $ fedInteractions fed

-- |Predicate selecting the standard interactions that are included just to make the 
-- RTI happy - the InteractionRoot, the RTIprivate subtree (generally empty),
-- and the Manager subtree
isStandardInteraction :: FedInteraction -> Bool
isStandardInteraction = isStandardQName . interactionQName

-- |Gets all FOM interactions (the ones for which isStandardInteraction returns False)
fomInteractions :: Fed -> [FedInteraction]
fomInteractions fed = findInteractions fed (not . isStandardInteraction)
