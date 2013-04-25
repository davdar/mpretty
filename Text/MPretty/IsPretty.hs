{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Text.MPretty.IsPretty where

import Data.Monoid
import Data.Map (Map)
import Data.Set (Set)
import Text.MPretty.MonadPretty
import Text.MPretty.StateSpace
import qualified Data.Map as Map
import qualified Data.Set as Set

class IsPretty t where
  pretty :: (MonadPretty env out state m) => t -> m ()
  prettyDrop :: (MonadPretty env out state m) => t -> m ()
  prettyDrop = pretty
  prettyList :: (MonadPretty env out state m) => [t] -> m ()
  prettyList = 
    encloseSep (pString "[") (pString "]") (pString ",") 
    . map pretty
  prettyDropList :: (MonadPretty env out state m) => [t] -> m ()
  prettyDropList =
    encloseSepDrop (pString "[") (pString "]") (pString ",")
    . map pretty

instance IsPretty Int where
  pretty = literal . text . pString . show

instance IsPretty Integer where
  pretty = literal . text . pString . show

instance IsPretty Char where
  pretty = literal . text . pString . show
  prettyList = literal . text . pString . ($ []) . showList
  prettyDropList = lineDrop . prettyList

instance (IsPretty a) => IsPretty [a] where
  pretty = prettyList

instance (IsPretty a) => IsPretty (Set a) where
  pretty = 
    encloseSep (pString "{") (pString "}") (pString ",") 
    . map pretty
    . Set.toList

instance (IsPretty k, IsPretty v) => IsPretty (Map k v) where
  pretty = 
    encloseSep (pString "{") (pString "}") (pString ",") 
    . map prettyMapping
    . Map.toList
    where
      prettyMapping (k,v) = infixOp 90 (pString "=>") (pretty k) (pretty v)

