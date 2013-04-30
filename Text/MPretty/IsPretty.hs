{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Text.MPretty.IsPretty where

import Text.MPretty.Pretty
import Data.Monoid
import Data.Map (Map)
import Data.Set (Set)
import Text.MPretty.MonadPretty
import Text.MPretty.StateSpace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T

class IsPretty t where
  pretty :: (MonadPretty env out state m) => t -> m ()
  prettyDropIndent :: (MonadPretty env out state m) => t -> m ()
  prettyDropIndent = dropIndent . pretty
  prettyList :: (MonadPretty env out state m) => [t] -> m ()
  prettyList = 
    encloseSep (pString "[") (pString "]") (pString ",") 
    . map pretty
  prettyDropIndentList :: (MonadPretty env out state m) => [t] -> m ()
  prettyDropIndentList =
    encloseSepDropIndent (pString "[") (pString "]") (pString ",")
    . map pretty

instance IsPretty Int where
  pretty = literal . string . show

instance IsPretty Integer where
  pretty = literal . string . show

instance IsPretty Double where
  pretty = literal . string . show

instance IsPretty Char where
  pretty = literal . string . show
  prettyList = literal . string . ($ []) . showList
  prettyDropIndentList = dropIndent . prettyList

instance IsPretty () where
  pretty () = punctuation $ string "()"
instance (IsPretty a, IsPretty b) => IsPretty (a,b) where
  pretty (a,b) = encloseSep (pString "(") (pString ")") (pString ",") 
    [pretty a, pretty b]
instance (IsPretty a, IsPretty b, IsPretty c) => IsPretty (a,b,c) where
  pretty (a,b,c) = encloseSep (pString "(") (pString ")") (pString ",") 
    [pretty a, pretty b, pretty c]
instance (IsPretty a, IsPretty b, IsPretty c, IsPretty d) => IsPretty (a,b,c,d) where
  pretty (a,b,c,d) = encloseSep (pString "(") (pString ")") (pString ",")
    [pretty a, pretty b, pretty c, pretty d]

instance (IsPretty a) => IsPretty [a] where
  pretty = prettyList
  prettyDropIndent = prettyDropIndentList

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
      prettyMapping (k,v) = group $ hsep
        [ pretty k
        , punctuation $ string "=>"
        , prettyDropIndent v
        ]

showFromPretty :: (IsPretty t) => t -> String
showFromPretty = T.unpack . execPretty . showPretty . pretty
