{-# LANGUAGE MultiParamTypeClasses #-}

module Util.HasLens where

import Data.Lens

class HasLens a b where
  view :: Lens a b
