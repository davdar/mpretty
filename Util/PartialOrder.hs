module Util.PartialOrder where

class PartialOrder t where
  lte :: t -> t -> Bool

(|<=|) :: (PartialOrder t) => t -> t -> Bool ; (|<=|) = lte
infix 4 |<=|
