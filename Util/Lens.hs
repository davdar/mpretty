module Util.Lens where

import Control.Monad.Reader
import Data.Lens

look :: (MonadReader env m) => Lens env b -> m b
look l = liftM (getL l) ask

