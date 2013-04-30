{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.MPretty.Pretty where

import Data.Maybe
import Text.MPretty.StateSpace
import Control.Monad.RWS
import Data.Text.Lazy (Text)
import Util.ConsoleState
import Text.MPretty.MonadPretty

newtype Pretty a = Pretty 
  { unPretty :: RWST PrettyEnv Text PrettyState [] a }
  deriving 
  ( Monad
  , MonadReader PrettyEnv
  , MonadWriter Text
  , MonadState PrettyState
  , MonadPlus
  )

runPretty :: Pretty a -> PrettyEnv -> PrettyState -> [(a,PrettyState,Text)]
runPretty aM r s = runRWST (unPretty aM) r s

execPretty :: Pretty () -> Text
execPretty aM =
  let aM' = emitConsoleStateCodes >> group aM
      ((),_,t) = head $ runPretty aM' defaultPrettyEnv defaultPrettyState
  in t
