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

startingPrettyEnv :: PrettyEnv
startingPrettyEnv = PrettyEnv
  { _width = 80
  , _ribbonRatio = 0.8
  , _nesting = 0
  , _layout = Break
  , _failure = NoFail
  , _precedence = closedPrecedence 0
  , _options = defaultPreOptions
  , _palette = defaultPalette
  , _consoleState = emptyConsoleState
  , _doConsole = True
  }

startingPrettyState :: PrettyState
startingPrettyState = PrettyState
  { _column = 0 
  , _ribbon = 0
  }

execPretty :: Pretty () -> Text
execPretty aM =
  let aM' = emitConsoleStateCodes >> aM
      ((),_,t) = head $ runPretty aM' startingPrettyEnv startingPrettyState
  in t
