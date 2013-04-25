{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Text.MPretty.StateSpace where

import Data.List
import System.Console.ANSI
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Util.ConsoleState
import Data.Lens.Template
import Data.Lens
import Util.HasLens

class (Monoid out) => PrettyOutput out where
  pString :: String -> out
  pLength :: out -> Int
  pFoldl :: (a -> Char -> a) -> a -> out -> a

instance PrettyOutput String where
  pString = id
  pLength = length
  pFoldl = foldl'

instance PrettyOutput Text where
  pString = T.pack
  pLength = fromIntegral . T.length
  pFoldl = T.foldl'

data Layout = Flat | Break
  deriving (Eq, Ord, Show, Enum)
data Failure = Fail | NoFail
  deriving (Eq, Ord, Show, Enum)

data Style = PreStyle | PostStyle | IndentStyle
  deriving (Eq, Ord, Show, Enum)
data Buffering = Buffer | NoBuffer
  deriving (Eq, Ord, Show, Enum)

data Options = Options
  { _style :: Style
  , _buffering :: Buffering
  , _indentation :: Int
  } deriving (Eq, Ord, Show)
makeLens ''Options

defaultPreOptions :: Options
defaultPreOptions = Options PreStyle Buffer 2

defaultPostOptions :: Options
defaultPostOptions = Options PostStyle NoBuffer 2

data Palette = Palette
  { _punctuationColor :: ConsoleState
  , _literalColor :: ConsoleState
  , _binderColor :: ConsoleState
  } deriving (Eq, Ord, Show)
makeLens ''Palette

defaultPalette :: Palette
defaultPalette = Palette
  { _punctuationColor = setConsoleColor Yellow Dull
  , _literalColor = setConsoleColor Red Dull
  , _binderColor = setConsoleColor Cyan Dull
  }

data PrettyEnv = PrettyEnv
  -- layout options
  { _width :: Int
  , _ribbonRatio :: Double
  -- dynamic environment
  , _nesting :: Int
  , _layout :: Layout
  , _failure :: Failure
  -- , _depth :: Int
  , _precedence :: ((Int,Int), (Int,Int))
  -- style
  , _options :: Options
  -- truncation
  -- , _truncateDepth :: Int
  -- , _truncate :: Bool
  -- console
  , _palette :: Palette
  , _consoleState :: ConsoleState
  , _doConsole :: Bool
  } deriving (Eq, Ord, Show)
makeLens ''PrettyEnv

instance HasLens PrettyEnv PrettyEnv where
  view = iso id id

data PrettyState = PrettyState
  { _column :: Int
  , _ribbon :: Int
  } deriving (Eq, Ord, Show)
makeLens ''PrettyState

instance HasLens PrettyState PrettyState where
  view = iso id id
