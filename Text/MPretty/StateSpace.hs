{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Text.MPretty.StateSpace where

import Util.PartialOrder
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

data Style = PreAlignStyle | PreSnugStyle | PostStyle | IndentStyle
  deriving (Eq, Ord, Show, Enum)
data Buffering = Buffer | NoBuffer
  deriving (Eq, Ord, Show, Enum)

data Direction = LeftD | RightD | NoD
  deriving (Eq, Show, Enum)
  
data Precedence = Precedence Direction Int Int
  deriving (Eq, Show)
instance PartialOrder Precedence where
  lte (Precedence d1 i1 j1) (Precedence d2 i2 j2) =
    d1 == d2 
    && (i1 < i2 
        || (i1 == i2 
            && j1 <= j2))

data Options = Options
  { _style :: Style
  , _buffering :: Buffering
  , _indentation :: Int
  } deriving (Eq, Ord, Show)
makeLens ''Options

defaultPreOptions :: Options
defaultPreOptions = Options PreAlignStyle Buffer 2

defaultPostOptions :: Options
defaultPostOptions = Options PostStyle NoBuffer 2

defaultIndentStyle :: Options
defaultIndentStyle = Options IndentStyle NoBuffer 2

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
  , _precedence :: (Precedence,Precedence)
  -- style
  , _options :: Options
  -- truncation
  -- , _truncateDepth :: Int
  -- , _truncate :: Bool
  -- console
  , _palette :: Palette
  , _consoleState :: ConsoleState
  , _doConsole :: Bool
  } deriving (Eq, Show)
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
