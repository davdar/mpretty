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

data Style = PreAlignStyle | PreSnugStyle | PostStyle | IndentStyle
  deriving (Eq, Ord, Show, Enum)
data Buffering = Buffer | NoBuffer
  deriving (Eq, Ord, Show, Enum)

data Direction = LeftD | RightD | NoD
  deriving (Eq, Show, Enum)
  
data Precedence = Precedence Int Bool
  deriving (Eq, Ord, Show)

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
  , _keywordColor :: ConsoleState
  } deriving (Eq, Ord, Show)
makeLens ''Palette

defaultPalette :: Palette
defaultPalette = Palette
  { _punctuationColor = setConsoleColor Dull Yellow
  , _literalColor = setConsoleColor Dull Red
  , _binderColor = setConsoleColor Dull Cyan
  , _keywordColor = 
      setConsole underliningM SingleUnderline
      `mappend` setConsole intensityM BoldIntensity
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

defaultPrettyEnv :: PrettyEnv
defaultPrettyEnv = PrettyEnv
  { _width = 80
  , _ribbonRatio = 0.8
  , _nesting = 0
  , _layout = Break
  , _failure = NoFail
  , _precedence = (Precedence 0 False,Precedence 0 False)
  , _options = defaultPreOptions
  , _palette = defaultPalette
  , _consoleState = emptyConsoleState
  , _doConsole = True
  }

instance HasLens PrettyEnv PrettyEnv where
  view = iso id id

data PrettyState = PrettyState
  { _column :: Int
  , _ribbon :: Int
  } deriving (Eq, Ord, Show)
makeLens ''PrettyState

defaultPrettyState :: PrettyState
defaultPrettyState = PrettyState
  { _column = 0 
  , _ribbon = 0
  }

instance HasLens PrettyState PrettyState where
  view = iso id id
