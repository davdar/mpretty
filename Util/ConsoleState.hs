{-# LANGUAGE TemplateHaskell #-}

module Util.ConsoleState where

import Data.Monoid
import System.Console.ANSI
import Data.Lens.Template
import Data.Maybe
import Control.Monad
import Data.Lens

data ConsoleState = ConsoleState
  { _intensityM :: Maybe ConsoleIntensity
  , _italicizedM :: Maybe Bool
  , _underliningM :: Maybe Underlining
  , _blinkSpeedM :: Maybe BlinkSpeed
  , _visibleM :: Maybe Bool
  , _swapFgBgM :: Maybe Bool
  , _gcolorM :: Maybe (ConsoleLayer,ColorIntensity,Color)
  } deriving (Eq, Ord, Show)
makeLens ''ConsoleState

emptyConsoleState :: ConsoleState
emptyConsoleState = 
  ConsoleState Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Monoid ConsoleState where
  mempty = emptyConsoleState
  mappend cs1 cs2 =
    let ConsoleState iy1 it1 u1 b1 v1 s1 g1 = cs1
        ConsoleState iy2 it2 u2 b2 v2 s2 g2 = cs2
    in ConsoleState (mplus iy1 iy2) (mplus it1 it2) (mplus u1 u2) 
         (mplus b1 b2) (mplus v1 v2) (mplus s1 s2) (mplus g1 g2)

setConsole :: Lens ConsoleState (Maybe a) -> a -> ConsoleState
setConsole l x = setL l (Just x) emptyConsoleState

setConsoleColor :: Color -> ColorIntensity -> ConsoleState
setConsoleColor c ci = setConsole gcolorM (Foreground,ci,c)

setConsoleStateCodes :: ConsoleState -> String
setConsoleStateCodes cs =
  setSGRCode $ Reset : catMaybes
    [ fmap SetConsoleIntensity $ _intensityM cs
    , fmap SetItalicized $ _italicizedM cs
    , fmap SetUnderlining $ _underliningM cs
    , fmap SetBlinkSpeed $ _blinkSpeedM cs
    , fmap SetVisible $ _visibleM cs
    , fmap SetSwapForegroundBackground $ _swapFgBgM cs
    , fmap (\(cl,ci,c) -> SetColor cl ci c) $ _gcolorM cs
    ]
