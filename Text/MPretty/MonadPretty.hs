{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell, ConstraintKinds, FlexibleContexts #-}

module Text.MPretty.MonadPretty where

import Prelude hiding (id, (.))

import Data.Char
import Control.Category
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Lens
import System.Console.ANSI
import Text.MPretty.StateSpace
import Util.ConsoleState
import Util.HasLens
import Util.Lens
import Util.List

type (MonadRWS env out state m) =
  ( MonadReader env m
  , MonadWriter out m
  , MonadState state m
  )

type (MonadPretty env out state m) = 
  ( MonadRWS env out state m
  , MonadPlus m
  , HasLens env PrettyEnv
  , PrettyOutput out
  , HasLens state PrettyState
  )

----- "Primitives" -----

text :: (MonadPretty env out state m) => out -> m ()
text s = do
  tell s
  column . view %= (pLength s +)
  ribbon . view %= (countNonSpace s +)
  m <- look $ failure . view
  when (m == Fail) $ do
    w <- look $ width . view
    rr <- look $ ribbonRatio. view
    k <- access $ column . view
    r <- access $ ribbon . view
    when (k > w) mzero
    when (fromIntegral r > fromIntegral w * rr) mzero
  where
    countNonSpace = pFoldl (\i c -> i + if isSpace c then 0 else 1) 0

space :: (MonadPretty env out state m) => Int -> m ()
space = text . pString . flip replicate ' '

tryFlat :: (MonadPretty env out state m) => m a -> m a -> m a
tryFlat dFlat dBreak = do
  l <- look $ layout . view
  case l of
    Flat -> dFlat
    Break -> dBreak

newline :: (MonadPretty env out state m) => m ()
newline = do
  tell $ pString "\n"
  column . view ~= 0
  return ()

hardLine :: (MonadPretty env out state m) => m ()
hardLine = do
  i <- look $ nesting . view
  newline
  space i

tryFlat :: (MonadPretty env out state m) => m a -> m a
tryFlat = local (modL (failure . view) $ const Fail) . local (modL (layout . view) $ const Flat)

-- softLine :: (MonadPretty env out state m) => m () -> m () -> m () -> m ()
-- softLine fs bsb bsa =
--   tryFlat fs $ bsb >> hardLine >> bsa

nest :: (MonadPretty env out state m) => Int -> m a -> m a
nest i = local $ modL (nesting . view) (i +)

group :: (MonadPretty env out state m) => m a -> m a
group aM = do
  l <- look $ layout . view
  case l of
    Flat -> aM
    Break -> msum
      [ tryFlat aM
      , aM
      ]

align :: (MonadPretty env out state m) => m a -> m a
align aM = do
  i <- look $ nesting . view
  k <- access $ column . view
  nest (k-i) aM

hang :: (MonadPretty env out state m) => Int -> m a -> m a
hang i = align . nest i

----- Helpers -----

withPrecedence :: (MonadPretty env out state m) => (Int,Int) -> (Int,Int) -> m a -> m a
withPrecedence lp rp = local $ modL view $ setL precedence (lp,rp)

preStyle :: (MonadPretty env out state m) => m a -> m a
preStyle = local $ modL (options . view) $ setL style PreStyle

postStyle :: (MonadPretty env out state m) => m a -> m a
postStyle = local $ modL (options . view) $ setL style PostStyle

indentStyle :: (MonadPretty env out state m) => m a -> m a
indentStyle = local $ modL (options . view) $ setL style IndentStyle

buffer :: (MonadPretty env out state m) => m a -> m a
buffer = local $ setL (buffering . options . view) Buffer

noBuffer :: (MonadPretty env out state m) => m a -> m a
noBuffer = local $ setL (buffering . options . view) NoBuffer

noConsole :: (MonadPretty env out state m) => m a -> m a
noConsole = local $ setL (doConsole . view) False

layoutWidth :: (MonadPretty env out state m) => Int -> m a -> m a
layoutWidth = local . setL (width . view)

----- Style helpers -----

getBuff :: (MonadPretty env out state m) => m out
getBuff = do
  b <- look $ buffering . options . view
  return $ case b of
    Buffer -> pString " "
    NoBuffer -> mempty

dropIndent :: (MonadPretty env out state m) => m () -> m ()
dropIndent d = do
  i <- look $ indent . options . view
  nest i $ do
    softLine mempty mempty mempty
    d

encloseSepPre :: (MonadPretty env out state m)
              => out -> out -> out -> [m ()] -> m ()
encloseSepPre lbrac rbrac sep ds = do
  buff <- getBuff
  let f = foldr (.) id
        [ mapFirst $ \ d -> do
            punctuation $ text lbrac
            tryFlat (text buff) $ do 
              space $ sepL - lbracL
              text buff
            d
        , mapRest $ \ d -> do
            tryFlat (text buff) $ do
              hardLine
              space $ lbracL - sepL
            punctuation $ text sep
            text buff
            d
        , mapLast $ \ d -> do
            d
            tryFlat (text buff) hardLine
            punctuation $ text rbrac
        ]
  group . sequence_ . f $ map (withPrecedence (0,0) (0,0) . align) ds

encloseSepPost :: (MonadPretty env out state m)
               => out -> out -> out -> [m ()] -> m ()
encloseSepPost lbrac rbrac sep ds = do
  b <- look $ buffering . options . view
  let buff = case b of
        Buffer -> pString " "
        NoBuffer -> mempty
      f = foldr (.) id $ case s of
        [ mapFirst $ \ d -> do
            punctuation $ text lbrac
            text buff
            d
        , mapRest $ \ d -> do
            softLine buff mempty mempty
            text $ pString $ flip replicate ' ' $ lbracL
            text buff
            d
        , mapLeading $ \ d -> do
            d
            text buff
            punctuation $ text sep
        , mapLast $ \ d -> do
            d
            text buff
            punctuation $ text rbrac
        ]
  group . sequence_ . f $ map (withPrecedence (0,0) (0,0) . align) ds

encloseSepIndent :: (MonadPretty env out state m)
                 => out -> out -> out -> [m ()] -> m ()
encloseSepIndent lbrac rbrac sep ds = do
  b <- look $ buffering . options . view
  let buff = case b of
        Buffer -> pString " "
        NoBuffer -> mempty
      f = foldr (.) id $ case s of
          [ mapFirst $ \ d -> do
              punctuation $ text lbrac
              d
          , map $ \ d -> do
              softLine buff mempty $ pString $ replicate i ' '
              d
          , mapLeading $ \ d -> do
              d
              text buff
              punctuation $ text sep
          , mapLast $ \ d -> do
              d
              softLine buff mempty mempty
          ]
  group . sequence_ . f $ map (withPrecedence (0,0) (0,0) . align) ds

encloseSep :: (MonadPretty env out state m) 
           => out -> out -> out -> [m ()] -> m ()
encloseSep lbrac rbrac _ [] = punctuation $ text lbrac >> text rbrac
encloseSep lbrac rbrac sep ds = do
  s <- look $ style . options . view
  case s of
    PreStyle -> encloseSepPre lbrac rbrac sep ds
    PostStyle -> encloseSepPost lbrac rbrac sep ds
    IndentStyle -> encloseSepIndent lbrac rbrac sep ds

encloseSepPreDrop :: (MonadPretty env out state m)
                  => out -> out -> out -> [m ()] -> m ()
encloseSepPreDrop lbrac rbrac sep = lineDrop . encloseSepPreDrop lbrac rbrac sep

encloseSepPostDrop :: (MonadPretty env out state m)
                   => out -> out -> out -> [m ()] -> m ()
encloseSepPostDrop lbrac rbrac sep = lineDrop . lbrac rbrac sep

encloseSepIndentDrop :: (MonadPretty env out state m)
encloseSepIndentDrop lbrac rbrac sep = do
  b <- look $ buffering . options . view
  let buff = case b of
        Buffer -> pString " "
        NoBuffer -> mempty
      f = foldr (.) id $ case s of
          [ mapFirst $ \ d -> do
              punctuation $ text lbrac
              d
          , map $ \ d -> do
              softLine buff mempty $ pString $ replicate i ' '
              d
          , mapLeading $ \ d -> do
              d
              text buff
              punctuation $ text sep
          , mapLast $ \ d -> do
              d
              softLine buff mempty mempty
          ]
  group . sequence_ . f $ map (withPrecedence (0,0) (0,0) . align) ds
  
encloseSepDrop :: (MonadPretty env out state m)
               => out -> out -> out -> [m ()] -> m ()
encloseSepDrop = undefined

infixOp :: (MonadPretty env out state m) => Int -> out -> m () -> m () -> m ()
infixOp = undefined

----- ANSI Console helpers -----

emitConsoleStateCodes :: (MonadPretty env out state m) => m ()
emitConsoleStateCodes = do
  proceed <- look $ doConsole . view
  when proceed $ do
    cs <- look $ consoleState . view
    tell $ pString $ setConsoleStateCodes cs

localConsole :: (MonadPretty env out state m) => (ConsoleState -> ConsoleState) -> m a -> m a
localConsole f aM = do
  a <- local (modL (consoleState . view) f) $ do
    emitConsoleStateCodes
    aM
  emitConsoleStateCodes
  return a

intensity :: (MonadPretty env out state m) => ConsoleIntensity -> m a -> m a
intensity = localConsole . setL intensityM . Just

italicized :: (MonadPretty env out state m) => Bool -> m a -> m a
italicized = localConsole . setL italicizedM . Just

underlining :: (MonadPretty env out state m) => Underlining -> m a -> m a
underlining = localConsole . setL underliningM . Just

blinkSpeed :: (MonadPretty env out state m) => BlinkSpeed -> m a -> m a
blinkSpeed = localConsole . setL blinkSpeedM . Just

visible :: (MonadPretty env out state m) => Bool -> m a -> m a
visible = localConsole . setL visibleM . Just

swapFgBg :: (MonadPretty env out state m) => Bool -> m a -> m a
swapFgBg = localConsole . setL swapFgBgM . Just

gcolor :: (MonadPretty env out state m) => ConsoleLayer -> ColorIntensity -> Color -> m a -> m a
gcolor cl ci c = localConsole $ setL gcolorM $ Just (cl,ci,c)

color :: (MonadPretty env out state m) => Color -> m a -> m a
color = gcolor Foreground Vivid

punctuation :: (MonadPretty env out state m) => m a -> m a
punctuation aM = do
  pc <- look $ punctuationColor . palette . view
  localConsole (mappend pc) aM

literal :: (MonadPretty env out state m) => m a -> m a
literal aM = do
  lc <- look $ literalColor . palette . view
  localConsole (mappend lc) aM

----- Testing -----

displayVariants :: (MonadPretty env out state m) => Int -> m () -> m ()
displayVariants i aM = do
  let configs =
        [ Options PreStyle    Buffer   i
        , Options PreStyle    NoBuffer i
        , Options PostStyle   Buffer   i
        , Options PostStyle   NoBuffer i
        , Options IndentStyle Buffer   i
        , Options IndentStyle NoBuffer i
        ]
  forM_ configs $ \ o -> do
    hardLine
    text $ pString "##### "
    text $ pString $ show o
    text $ pString " #####"
    hardLine
    local (setL (options . view) o) aM
    hardLine
