{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell, ConstraintKinds, FlexibleContexts #-}

module Text.MPretty.MonadPretty where

import Prelude hiding (id, (.))

import qualified Data.List as L
import Util.PartialOrder
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
text s = 
  let sL = pLength s
      nsL = countNonSpace s
  in do
    tell s
    column . view %= (+) sL
    ribbon . view %= (+) nsL
    m <- look $ failure . view
    when (m == Fail) $ do
      w <- look $ width . view
      rr <- look $ ribbonRatio . view
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

hardLine :: (MonadPretty env out state m) => m ()
hardLine = do
  i <- look $ nesting . view
  tell $ pString "\n"
  column . view ~= 0
  ribbon . view ~= 0
  space i

flatFail :: (MonadPretty env out state m) => m a -> m a
flatFail = 
  local (modL (failure . view) $ const Fail) 
  . local (modL (layout . view) $ const Flat)

nest :: (MonadPretty env out state m) => Int -> m a -> m a
nest i = local $ modL (nesting . view) (i +)

group :: (MonadPretty env out state m) => m a -> m a
group aM = do
  l <- look $ layout . view
  case l of
    Flat -> aM
    Break -> msum
      [ flatFail aM
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

closedPrecedence :: Int -> (Precedence,Precedence)
closedPrecedence i = (Precedence NoD i 0,Precedence NoD i 0)

withPrecedence :: (MonadPretty env out state m) 
               => (Precedence,Precedence) -> m a -> m a
withPrecedence = local . modL view . setL precedence

preStyle :: (MonadPretty env out state m) => m a -> m a
preStyle = local $ modL (options . view) $ setL style PreAlignStyle

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
  i <- look $ indentation . options . view
  tryFlat (return ()) $ do
    hardLine
    space i
  align d

encloseSepPre :: (MonadPretty env out state m)
              => out -> out -> out -> Bool -> [m ()] -> m ()
encloseSepPre lbrac rbrac sep snug ds = 
  let lbracL = pLength lbrac
      sepL = pLength sep
  in do
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
              if snug then text buff else tryFlat (text buff) hardLine
              punctuation $ text rbrac
          ]
    group . sequence_ . f $ map (withPrecedence (closedPrecedence 0)  . align) ds

encloseSepPost :: (MonadPretty env out state m)
               => out -> out -> out -> [m ()] -> m ()
encloseSepPost lbrac rbrac sep ds =
  let lbracL = pLength lbrac
  in do
    buff <- getBuff
    let f = foldr (.) id $
          [ mapFirst $ \ d -> do
              punctuation $ text lbrac
              text buff
              d
          , mapRest $ \ d -> do
              tryFlat (return ()) $ do
                hardLine
                space lbracL
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
    group . sequence_ . f $ map (withPrecedence (closedPrecedence 0) . align) ds

encloseSepIndent :: (MonadPretty env out state m)
                 => out -> out -> out -> [m ()] -> m ()
encloseSepIndent lbrac rbrac sep ds = do
  buff <- getBuff
  i <- look $ indentation . options . view
  let f = foldr (.) id $
        [ mapFirst $ \ d -> do
            punctuation $ text lbrac
            d
        , map $ \ d -> do
            tryFlat (text buff) $ do
              hardLine
              space i
            d
        , mapLeading $ \ d -> do
            d
            text buff
            punctuation $ text sep
        , mapLast $ \ d -> do
            d
            tryFlat (text buff) hardLine
            punctuation $ text rbrac
        ]
  group . sequence_ . f $ map (withPrecedence (closedPrecedence 0) . align) ds

encloseSep :: (MonadPretty env out state m) 
           => out -> out -> out -> [m ()] -> m ()
encloseSep lbrac rbrac _ [] = punctuation $ text lbrac >> text rbrac
encloseSep lbrac rbrac sep ds = do
  s <- look $ style . options . view
  case s of
    PreAlignStyle -> encloseSepPre lbrac rbrac sep False ds
    PreSnugStyle -> encloseSepPre lbrac rbrac sep True ds
    PostStyle -> encloseSepPost lbrac rbrac sep ds
    IndentStyle -> encloseSepIndent lbrac rbrac sep ds

encloseSepDropIndent :: (MonadPretty env out state m)
                     => out -> out -> out -> [m ()] -> m ()
encloseSepDropIndent lbrac rbrac _ [] = punctuation $ text lbrac >> text rbrac
encloseSepDropIndent lbrac rbrac sep ds = do
  s <- look $ style . options . view
  case s of
    PreAlignStyle -> dropIndent $ encloseSepPre lbrac rbrac sep False ds
    PreSnugStyle -> dropIndent $ encloseSepPre lbrac rbrac sep True ds
    PostStyle -> dropIndent $ encloseSepPost lbrac rbrac sep ds
    IndentStyle -> encloseSepIndent lbrac rbrac sep ds

infixOp :: (MonadPretty env out state m) 
        => Direction -> Int -> m () -> m () -> m () -> m ()
infixOp d n infixD leftD rightD = do
  s <- look $ style . options . view
  buff <- getBuff
  (pl,pr) <- look $ precedence . view
  let pl' = Precedence d n $ case d of
        LeftD -> 0
        RightD -> 1
        NoD -> 0
      pr' = Precedence d n $ case d of
        LeftD -> 1
        RightD -> 0
        NoD -> 0
      enclose = if pl |<=| pl' && pr |<=| pr'
        then id
        else parenthesize
  enclose $ do
    withPrecedence (pl,pl') leftD
    let preSep = do
          tryFlat (text buff) hardLine
          infixD
          text buff
        postSep = do
          text buff
          infixD
          tryFlat (text buff) hardLine
    case s of
      PreAlignStyle -> preSep
      PreSnugStyle -> preSep
      PostStyle -> postSep
      IndentStyle -> postSep
    withPrecedence (pr',pr) rightD

hsep :: (MonadPretty env out state m) => [m ()] -> m ()
hsep ds = do
  buff <- getBuff
  foldr (>>) (return ()) $ L.intersperse (text buff) ds

parenthesize :: (MonadPretty env out state m) => m () -> m ()
parenthesize d = text (pString "(") >> group (align d) >> text (pString ")")

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

styleVariants :: (MonadPretty env out state m) => m () -> m ()
styleVariants aM = do
  i <- look $ indentation . options . view
  let configs =
        [ Options PreAlignStyle Buffer   i
        , Options PreAlignStyle NoBuffer i
        , Options PreSnugStyle  Buffer   i
        , Options PreSnugStyle  NoBuffer i
        , Options PostStyle     Buffer   i
        , Options PostStyle     NoBuffer i
        , Options IndentStyle   Buffer   i
        , Options IndentStyle   NoBuffer i
        ]
  forM_ configs $ \ o -> do
    hardLine
    text $ pString "##### "
    text $ pString $ show o
    text $ pString " #####"
    hardLine
    local (setL (options . view) o) aM
    hardLine
