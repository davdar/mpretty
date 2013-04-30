{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Util.Pty where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Data.Functor
import Control.Arrow
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Data.Maybe

data Doc =
    EmptyD
  | ConcatD Doc Doc
  -- (TextD n s) where n = length s
  | TextD String Int
  | NestD Int Doc
  -- (LineD n s) where n = length s 
  -- renders to either a linebreak or to s
  | LineD String Int
  | GroupD Doc
  | NestingD (Int -> Doc)
  | ColumnD (Int -> Doc)

type NormDoc = [NormDocElem]
data NormDocElem =
    TextND Int String Int -- (nest level, string, string len)
  | LineND Int String Int -- (nest level, string, string len)
  | GroupND NormDoc
  | ColumnND (Int -> NormDoc)

type TinyDoc = [TinyDocElem]
data TinyDocElem =
    TextTD String
  | LineTD Int

data FitMode =
    FlatFM
  | BreakFM

normalize :: Int      -- ^ current nest level
          -> Doc      -- ^ the doc to normalize
          -> NormDoc  -- ^ the tail of the computation
          -> NormDoc  -- ^ the final result
normalize i d tl = case d of
  EmptyD -> tl
  ConcatD dl dr -> normalize i dl $ normalize i dr tl 
  TextD s slen -> TextND i s slen : tl
  NestD j dn -> normalize (i + j) dn tl
  LineD s slen -> LineND i s slen : tl
  GroupD dg -> GroupND (normalize i dg []) : tl
  NestingD f -> normalize i (f i) tl
  ColumnD f -> ColumnND (\k -> normalize i (f k) []) : tl

fits :: Int                    -- ^ max column width (positive)
     -> Int                    -- ^ current cursor column (positive)
     -> [(Int, FitMode, Doc)]  -- ^ triples (nest level (positive), fit mode, doc)
     -> Bool                   -- ^ whether or not it fits
fits w k ps 
  | k > w = False
  | otherwise = case ps of
      [] -> True
      p : ps -> case p of
        -- continue
        (_, _, EmptyD) -> fits w k ps
        -- continue, maintaining nesting and mode
        (i, m, ConcatD dl dr) -> fits w k $ (i, m, dl) : (i, m, dr) : ps
        -- continue, adding nesting
        (i, m, NestD j dn) -> fits w k $ (i + j, m, dn) : ps
        -- continue, shortening fit width
        (i, m, TextD s slen) -> fits w (k + slen) ps
        -- continue, shortening fit width
        (i, FlatFM, LineD s slen) -> fits w (k + slen) ps
        -- succeed: the doc had no breaks, and this is after the doc in
        -- question, which breaks, making a successful fit
        (i, BreakFM, LineD _ _) -> True
        -- unroll all groups in question
        (i, m, GroupD dg) -> fits w k $ (i, FlatFM, dg) : ps
        -- give the current nesting
        (i, m, NestingD f) ->
          fits w k $ (i, m, f i) : ps
        -- give the current column
        (i, m, ColumnD f) ->
          fits w k $ (i, m, f k) : ps

format :: Int                    -- ^ max column width (positive)
       -> Int                    -- ^ current cursor column (positive)
       -> [(Int, FitMode, Doc)]  -- ^ triples (nest level (positive), fit mode, doc)
       -> TinyDoc                -- ^ a fixed layout document
format w k ps = case ps of
  [] -> []
  p : ps -> case p of
    -- continue
    (i, m, EmptyD) -> format w k ps
    -- continue, maintaining nesting and mode
    (i, m, ConcatD dl dr) -> format w k $ (i, m, dl) : (i, m, dr) : ps
    -- continue, adding nesting
    (i, m, NestD j dn) -> format w k $ (i + j, m, dn) : ps
    -- emit text, moving cursor
    (i, m, TextD s slen) -> TextTD s : format w (k + slen) ps
    -- emit text, moving cursor
    (i, FlatFM, LineD s slen) -> TextTD s : format w (k + slen) ps
    -- emit a line break, maintaining nesting
    (i, BreakFM, LineD _ _) -> LineTD i : format w i ps
    -- layout flat if possible, otherwise break
    (i, m, GroupD dg) ->
      let mode = if fits w k $ (i, FlatFM, dg) : ps
            then FlatFM
            else BreakFM
      in format w k $ (i, mode, dg) : ps
    -- give the current nesting
    (i, m, NestingD f) ->
      format w k $ (i, m, f i) : ps
    -- give the current column
    (i, m, ColumnD f) ->
      format w k $ (i, m, f k) : ps

type FormatMonad m = 
  ( Functor m
  , MonadReader (Int, FitMode, Bool) m
  , MonadState Int m
  , MonadWriter (TinyDoc -> TinyDoc) m
  , MonadPlus m
  )

type M a = 
  ReaderT (Int, FitMode, Bool) 
    (StateT Int 
      (WriterT (TinyDoc -> TinyDoc) 
        (MaybeT Identity))) 
    a

runM :: M a -> Int -> FitMode -> Int -> Maybe (a, Int, TinyDoc)
runM aM w mode k =
  fmap (unTuple . second ($ []))
  $ runIdentity
  $ runMaybeT
  $ runWriterT
  $ flip runStateT k
  $ flip runReaderT (w,mode,False)
  $ aM
  where
    unTuple ((x,y),z) = (x,y,z)

tellCons :: (MonadWriter ([x] -> [x]) m) => x -> m ()
tellCons x = tell (x:)

tellText :: (FormatMonad m) => String -> Int -> m ()
tellText s slen = do
  tellCons $ TextTD s
  modify (+ slen)

tellLine :: (FormatMonad m) => Int -> m ()
tellLine i = do
  tellCons $ LineTD i
  put i

askW :: (FormatMonad m) => m Int
askW = (\(a,_,_) -> a) <$> ask

askMode :: (FormatMonad m) => m FitMode
askMode = (\(_,b,_) -> b) <$> ask

localMode :: (FormatMonad m) => (FitMode -> FitMode) -> m a -> m a
localMode f = local (\(a,b,c) -> (a,f b,c))

askTrying :: (FormatMonad m) => m Bool
askTrying = (\(_,_,c) -> c) <$> ask

localTrying :: (FormatMonad m) => (Bool -> Bool) -> m a -> m a
localTrying f = local (\(a,b,c) -> (a,b,f c))

tryLayout :: (FormatMonad m) => m a -> m a
tryLayout = localTrying (const True)

format' :: (FormatMonad m) => NormDoc -> m ()
format' pss = do
  trying <- askTrying
  when trying $ do
    w <- askW
    k <- get
    guard (k < w)
  case pss of
    [] -> return ()
    p : ps -> case p of
      TextND i s slen -> do
        tellText s slen
        format' ps
      LineND i s slen -> do
        mode <- askMode
        case mode of
          FlatFM -> do
            tellText s slen
            format' ps
          BreakFM -> do
            tellLine i
            format' ps
      GroupND nd -> msum
        [ tryLayout $ do
            localMode (const FlatFM) $
              format' nd
            format' ps
        , do
            localMode (const BreakFM) $
              format' nd 
            format' ps
        ]
      ColumnND f -> do
        format' . f =<< get
        format' ps

format'' :: (FormatMonad m) => Doc -> m ()
format'' i d = do
  trying <- askTrying
  when trying $ do
    w <- askW
    k <- get
    guard $ k < w
  case d of
    EmptyD -> return ()
    ConcatD dl dr -> do
      format'' i dl
      format'' i dr
    TextD s slen -> do
      tellText s slen
    NestD j dn -> do
      format'' (i + j) dn
    LineD s slen -> do
      mode <- askMode
      case mode of
        FlatFM -> do
          tellText s slen
        BreakFM -> do
          tellLine i
    GroupD dg -> msum
      [ do
          putTrying True
          localMode (const FlatFM) $
            format' i dg
      , localMode (const BreakFM) $
          format' i dg 
      ]
    NestingD f -> 
      format'' $ f i
    ColumnD f ->
      format'' . f =<< get

runFormat :: Int -> Doc -> TinyDoc
runFormat w d = format w 0 [(0,FlatFM,d)]

runFormat' :: Int -> Doc -> TinyDoc
runFormat' w d = 
  let (_, _, td) = fromJust $ runM (format' (normalize 0 d [])) w FlatFM 0
  in td

layout :: TinyDoc -> ShowS
layout [] = id
layout (TextTD s:td) = showString s . layout td
layout (LineTD i:td) = showString ('\n':replicate i ' ') . layout td

empty = EmptyD
text s = TextD s (length s)
line s = LineD s (length s)
(%+%) = ConcatD
(%++%) x y = x %+% text " " %+% y
(%/%) x y = x %+% line "" %+% y
(%//%) x y = x %+% line " " %+% y
column = ColumnD
nesting = NestingD
group = GroupD

fold _ [] = empty
fold _ [x] = x
fold f (x:xs) = x `f` fold f xs

vsep = fold (%//%)
hsep = fold (%++%)
nest = NestD
align x = column $ \ k -> nesting $ \ i -> nest (k - i) x

prettyList :: (a -> Doc) -> [a] -> Doc
prettyList _ [] = text "[]"
prettyList prettyA (x:xs) =
  group $ vsep $ concat
  [ [ hsep 
      [ text "["
      , align $ prettyA x
      ]
    ]
  , flip map xs $ \x' -> 
      hsep 
      [ text ","
      , align $ prettyA x'
      ]
  , [ text "]" ]
  ]

prettyInt :: Integer -> Doc
prettyInt = text . show

thing :: [[[Integer]]]
thing =
  [ [ [ 1
      , 2
      , 3
      ]
    , []
    , [ 4
      , 5
      ]
    ]
  , [ []
    , [ 6
      , 7
      ]
    ]
  , []
  , [ [ 8
      , 9
      ]
    , []
    ]
  ]

prettyThing :: Doc
prettyThing = (prettyList $ prettyList $ prettyList prettyInt) thing

main :: IO ()
main = 
  putStrLn 
  $ ($[]) 
  $ layout 
  $ runFormat' 20 prettyThing
