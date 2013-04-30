module Test where

import Prelude hiding ((.))
import Control.Category
import Text.MPretty
import qualified Data.Text.Lazy.IO as T
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Util.Lens
import Data.Lens
import Control.Monad.Reader
import Util.HasLens

e1 :: [[Map String [Integer]]]
e1 =
  [ [ Map.fromList 
      [ ("foo", [1111,1112,1113,1114])
      , ("bar", [2222,2223,2224])
      , ("baz", [3333,3334])
      ]
    , Map.empty
    , Map.fromList
      [ ("foo", [4444,4443,4442,4441])
      , ("bar", [5555,5554,5553,5552,5551])
      ]
    ]
  , [ Map.empty
    , Map.fromList
      [ ("foo", [6666,6667,6668,6669])
      , ("bar", [7777,7778,7779])
      ]
    ]
  , []
  , [ Map.fromList
      [ ("foo", [8888,8889])
      , ("bar", [9999])
      , ("baz", [])
      ]
    , Map.empty
    ]
  ]

data Exp = Var String | App Exp Exp | Dollar Exp Exp | Compose Exp Exp

instance IsPretty Exp where
  pretty (Var x) = text $ pString x
  pretty (App e1 e2) =
    infixOp LeftD 10 NoBuffer (text $ pString " ") (pretty e1) (pretty e2)
  pretty (Dollar e1 e2) = 
    infixOp RightD 1 Buffer (text $ pString "$") (pretty e1) (pretty e2)
  pretty (Compose e1 e2) =
    infixOp RightD 9 Buffer (text $ pString ".") (pretty e1) (pretty e2)

e2 :: Exp
e2 = Dollar (Var "x") $ Compose (Var "y") $ Dollar (App (App (Var "l") (Var "z")) $ Var "q") $ Var "m"

main :: IO ()
main = do
  T.putStrLn $ execPretty $ pretty e1
  T.putStrLn $ execPretty $ showPretty $ pretty e1
  T.putStrLn $ execPretty $ group $ layoutWidth 5 $ pretty e2
