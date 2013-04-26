import Text.MPretty
import qualified Data.Text.Lazy.IO as T
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

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

main :: IO ()
main = T.putStrLn $ execPretty $ pretty e1
