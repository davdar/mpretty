import Text.MPretty
import qualified Data.Text.Lazy.IO as T
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

e1 :: [[Map String Integer]]
e1 =
  [ [ Map.fromList 
      [ ("foo", 1111)
      , ("bar", 2222)
      , ("baz", 3333)
      ]
    , Map.empty
    , Map.fromList
      [ ("foo", 4444)
      , ("bar", 5555)
      ]
    ]
  , [ Map.empty
    , Map.fromList
      [ ("foo", 6666)
      , ("bar", 7777)
      ]
    ]
  , []
  , [ Map.fromList
      [ ("foo", 8888)
      , ("bar", 9999)
      ]
    , Map.empty
    ]
  ]

main :: IO ()
main = T.putStrLn $ execPretty $ pretty e1
