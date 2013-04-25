import Text.PrettyPrint.HughesPJ

sepEncloseBy :: String -> String -> String -> [Doc] -> Doc
sepEncloseBy lbrac rbrac del [] = text lbrac <> text rbrac
sepEncloseBy lbrac rbrac del (d:ds) = 
  sep (text lbrac <+> d : map (\d -> text del <+> d) ds)
  <+> text rbrac

tuple :: [Doc] -> Doc
tuple = sepEncloseBy "(" ")" ","

apply :: [Doc] -> Doc
apply = sepEncloseBy "(" ")" " "

letExpr :: [String] -> [String] -> [String] -> Doc
letExpr ps es bs =
  sep
  [ text "let"
    <+>
    tuple (map text ps)
    <+>
    text "="
    <+>
    tuple (map text es)
  , text "in"
    <+>
    apply (map text bs)
  ]

main :: IO ()
main = 
  putStrLn 
  $ renderStyle (Style PageMode 40 1.5)
  $ letExpr ["x", "y", "z"] ["f", "g", "h"] ["x y z"]
