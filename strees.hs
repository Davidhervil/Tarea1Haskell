data SuffixTree = Leaf Int
				| Node [(String, SuffixTree)]
				deriving(Eq,Ord,Show)

isPrefix ::  String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (p:ps) (s:ss) = p == s && (isPrefix ps ss)

removePrefix :: String -> String -> String
removePrefix p s = if isPrefix p s then drop (length p) s
					else s
