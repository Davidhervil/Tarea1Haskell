data SuffixTree = Leaf Int
				| Node [(String, SuffixTree)]
				deriving(Eq,Ord,Show)

isPrefix ::  String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (p:ps) (s:ss) = p == s && (isPrefix ps ss)
