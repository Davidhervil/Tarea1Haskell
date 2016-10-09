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
suffixes :: [a] -> [[a]]
suffixes s = scanr (:) [] s

isSubstring :: String -> String -> Bool
isSubstring s1 s2 = any (isPrefix s1) (suffixes s2)

findSubstrings :: String -> String -> [Int]
findSubstrings s1 s2 = findIndices True $ map (isPrefix s1) suffixes s2

--Segunda Parte
getIndices :: SuffixTree -> [Int]
getIndices (Leaf a) = [a]
getIndices (Node st )= foldl irHoja [] st
						where
							irHoja l (_,Leaf x) = x:l
							irHoja l (_,Node t) = foldl irHoja l t
