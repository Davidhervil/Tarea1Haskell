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
findSubstrings s1 s2 = [n | (n,x) <-zip [1..] (map (isPrefix s1) (suffixes s2)), x] 

--Segunda Parte
getIndices :: SuffixTree -> [Int]
getIndices (Leaf a) = [a]
getIndices (Node st )= foldl irHoja [] st
						where
							irHoja l (_,Leaf x) = x:l
							irHoja l (_,Node t) = foldl irHoja l t

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf _)  = []
findSubstrings' s (Node st) 
				| st == [] = []
				| isPrefix s a = getIndices nodo 
				| isPrefix a s = findSubstrings' (removePrefix a s) nodo
				| otherwise = findSubstrings' s $ Node $ tail st
				where
					a = fst $ head st
					nodo = snd $ head st

bananatree = Node [("banana", Leaf 0),
					("a", Node [("na", Node [("na", Leaf 1),
								("", Leaf 3)]),
								("", Leaf 5)]),
					("na", Node [("na", Leaf 2),
					("", Leaf 4)])]
