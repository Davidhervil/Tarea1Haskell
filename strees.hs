data SuffixTree = Leaf Int
				| Node [(String, SuffixTree)]
				deriving(Eq,Ord,Show)

isPrefix ::  String -> String -> Bool
isPrefix   []      _   = True
isPrefix    _     []   = False
isPrefix (p:ps) (s:ss) = p == s && (isPrefix ps ss)

removePrefix :: String -> String -> String
removePrefix p s =  if isPrefix p s then drop (length p) s 
					else s

suffixes :: [a] -> [[a]]
suffixes s = scanr (:) [] s

isSubstring :: String -> String -> Bool
isSubstring s1 s2 = any (isPrefix s1) (suffixes s2)
	
findSubstrings :: String -> String -> [Int]
findSubstrings s1 s2 = [ n | (n,x) <-zip [0..] (map (isPrefix s1) (suffixes s2)), x ] 

--Segunda Parte
getIndices :: SuffixTree -> [Int]
getIndices (Leaf a) = [a]
getIndices (Node st )= foldl obtHoja [] st
						where
							obtHoja l (_,Leaf x) = x:l
							obtHoja l (_,Node t) = foldl obtHoja l t

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

insert :: (String,Int) -> SuffixTree -> SuffixTree
insert (s,i) (Leaf _) = error "Agregando en hojas"
insert (s,i) (Node a) = if mismoPre == [] then Node ((s,Leaf i):a)
						else
							if (isLeaf.snd) found then 
								Node (map replace a)
							else
								if resto /= [] then	
									Node (map push a)
								else 
									Node (map seguir a)
	where
		comunes [] _ = []
		comunes _ [] = []
		comunes (s1:ss1) (s2:ss2) = if s1 == s2 then s1:(comunes ss1 ss2)
									else []
		isLeaf (_,Leaf _) = True
		isLeaf   (_,_)    = False
		mismoPre = [ n | n <- a, fst n /= "",(head.fst) n == head s ]
		found = head mismoPre
		comun = comunes s (fst found)
		diff = removePrefix (comunes s (fst found)) s
		resto = removePrefix (comunes s (fst found)) (fst found)
		replace e = if found == e then (comun,Node [(resto,snd found),(diff, Leaf i)])
					else e
		push e = if found == e then (comun,Node[(resto,snd found),(diff,Leaf i)])
				 else e
		seguir e = if e == found then (fst e,insert (diff,i) (snd found))
				   else e
buildTree :: String -> SuffixTree
buildTree s = foldl (flip insert) (Node []) (reverse (zip (suffixes s) [0..]))