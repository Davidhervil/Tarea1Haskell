unzipR :: [(a,b)] -> ([a],[b])
unzipR [] = ([],[])
unzipR (p:ps) = ( fst (p):fst (unzip ps), snd (p):snd (unzip ps) )

unzipF :: [(a,b)] -> ([a],[b])
unzipF [] = ([],[])
unzipF ps = foldr func ([],[]) ps
	where 
		func (a,b) (l1,l2) = (a:l1,b:l2)

unzipM :: [(a,b)] -> ([a],[b])
unzipM [] = ([],[])
unzipM a   = (map fst a, map snd a)
