--|'unzipR' implementación recursiva de 'unzip'
unzipR :: [(a,b)] -> ([a],[b])
unzipR [] = ([],[])
unzipR (p:ps) = ( fst (p):fst (unzip ps), snd (p):snd (unzip ps) )

--|'unzipF' implementación de 'unzip' utilizando 'fold'
unzipF :: [(a,b)] -> ([a],[b])
unzipF [] = ([],[])
unzipF ps = foldr func ([],[]) ps
	where 
		func (a,b) (l1,l2) = (a:l1,b:l2)

--|'unzipM' implementación de 'unzip' utilizando 'map'
unzipM :: [(a,b)] -> ([a],[b])
unzipM [] = ([],[])
unzipM a   = (map fst a, map snd a)

--|'unzipL' implementación de 'unzip' usando listas por comprensión
unzipL :: [(a,b)] -> ([a],[b])
unzipL [] = ([],[])
unzipL ps = ([ fst p | p <-ps ],[ snd p | p <-ps ])
