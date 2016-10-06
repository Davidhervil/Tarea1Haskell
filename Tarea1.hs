unzipR :: [(a,b)] -> ([a],[b])
unzipR [] = ([],[])
unzipR (p:ps) = ( fst (p):fst (unzip ps),snd (p):snd (unzip ps) )

--unzipF :: [(a,b)] -> ([a],[b])
--unzipF [] = ([],[])


unzipM :: [(a,b)] -> ([a],[b])
unzipM [] = ([],[])
unzipM a   = (map fst a, map snd a)
