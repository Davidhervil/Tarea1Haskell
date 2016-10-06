unzipR:: [(a,b)] -> ([a],[b])
unzipR [] = ([],[])
unzipR (p:ps) = ( fst (p):fst (unzip ps),snd (p):snd (unzip ps) )

unzipF:: [(a,b)] -> ([a],[b])
