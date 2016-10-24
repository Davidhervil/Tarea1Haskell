import Data.List (sortBy)
import System.Directory
import qualified Data.Time.Clock as TCLO
import qualified Data.Time.Calendar as TCAL

-- Type
type Day   = Int
type Year  = Int
type Height  = Int
type Width   = Int

-- Data 
data Month = Enero
			| Febrero
			| Marzo
			| Abril
			| Mayo
			| Junio
			| Julio
			| Agosto
			| Septiembre
			| Octubre
			| Noviembre
			| Diciembre
	deriving (Bounded,Show,Eq,Ord,Enum,Read)
data DayName = Domingo
			| Lunes
			| Martes
			| Miercoles
			| Jueves
			| Viernes
			| Sabado
	deriving (Bounded,Show,Eq,Ord,Enum,Read)
data Picture = Picture {
					height :: Height,
					width  :: Width,
					pixels :: [[Char]]
					} 
	deriving (Show)

data Evento = Evento {
					year:: Year,
					month:: Month,
					dayZ:: Day,
					nth	:: Int,
					description :: String
			}
	deriving (Show,Read)



-- |La funcion 'pixel' devuleve un 'Picture' dado un 'Char'.
pixel :: Char -> Picture
pixel c = Picture { height = 1, width = 1, pixels = [[c]]}

-- |'above' toma dos 'Picture', p0 y p1 y genera otro 'Picture' cuyo 'pixels'
-- tendrá a p0 "arriba" de p1.


above :: Picture -> Picture -> Picture
above p0 p1  = if width p0 == width p1 then
					Picture { height = height p0 + width p1,
							 width   = width p0,
							 pixels  = pixels p0 ++ pixels p1 
							}
				else
					error "can’t ’above’ different widths"


-- |'beside' toma dos 'Picture', p0 y p1 y genera otro 'Picture'cuyo 'pixels'
-- tendrá a p0 a la izqueirda de p1.
beside :: Picture -> Picture -> Picture
beside p0 p1 = if height p0 == height p1 then
					Picture { height = height p0,
						 	  width  = width  p0 + width p1,
							  pixels = beside' (pixels p0) (pixels p1)
							}
				else 
					error "can’t ’beside’ different height"
				where 
					beside'   []      xs2  	 = xs2
					beside'   xs1      []  	 = xs1
					beside' (x:xs1) (x2:xs2) = (x ++ x2) : 
											   beside' xs1 xs2

-- |'toString' dvuelve un 'String' "listo para printear" dado un 'Picture'.
-- Notar que antes era un 'intercalate "\n" . pixels'. Lo que deja sin '\n' al
-- final.
toString :: Picture -> String
toString = concatMap ( ++ "\n") . pixels 

-- |'stack' toma una lista de 'Picture' y retorna un 'Picture' con el resultado
-- de haber apilado las "imágenes" con el primero elemento en el tope.
stack :: [Picture] -> Picture
stack = foldr1 above

-- |'spread' toma una lista de 'Picture' y retorna un 'Picture' con el resultado
-- de haber puesto las "imágenes" en el orden de la lista.
spread :: [Picture] -> Picture
spread = foldl1 beside

-- |'row' retorna un 'Picture' de altura 1 y anchura el tamaño del 'String' que
-- recibe. Su 'pixels' contendrá al 'String'.
row :: String -> Picture
row = spread . map pixel

-- |'blank' recibe un par '(Height,Width)' y devuelve un 'Picture' de esas 
-- dimensiones cuyo contenido son espacios en blanco.
--  
-- Para realizar la forma pointfree se partió de la expresion:
-- 		blank (h,w) = stack $ replicate h $ row $ replicate w ' '
-- y mediante operaciones de composición de funciones se llegó a la
-- expresión final.
blank :: (Height,Width) -> Picture
blank = uncurry ((stack .) . (. (row . flip replicate ' ') ) . replicate )


------------------------------------------------------------------------------------

-- |'stackWith' recibe un parámetro 'Height' y una lista de 'Picture' y retorna
-- un 'Picture' con el resultado de apilar las "imágenes" intercalando "blanks"
-- entre ellas con el mismo 'width' y de altura igual a la del primer parámetro.
-- 
-- Para transformar a forma point free se partió de la expresión:
--
-- 	 stackWith h ps = foldl1 above (map (above (blank (h,width (head ps)))) ps)
-- 
-- Mediante operaciones de comppsición de funciones se llegó a la expresión:
--   stackWith = (foldl1 above .) . chevrFunc . (interspersex .) . (. (width . head)) . curry blank

stackWith :: Height -> [Picture] -> Picture
--stackWith h ps = foldl1 above ( (interspersex . ( ( (. (width . head) ) . (curry blank) ) h ) ) ps ps )
--stackWith h ps = foldl1 above ( chevrFunc (interspersex . ( ( (. (width . head) ) . (curry blank) ) h ) ) ps )
stackWith = (foldl1 above .) . chevrFunc . (interspersex .) . (. (width . head)) . curry blank
	where
		-- |Esta función fue implementada para poder lograr la forma pointfree
		-- puesto hacia  falta convertir en un sólo parametro los 'ps' finales
		-- de la expresión:
		--  stackWith h ps = foldl1 above ( (interspersex . ( ( (. (width . head) ) 
		--								   . (curry blank) ) h ) ) ps ps 
		--								  )
		chevrFunc::(a -> a -> b) -> a -> b
		chevrFunc f a = f a a

		-- |Esta función es nuestra reimplementación de 'intersperse'
		-- de Data.List
		interspersex :: a -> [a] -> [a]
		interspersex a xs = tail $ foldr f [] xs
			where
				f x ls = a:(x:ls)

-- |'spreadWith' recibe un parámetro 'Width' y una lista de 'Picture' y retorna
-- un 'Picture' con el resultado de juntar de forma horizontal las "imágenes"
-- intercalando "blanks" entre ellas con el mismo 'height' y de anchura igual
-- a la del primer parámetro.
-- 
-- Para transformar a forma point free se partió de la expresión:
--
-- 	spreadWith w ps = foldl1 beside  (interspersex ( blank ( (height . head) ps , w) ) ps ) 
-- 
-- Mediante operaciones de comppsición de funciones se llegó a la expresión final.
spreadWith :: Width -> [Picture] -> Picture
spreadWith = (foldl1 beside .) . chevrFunc . (interspersex .) . (. (height . head)) . curry blank
	where
		-- |Esta función fue implementada para poder lograr la forma pointfree
		-- puesto hacia  falta convertir en un sólo parametro los ps de la
		-- expresión análoga en stackWith
		chevrFunc::(a -> a -> b) -> a -> b
		chevrFunc f a = f a a

		-- |Esta función es nuestra reimplementación de 'intersperse'
		-- de Data.List
		interspersex :: a -> [a] -> [a]
		interspersex a xs = tail $ foldr f [] xs
			where
				f x ls = a : (x : ls)

-- |'tile' toma una mtariz de de 'Picture' y retorna un 'Picture' 
-- con el resultado de unir todas las imágenes de la matriz.
tile :: [[Picture]] -> Picture
tile = stack . (map spread)

-- |'tileWith' recibe un par de altura y ancho junto con una matriz de imágenes
-- y retorna un 'Picture' con el resultado de aplicar la union de las imágenes
-- de la matriz intercalando horizontalmente "blanks" de ancho 'w' y entre las
-- filas "blanks" de altura 'h'.
tileWith :: (Height, Width) -> [[Picture]] -> Picture
--tileWith (h,w) pss = stackWith h (map (spreadWith w) pss)
tileWith = uncurry ((. (map . spreadWith)) . (.) . stackWith)

-----------------------------------------------------------------------------------
rjustify :: Int -> String -> String
rjustify n s = 	if n >= (length s) then
					(replicate (n - length s ) ' ') ++ s
				else
					error "Tamaño final menor al texto"

-- |
dnames:: Picture
dnames = beside (pixel ' ') $ spreadWith 1 $ map (row.(take 2).show.cast) [0..6]
	where
		cast i = toEnum i :: DayName

-- |
banner :: Month -> Year -> Picture
banner m y   = row (rjustify (width dnames ) (show m ++ (' ':show y)))

-- |
heading :: Month -> Year -> Picture
heading m y = banner m y `above` dnames

-- | holas
leap :: Year -> Bool
leap y = if (mod y 100) == 0 then 
			(mod y 400) == 0 
		 else 
			(mod y 4)   == 0

-- |
mlengths  :: Year -> [Day]
mlengths y = [31, a, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	where a = if leap y then 29 else 28

-- |
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs
  | n > 0 	  = (take n xs) : (group n (drop n xs))
  | otherwise = error "N no positivo"
-- |
jan1 :: Year -> DayName
jan1 y = toEnum  (mod
					(
					1                 +
					((y-1) * 365)     +
			 		div (y - 1) 4     +
			 		div (y - 1) 400   -
			 		div (y - 1) 100 )
			 		7)

-- |
mtotals :: Year -> [Int]
mtotals y = scanl  (+) (fromEnum (jan1 y)) b
			where b = mlengths y

-- |
fstdays :: Year -> [DayName]
fstdays y = map toEnum ( map (`mod` 7) ((init . mtotals )y))
-- |

fstday :: Month -> Year -> DayName
fstday m y = (fstdays y ) !! (fromEnum m)
-- |

day :: Day -> Month -> Year -> DayName
day d m y = toEnum $ ( mod ((fromEnum (fstday m y)) + (fromEnum d) + 6 ) 7 )

-- |
miord :: Evento -> Evento -> Ordering
miord e1 e2 = if a < b then LT
			  else
			  	if a > b then GT
			  	else
			  		EQ
		where 
			a = (year e1, month e1, dayZ e1)
			b = (year e2, month e2, dayZ e2)

-- |
loadEvents :: FilePath -> IO [Evento]
loadEvents pathName = do 
	s <-readFile pathName
	return ( sortBy miord (((map castlE).lines) s))
	where
		castlE s = read s :: Evento
-- |
saveEvents :: FilePath -> [Evento] -> IO ()
saveEvents path es = writeFile path $ unlines $ map show $ sortBy miord es
-- |
eventsOnYear :: [Evento] -> Year -> [Evento]
eventsOnYear es y = filter (\e -> year e == y) es

-- |
eventsOnMonth :: [Evento] -> Month -> [Day]
eventsOnMonth es m = foldl acum [] $ map dayZ $ filter (\e -> month e == m) es
	where 
		acum xs x = if not (elem x xs) then x:xs
					else xs
-- |
pix :: DayName -> Day -> [Day] -> [Picture]
pix d s ms = (replicate (fromEnum d) (row "   ") )++
			 (map ache [1..s]) ++
			 (replicate ( 6 - (mod ( (fromEnum d)+s + 6) 7 ) ) (row "   ") )
			 where
			 	ache n = if elem n ms then
			 							if n < 10 then row ( "> " ++ (show n))
			 							else row (">" ++ (show n))
			 			 else
			 			 	if n < 10 then row ("  " ++ (show n))
			 							else row (" " ++ (show n))
-- |
entries :: DayName -> Day -> [Day] -> Picture
entries dn d ds = tile $ group 7 $ pix dn d ds
-- |
picture :: (Month,Year,DayName,Day,[Day]) -> Picture
picture (m,y,d,s,ms) = heading m y `above` entries d s ms

-- |
helpPls :: IO ()
helpPls = putStr x
	where	x = "\nTeclas validas:\n\
				\  j: retroceder un dia\n\
				\  k: avanzar un dia\n\
				\  h: retroceder un mes\n\
				\  l: avanzar un mes\n\
				\  r: registrar un evento\n\
				\  d: eliminar un evento\n\
				\  q: salir\n \n \n"


-- |
currentDate :: IO (Year, Month, Day)
currentDate = do
	a <- TCLO.getCurrentTime
	let (p,q,r) = TCAL.toGregorian ( TCLO.utctDay a)
	let  x 		= read (show p) :: Year
	let    y 	= toEnum q 		:: Month
	let 	 z 	= read (show r) :: Day
	return ( x , toEnum (mod (q+11) 12) ,z)
-- |
move :: Char -> (Year, Month, Day) -> (Year,Month,Day)
move op (y,m,d)
	| op == 'h' = 
		if (prevM m > m) then
			(y-1,prevM m, d)
			else 
			(y, prevM m, mlengths y !! (prevM m))
	| op == 'l'	= 		
		if (nextM m < m) then 
			(y+1, nextM m, d)
			else
				(y,nextM m, mlengths y !! (nextM m))
	| op == 'j' = 		
		if (d-1 < 1) then
			move 'h' (y,m,mlengths y !! (fromEnum m))
			else 
				(y,m,d-1)
	| otherwise = 		
		if d+1 > mlengths y !! (fromEnum m) 
			then
				if fromEnum m /= 11 then
					(y,nextM m, 1)
					else	
						(y+1, nextM m,1)
				else 
				(y,m,d+1)
	where
		nextM m = toEnum $ mod (fromEnum m + 1) 12
		prevM m = toEnum $ mod (fromEnum m + 11) 12

-- |
prompt :: (Year, Month, Day) -> IO()
prompt (y,m,d) = putStr (show y 				 	++ 
			     (' ': show m) 	++
				 (' ':show d)						++
				 "> ")

-- |
descPrompt :: (Year, Month, Day) -> Int -> [Evento] -> [Char] -> [Evento]
descPrompt (y,m,d) n list msj= list ++ [e] 
				where e = Evento {
							year  = y,
							month = m,
							dayZ  = d,
							nth	  = n,
							description = msj}
-- |
clearS :: IO ()
clearS = putStr (replicate 24 '\n')
-- |
removePrompt :: (Year, Month, Day) -> [Evento] -> Int -> [Evento]
removePrompt (y,m,d) list r  = filter (anotherD) list
	where
		anotherD a = (y,m,d) /= (year a, month a,dayZ a) ||
				 r /= nth a
-- |
getMax4Day :: (Year, Month, Day) -> [Evento] -> Int
getMax4Day (y,m,d) []   = 0
getMax4Day (y,m,d) list = maximum $ map nth (filter (sameD) (list))
	where sameD a = (y,m,d) == (year a, month a,dayZ a)
-- |
getInt :: IO Int
getInt = do str <- getLine
            return (read str)
-- |
actual :: (Year,Month,Day) -> [Evento] -> Picture
actual (y,m,d) lista = picture(m,y,fstday m y,(mlengths y) !! fromEnum m, eventsOnMonth (eventsOnYear lista y) m)
-- |
todayEvents :: (Year,Month,Day) -> [Evento] -> [Evento]
todayEvents (y,m,d) []   =  []
todayEvents (y,m,d) list = filter sameD list 
	where sameD a = (y,m,d) == (year a, month a,dayZ a)

-- |
todayAgenda:: [Evento] -> IO ()
todayAgenda [] = do putStrLn ""
todayAgenda (e:es) = do
    putStrLn $ "Evento Nro: " ++ (show $ nth e)
    putStrLn $ "Descripción " ++ (description e)
    todayAgenda es
-- |
mostrarHoy :: [Evento] -> IO ()
mostrarHoy [] = do putStrLn "Por ahora no tiene eventos hoy."
mostrarHoy es = do putStrLn "Eventos de hoy: " >> todayAgenda es    
-- |
hacer :: [Evento] -> (Year, Month, Day) -> IO ()
hacer list (y,m,d)= do
	-- teclas para movimientos y eventos
	let moves = ['j','k','l','h']
	let evnts = ['d','r']
	let calActual = actual (y,m,d) list
	let dehoy = todayEvents (y,m,d) list
	putStr $ toString calActual
	-- Mostrar fecha actual
	mostrarHoy dehoy
	prompt (y,m,d)
	-- Solicitar entrada del Usuario
	s <- getChar
	-- Colocar un salto de linea
	putStrLn ""
	-- Si el elemento esta en movimiento
	if elem s moves then
		hacer list (move s (y,m,d))
	else
		-- registrar
		if s == 'r' then do
			putStr "descr: "
			msj <- getLine
			let n =  getMax4Day (y,m,d) list
			let list' = descPrompt (y,m,d) (n+1) list msj
			hacer list' (y,m,d)
		else
			if s == 'd' then do
					i <- getInt
					let list' = removePrompt (y,m,d) list i
					hacer list' (y,m,d)
			else
				if s == 'q' then
					saveEvents "hagenda.txt" list
				else
					helpPls >> hacer list (y,m,d)


main = do
		c <- currentDate
		exists <- doesFileExist "hagenda.txt"
 		if exists then do
			list <- loadEvents "hagenda.txt"
			hacer list c
		else 
			hacer [] c


