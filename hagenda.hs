import Data.List (sortBy)
import System.Directory
import qualified Data.Time.Clock as TCLO
import qualified Data.Time.Calendar as TCAL
import System.IO

-- Importamos System.Directory para saber si existe "hagenda.txt"
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
spreadWith = (\w ps -> foldl1 beside  (intersperseX ( blank ( (height . head) ps , w) ) ps )) 
	where
		-- |Esta función es nuestra reimplementación de 'intersperse'
		-- de Data.List
		intersperseX :: a -> [a] -> [a]
		intersperseX a xs = tail $ foldr f [] xs
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

-- | 'rjustify' recibe un Int y un String para retornar un String del tamaño 
-- especificado
rjustify :: Int -> String -> String
rjustify n s = 	if n >= (length s) then
					(replicate (n - length s ) ' ') ++ s
				else
					error "Tamaño final menor al texto"

-- | 'dnames' produce un 'Picture' con los nombres de los dias
dnames:: Picture
dnames = beside (pixel ' ') $ spreadWith 1 $ map (row.(take 2).show.cast) [0..6]
	where
		cast i = toEnum i :: DayName

-- | 'banner' recibe un mes y un año para producir un 'Picture' alineado
-- a la derecha
banner :: Month -> Year -> Picture
banner m y   = row (rjustify (width dnames ) (show m ++ (' ':show y)))

-- | 'heading' recibe un mes y un año para retornar un 'Picture' con el
-- membrete del calendario
heading :: Month -> Year -> Picture
heading m y = banner m y `above` dnames

-- | 'leap' recibe un año y retorna True si es bisiesto (False en caso contrario).
-- Para ello se utiliza una fórmula cerrada
leap :: Year -> Bool
leap y = if (mod y 100) == 0 then 
			(mod y 400) == 0 
		 else 
			(mod y 4)   == 0

-- | 'mlengths' recibe un año y retorna una lista de enteros con el número de
-- días de cada uno de los meses del año en cuestión.
mlengths  :: Year -> [Day]
mlengths y = [31, a, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	where a = if leap y then 29 else 28

-- | 'group' recibe un 'Int' y una lista de elementos y retorna una lista de listas
-- con los elementos agrupados según el nñumero especificado. Los elementos son
-- agrupados en orden 
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs
  | n > 0 	  = (take n xs) : (group n (drop n xs))
  | otherwise = error "N no positivo"

-- | 'jan1' recibe un 'Year' y retorna el día correspondiente al primero de Enero
-- de ese año.
jan1 :: Year -> DayName
jan1 y = toEnum  (mod
					(
					1                 +
					((y-1) * 365)     +
			 		div (y - 1) 4     +
			 		div (y - 1) 400   -
			 		div (y - 1) 100 )
			 		7)

-- | 'mtotals' recibe un 'Year' y retorna una lista de enteros con 
mtotals :: Year -> [Int]
mtotals y = scanl  (+) (fromEnum (jan1 y)) b
			where b = mlengths y

-- | 'fstdays' recibe un 'Year' y retorna una lista con el primer día de cada mes
-- correspondiente al año especificado.
fstdays :: Year -> [DayName]
fstdays y = map toEnum ( map (`mod` 7) ((init . mtotals )y))

-- | 'fstday' recibe un 'Month' y un 'Year' y retorna el primer día del mes,
-- en el año especificado.
fstday :: Month -> Year -> DayName
fstday m y = (fstdays y ) !! (fromEnum m)

-- | 'day' recibe un dia, un mes y un año y retorna a que día corresponde.
day :: Day -> Month -> Year -> DayName
day d m y = toEnum $ ( mod ((fromEnum (fstday m y)) + (fromEnum d) + 6 ) 7 )

-- | 'miord' recibe dos eventos y determina cuál es mas antiguo
miord :: Evento -> Evento -> Ordering
miord e1 e2 = if a < b then LT
			  else 
			  	if a > b then GT
			  	else EQ
		where 
			a = (year e1, month e1, dayZ e1, nth e1)
			b = (year e2, month e2, dayZ e2, nth e2)

-- | 'loadEvents' recibe la dirección de un archivo y retorna un monad IO con una
-- lista de 'Evento'. La lista está ordenada por antiguedad.
loadEvents :: FilePath -> IO [Evento]
loadEvents pathName = do 
	s <-readFile pathName
	return ( sortBy miord (((map castlE) . lines) s))
	where
		castlE s = read s :: Evento

-- | 'saveEvents' recibe la dirección de un archivo y una lista de Eventos escribirlos en
-- el archivo especificado.
saveEvents :: FilePath -> [Evento] -> IO ()
saveEvents path es = writeFile path $ unlines $ map show $ sortBy miord es

-- | 'eventsOnYear' recibe una lista de Eventos, un Año y devuelve una lista de los eventos
-- para el año en cuestión.
eventsOnYear :: [Evento] -> Year -> [Evento]
eventsOnYear es y  = filter (\e -> year e == y) es

-- | 'eventsOnMonth' recibe una lista de Eventos y un mes para retornar una lista de los días
-- del mes en los ue hay eventos planificados.
eventsOnMonth :: [Evento] -> Month -> [Day]
eventsOnMonth es m = foldl acum [] $ map dayZ $ filter (\e -> month e == m) es
	where 
		acum xs x  = if not (elem x xs) then x : xs
					 else xs

-- | 'pix' construye la lista de «imágenes» correspondiente a un
-- mes particular, marcando aquellos días que tengan al menos un evento registrado
pix :: DayName -> Day -> [Day] -> [Picture]
pix d s ms = (replicate (fromEnum d) (row "   ") ) ++
			 (map ache [1..s]) ++
			 (replicate ( 6 - (mod ( (fromEnum d) + s + 6) 7 ) ) (row "   ") )
			 where
			 	ache n = if elem n ms then
			 				if n < 10 then
			 					row ( "> " ++ (show n))
			 				else 
			 					row ( ">"  ++ (show n))
			 			 else
			 			 	if n < 10 then 
			 			 		row ( "  " ++ (show n))
			 				else 
			 					row ( " "  ++ (show n))

-- | 'entries' recibe un 'DayName', un 'Day' y una lista de dias para
-- producir una 'Picture' cprrespondiente a la matriz de dias del calen
-- dario. 
entries :: DayName -> Day -> [Day] -> Picture
entries dn d ds 	 = tile $ group 7 $ pix dn d ds

-- | 'picture' construye la 'Picture' completa con el calendario.
picture :: (Month,Year,DayName,Day,[Day]) -> Picture
picture (m,y,d,s,ms) = heading m y `above` entries d s ms

-- | 'helpPls' muestra las opciones validas al usuario por el terminal
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


-- | 'currentDate' retorna un monad IO que contiene la fecha actual.
currentDate :: IO (Year, Month, Day)
currentDate = do
	a 		   <- TCLO.getCurrentTime
	let (p,q,r) = TCAL.toGregorian (TCLO.utctDay a)
	let  x 		= read (show p) :: Year
	let    y 	= toEnum q 		:: Month
	let 	 z 	= read (show r) :: Day
	return (x , toEnum (mod (q + 11) 12) , z)

-- | 'move' recibe un Caracter y una fecha y produce una nueva fecha
-- en función de la opción especificada por el usuario.
move :: Char -> (Year, Month, Day) -> (Year,Month,Day)
move op (y,m,d)
	| op == 'h' = 
		if (prevM m > m) then
			(y-1, prevM m, d)
			else 
			(y, prevM m, mlengths y !! (prevM m))
	| op == 'l'	= 		
		if (nextM m < m) then 
			(y+1, nextM m, d)
			else
				(y, nextM m, mlengths y !! (nextM m))
	| op == 'j' = 		
		if (d-1 < 1) then
			move 'h' (y, m, mlengths y !! (fromEnum m))
			else 
				(y, m, d-1)
	| otherwise = 		
		if d+1 > mlengths y !! (fromEnum m) 
			then
				if fromEnum m /= 11 then
					(y, nextM m, 1)
					else	
						(y+1, nextM m,1)
				else 
				(y, m, d+1)
	where
		nextM m = toEnum $ mod (fromEnum m + 1) 12
		prevM m = toEnum $ mod (fromEnum m + 11) 12

-- | 'prompt' recibe una fecha y muestra un prompt para indicarle al usuario
-- la fecha en la que se encuentra trabajando.
prompt :: (Year, Month, Day) -> IO()
prompt (y,m,d) = putStr (	   show y   ++ 
			     		(' ' : show m) 	++
				 		(' ' : show d)	++
				 		"> ")

-- | 'descPrompt' recibe una fecha, un entero, un arreglo de eventos y uno de caracteres
-- para retornar una lista actualizada con un nuevo evento.
descPrompt :: (Year, Month, Day) -> Int -> [Evento] -> [Char] -> [Evento]
descPrompt (y, m, d) n list msj  = list ++ [e] 
	where e = Evento {
				year  		= y,
				month 		= m,
				dayZ  		= d,
				nth	  		= n,
				description = msj
			  }

-- | 'clearS' limpia la pantalla produciendo 24 lineas en blanco
clearS :: IO ()
clearS = putStr (replicate 24 '\n')

-- | 'removePrompt' recibe una fecha, una lista de eventos y un entero y retorna
-- una nueva lista de Eventos en la que no está el Evento del número especificado.
removePrompt :: (Year, Month, Day) -> [Evento] -> Int -> [Evento]
removePrompt (y, m, d) list r  = filter (anotherD) list
	where
		anotherD a  = (y,m,d) /= (year a, month a,dayZ a) ||
				 	  	 r    /=  nth  a

-- | 'getMax4Day' recibe una fecha, una lista de eventos y retorna el nayor número de evento.
getMax4Day :: (Year, Month, Day) -> [Evento] -> Int
getMax4Day (y, m, d) []      = 0
getMax4Day (y, m, d) list    = maximum $ map nth (filter sameD list)
	where sameD a = (y,m,d) == (year a, month a, dayZ a)

-- | 'getInt' lee un entero proporcionado por el usuario y retorna un monad IO Int
getInt :: IO Int
getInt = do str <- getLine
            return (read str)

-- | 'actual' recibe una fecha, una lista de eventos y devuelve un 'Picture'
-- con el calendario construido.
actual :: (Year,Month,Day) -> [Evento] -> Picture
actual (y,m,d) lista = picture (m, y, fstday m y, 
					   		   (mlengths y) !! fromEnum m,
					   		   eventsOnMonth (eventsOnYear lista y) 
					  		   m)

-- | 'todayEvents' recibe una fecha, una lista de eventos y produce una lista con los eventos
-- correspondientes a la fecha especificada.
todayEvents :: (Year,Month,Day) -> [Evento] -> [Evento]
todayEvents (y,m,d) []  	 = []
todayEvents (y,m,d) list 	 = filter sameD list 
	where sameD a = (y,m,d) == (year a, month a,dayZ a)

-- | 'todayAgenda' recibe una lista de Eventos y los muestra al usuario.
todayAgenda :: [Evento] -> IO ()
todayAgenda [] 	   = do putStrLn ""
todayAgenda (e:es) = do
    putStrLn $ "Evento Nro: "  ++ (show $ nth e)
    putStrLn $ "Descripción: " ++ (description e)
    todayAgenda es

-- | 'mostrarHoy' recibe una lista de Eventos y muestra los eventos del dia al usuario
mostrarHoy :: [Evento] -> IO ()
mostrarHoy [] = do putStrLn "No tiene eventos este día."
mostrarHoy es = do putStrLn "Eventos del día: " >> todayAgenda es   

-- | 'hacer' recibe una lista de eventos, una fecha y retorna un monad IO.
-- Esta función es empleada para hacer interactuar con el usuario y tomar las
-- acciones pertinentes de acuerdo a las opciones seleccionadas.
hacer :: [Evento] -> (Year, Month, Day) -> IO ()
hacer list (y,m,d)= do
	-- teclas para movimientos y eventos
	let moves 	  = ['j', 'k', 'l', 'h']
	let calActual = actual (y,m,d) list
	let dehoy 	  = todayEvents (y,m,d) list
	-- Mostrar fecha calendario, y eventos actuales
	putStr   $ toString calActual
	mostrarHoy dehoy
	prompt 	   (y, m, d)
	-- Solicitar entrada del Usuario
	hSetBuffering stdin NoBuffering
	s <- getChar
	hSetBuffering stdin LineBuffering
	-- Colocar un salto de linea
	putStrLn ""
	-- Si el elemento esta en movimiento
	if elem s moves then do
		clearS
		hacer list (move s (y, m, d))
	else
		-- registrar
		if s == 'r' then do
			putStr "Coloque descripción: "
			msj  	 <- getLine
			let n 	  = getMax4Day (y, m, d) list
			let list' = descPrompt (y, m, d) (n + 1) list msj
			clearS
			hacer list' (y, m, d)
		else
			-- eliminar
			if s == 'd' then do
				putStr "Borrar evento Nro: "
				i 		 <- getInt
				let list' = removePrompt (y, m, d) list i
				clearS
				hacer list' (y, m, d)
			else
				-- salir
				if s == 'q' then
					saveEvents "hagenda.txt" list
				--mostrar ayuda
				else
					clearS  >>
					helpPls >> hacer list (y, m, d)

-- Main
main = do
		hSetBuffering stdout NoBuffering
		c      <- currentDate
		exists <- doesFileExist "hagenda.txt"
 		if exists then do
			list <-  loadEvents "hagenda.txt"
			hacer list c
		else 
			hacer [] c


