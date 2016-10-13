--import Data.List
type Day   = Int
-- Suponga que est· entre 1 y 31
type Year  = Int
-- Suponga que es positivo
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
	deriving (Show,Eq,Ord,Enum)
data DayName = Domingo
			| Lunes
			| Martes
			| Miercoles
			| Jueves
			| Viernes
			| Sabado
	deriving (Show,Eq,Ord,Enum)
type Height  = Int
type Width   = Int
data Picture = Picture {
					height :: Height,
					width  :: Width,
					pixels :: [[Char]]
					} deriving (Show)

-- |La funcion 'pixel' devuleve un 'Picture' dado un 'Char'.
pixel :: Char -> Picture
pixel c = Picture { height=1, width=1, pixels = [[c]]}

-- |'above' toma dos 'Picture', p0 y p1 y genera otro 'Picture' cuyo 'pixels'
-- tendrá a p0 arriba de p1.
above :: Picture -> Picture -> Picture
above p0 p1  = if width p0 == width p1 then
					Picture { height= height p0 + width p1,
							 width= width p0,
							 pixels = pixels p0 ++ pixels p1 
							}
				else
					error "can’t ’above’ different widths"
-- |'beside' toma dos 'Picture', p0 y p1 y genera otro 'Picture'cuyo 'pixels'
-- tendrá a p0 a la izqueirda de p1.
beside :: Picture -> Picture -> Picture
beside p0 p1 = if height p0 == height p1 then
					Picture { height= height p0,
						 	  width= width p0 + width p1,
							  pixels = beside' (pixels p0) (pixels p1)
							}
				else 
					error "can’t ’beside’ different height"
				where 
					beside' []      xs2  = xs2
					beside' xs1      []  = xs1
					beside' (x:xs1) (x2:xs2) = (x ++ x2):beside' xs1 xs2


-- |'toString' dvuelve un 'String' "listo para printear" dado un 'Picture'.
-- Notar que antes era un 'intercalate "\n" . pixels'. Lo que deja sin '\n' al
-- final.
toString :: Picture -> String
toString = concatMap (++"\n") . pixels 

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
-- blank (h,w) = stack $ replicate h $ row $ replicate w ' ' <==== VERSION POINTFUL
blank :: (Height,Width) -> Picture
blank = uncurry ((stack .) . (. (row . flip replicate ' ') ) . replicate )


------------------------------------------------------------------------------------
stackWith :: Height -> [Picture] -> Picture
--stackWith h ps = foldl1 above (map (above (blank (h,width (head ps)))) ps)
--stackWith h ps = foldl1 above ( (interspersex . ( ( (. (width . head) ) . (curry blank) ) h ) ) ps ps )
--stackWith h ps = foldl1 above ( chevrFunc (interspersex . ( ( (. (width . head) ) . (curry blank) ) h ) ) ps )
stackWith = (foldl1 above .) . chevrFunc . (interspersex .) . (. (width . head)) . curry blank
	where
		chevrFunc::(a-> a-> b) -> a -> b
		chevrFunc f a = f a a

		interspersex :: a -> [a] -> [a]
		interspersex a xs = tail $ foldr f [] xs
			where
				f x ls = a:(x:ls)

spreadWith :: Width -> [Picture] -> Picture
--spreadWith w ps = foldl1 beside  (interspersex ( blank ( (height . head) ps , w) ) ps ) 
spreadWith = (foldl1 beside .) . chevrFunc . (interspersex .) . (. (height . head)) . curry blank
	where
		chevrFunc::(a-> a-> b) -> a -> b
		chevrFunc f a = f a a

		interspersex :: a -> [a] -> [a]
		interspersex a xs = tail $ foldr f [] xs
			where
				f x ls = a:(x:ls)
				
tile :: [[Picture]] -> Picture
tile = stack . (map spread)

tileWith :: (Height, Width) -> [[Picture]] -> Picture
tileWith (h,w) pss = stackWith h (map (spreadWith w) pss)
-----------------------------------------------------------------------------------
rjustify :: Int -> String -> String
rjustify n s = 	if n >= (length s) then
					(replicate (n - length s ) ' ') ++ s
				else
					error "Tamaño final menor al texto"

dnames:: Picture
dnames = beside (pixel ' ') $ spreadWith 1 $ map (row.(take 2).show.cast) [0..6]
	where
		cast i = toEnum i :: DayName

banner :: Month -> Year -> Picture
--banner m y = row (rjustify (width dnames) (toString (spreadWith 1 [(row . show) m, (row . show) y])))
banner m y = row (rjustify (width dnames ) (show m ++ (' ':show y)))


heading :: Month -> Year -> Picture
heading m y = banner m y `above` dnames


--------------------------------------------------------------------------------------

leap :: Year -> Bool
leap y = if (mod y 100) == 0 then (mod y 400) == 0 else (mod y 4) == 0

mlengths  :: Year -> [Day]
mlengths y = [31, if leap y then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

group1 :: Int -> [a] -> [[a]]
group1 _ [] = []
group1 n xs
  | n > 0 = (take n xs) : (group1 n (drop n xs))
  | otherwise = error "N no positivo"

jan1 :: Year -> DayName
jan1 y = toEnum  (mod
					(
					1                 +
					((y-1) * 365)     +
			 		div (y - 1) 4     +
			 		div (y - 1) 400   -
			 		div (y - 1) 100 )
			 		7)
				
--      Domingo es cero
-- 		Multiplicamos el numero de años que han pasado
-- 		Sumamos los años bisiestos 

mtotals :: Year -> [Int]
mtotals y = scanl (+) (fromEnum (jan1 y)) b
			where b = mlengths y

fstdays :: Year -> [DayName]
fstdays y = map toEnum ( map (`mod` 7) ((init . mtotals )y))

fstday :: Month -> Year -> DayName
fstday m y = (fstdays y ) !! (fromEnum m)

day :: Day -> Month -> Year -> DayName
day d m y = toEnum $ ( mod ((fromEnum (fstday m y)) + (fromEnum d) +6 ) 7 )


---------------------------------------------------------------------------

a = pixel 'a'
b = pixel 'b'
c = pixel 'c'
d = pixel 'd'
e = pixel 'e'
f = pixel 'f'
g = pixel 'g'
h = pixel 'h'