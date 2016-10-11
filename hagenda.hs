import Data.List
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

pixel :: Char -> Picture
pixel c = Picture { height=1, width=1, pixels = [[c]]}

above :: Picture -> Picture -> Picture
above p0 p1 = if width p0 == width p1 then
										Picture { height= height p0 + width p1,
												 width= width p0,
												 pixels = pixels p0 ++ pixels p1 
												}
									else
										error "can’t ’above’ different widths"

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

toString :: Picture -> String -- Notar que antes era un intercalate "\n" . pixels. Lo que deja sin '\n' al final
toString = concatMap (++"\n") . pixels 

stack :: [Picture] -> Picture
stack = foldr1 above

spread :: [Picture] -> Picture
spread = foldl1 beside

row :: String -> Picture
row = spread . map pixel

--blank (h,w) = stack $ replicate h $ row $ replicate w ' ' <==== VERSION POINTFUL
blank :: (Height,Width) -> Picture
blank = uncurry ((stack .) . (. (row . flip replicate ' ') ) . replicate )


------------------------------------------------------------------------------------
-- CREO QUE ESTAS DOS SE PUEDEN HACER SOLO CON FOLDL1
-- NOTA : Sobra uno en blanco. hacer con init y pegar ultimo.
stackWith :: Height -> [Picture] -> Picture
--stackWith h ps = foldl1 above (map (above (blank (h,width (head ps)))) ps)
stackWith h ps = foldl1 above (intersperse ( ( (curry blank) h ( (width . head) ps ) ) ) ps )

-- NOTA: Sobra uno en blanco. hacer init y pegar ultimo.
spreadWith :: Height -> [Picture] -> Picture
spreadWith w ps = foldl1 beside  (map (beside (blank (w, height (head ps)))) ps)

tile :: [[Picture]] -> Picture
tile = stack . (map spread)

tileWith :: (Height, Width) -> [[Picture]] -> Picture
tileWith (h,w) pss = stackWith h (map (spreadWith w) pss)

--------------------------------------------------------------------------------------

leap :: Year -> Bool
leap y = if (mod y 100) == 0 then (mod y 400) == 0 else (mod y 4) == 0

mlengths  :: Year -> [Day]
mlengths y = [31, if leap y then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


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

--rjustify :: Int -> String -> String
--rjustify n s = 