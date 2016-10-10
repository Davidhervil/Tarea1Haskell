import Data.List
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
toString = concat . map (++"\n") . pixels 

stack :: [Picture] -> Picture
stack = foldr1 above

spread :: [Picture] -> Picture
spread = foldl1 beside

row :: String -> Picture
row = spread . map pixel

blank :: (Height,Width) -> Picture
--blank (h,w) = stack $ replicate h $ row $ replicate w ' ' <==== VERSION POINTFUL
blank = uncurry ((stack .) . (. (row . flip replicate ' ') ) . replicate )
