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
												 pixels = 
												}
									else 
										error "can’t ’above’ different widths"
