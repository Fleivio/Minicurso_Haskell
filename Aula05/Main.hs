
data Temperature = 
    C Float 
  | F Float 

celsiusToF :: Float -> Float 
celsiusToF c = 1.8 * c + 32

instance Eq Temperature where 
  (F f1) == (F f2) = f1 == f2
  (C c1) == (C c2) = c1 == c2
  (F f1) == (C c1) = celsiusToF c1 == f1 
  (C c1) == (F f1) = celsiusToF c1 == f1 

instance Show Temperature where 
  show (F f1) = show f1 ++ "f"
  show (C c1) = show c1 ++ "c"


data Shape = 
    Circle Double 
  | Rectangle Double Double 
  | Triangle Double Double 
  deriving (Show)

data Solid =
    Sphere Double
  | Cuboid Double Double Double
  deriving (Show)

class HasArea a where 
  area :: a -> Double

  squareArea :: a -> Double
  squareArea forma = area forma ** 2  

instance HasArea Shape {- a -} where 
  area (Circle radius) = radius * radius * pi
  area (Rectangle h w) = h * w
  area (Triangle b h)  = (b * h) / 2

instance HasArea Solid {- a -}where
  area (Sphere r) = 4 * pi * r * r
  area (Cuboid w h d) = 2 * (w*h + w*d + h*d)

instance Eq Shape where
  shape1 == shape2 
    = (area shape1 == area shape2)

instance Ord Shape where 
  shape1 <= shape2 = area shape1 <= area shape2


main :: IO ()
main = do 
  print "fasdfsad"
  print "ifadfbsadf"
  string <- readFile "filePath"
