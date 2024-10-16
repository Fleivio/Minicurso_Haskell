import Data.Char
import Data.List (intercalate)

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
    = area shape1 == area shape2

instance Ord Shape where 
  shape1 <= shape2 = area shape1 <= area shape2

-------------
data Meta a = Meta {
    content :: a,
    nLines :: Int,
    nWords :: Int
  } deriving Eq

instance Show a => Show (Meta a) where
  show (Meta c l w) = 
    unlines [show c, show l, show w]

instance Functor Meta where 
  fmap f meta = meta {content = f (content meta)}

instance Semigroup a => Semigroup (Meta a) where
  Meta s1 l1 w1 <> Meta s2 l2 w2 
    = Meta (s1 <> s2) 
           (l1 + l2)
           (w1 + w2)

metaFromString :: String -> Meta String
metaFromString str = 
  Meta str
       (length $ lines str)
       (length $ words str)

capitalise :: String -> String 
capitalise = unwords . fmap (\(x:xs) -> toUpper x : xs) . words

main :: IO ()
main = do
  str1 <- metaFromString <$> readFile "a" 
  str2 <- metaFromString <$> readFile "b" 
  let meta = str1 <> fmap (' ':) str2
  writeFile "meta.txt" (show $ fmap capitalise meta)