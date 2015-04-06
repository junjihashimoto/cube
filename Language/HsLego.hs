{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE InstanceSigs#-}

module Language.HsLego where
import Prelude
--hiding (map)
--import Data.Set
import Data.List
import Data.Monoid
import Control.Applicative
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Graphics.Formats.STL

class ToSTL a where
  toSTL :: a -> STL

data Unit = Unit { -- quaternion
  us :: Int
, ux :: Int
, uy :: Int
, uz :: Int
} deriving (Show,Eq,Ord)


instance Monoid Unit where
  mappend a b = a + b
  mempty = Unit 0 0 0 0


instance Num Unit where
  (+) (Unit ax ay az ar) (Unit bx by bz br) =
    Unit (ax+bx) (ay+by) (az+bz) (ar+br)
  (-) (Unit ax ay az ar) (Unit bx by bz br) =
    Unit (ax-bx) (ay-by) (az-bz) (ar-br)
  (*) (Unit a1 b1 c1 d1) (Unit a2 b2 c2 d2) =
    Unit
     (a1*a2-b1*b2-c1*c2-d1*d2)
     (a1*b2+b1*a2+c1*d2-d1*c2)
     (a1*c2-b1*d2+c1*a2+d1*b2)
     (a1*d2+b1*c2-c1*b2+d1*a2)
  abs (Unit ax ay az ar) = Unit (abs ax) (abs ay) (abs az) (abs ar)
  signum (Unit ax ay az ar) = Unit (signum ax) (signum ay) (signum az) (signum ar)
  fromInteger a = Unit (fromIntegral a) 0 0 0

data Block a =
  Block {
    units :: [a]
  } deriving (Show,Eq)

instance (Eq a,Num a) => Num (Block a) where
  (+) (Block a) (Block b) = Block $ a <> b
  (-) (Block a) (Block b) = Block $ a \\ b
  (*) (Block a) (Block b) = Block $ do
    au <- a
    bu <- b
    return (au + bu)
  abs (Block a) = Block $ map abs a
  signum (Block a) = Block $ map signum a
  fromInteger a = Block $ [fromInteger a]

instance Functor Block where
  fmap func (Block elems) = Block $ fmap func elems

cubeTriangle :: [([Int], [Int], [Int])]
cubeTriangle =  [(a,b,c) | a <- [[0,0,0],[0,1,1],[1,0,1],[1,1,0]],b <- cube, c <- cube, dist a b == 1 , dist a c == 1 , dist b c == 2, b < c ]
  where
    cube = do
      a <- [0,1]
      b <- [0,1]
      c <- [0,1]
      return [a,b,c]
  
    dist :: [Int] -> [Int] -> Int
    dist a b =  sum $ map abs $ map (uncurry (-)) $ zip a b


instance ToSTL Unit where
  toSTL (Unit _s x y z) = STL "unit" $ (flip map) cubeTriangle $ \(t0,t1,t2) ->
      Triangle Nothing (
         (ve (map (uncurry (+)) $ zip t0 xyz)),
         (ve (map (uncurry (+)) $ zip t1 xyz)),
         (ve (map (uncurry (+)) $ zip t2 xyz)))
    where
      ve :: [Int] -> Vector
      ve [a,b,c] = (fromIntegral a,fromIntegral b,fromIntegral c)
      ve _ = error ""
      xyz :: [Int]
      xyz = [x,y,z]

instance ToSTL a => ToSTL (Block a) where
  toSTL (Block sets) = foldr (<>) mempty $ map toSTL sets

instance Monoid STL where
  mappend (STL an at) (STL bn bt) = STL an (at<>bt)
  mempty = STL "emptry" []

instance Monoid a => Monoid (Block a) where
  mappend (Block a) (Block b) = Block (a<>b)
  mempty = Block [mempty]

writeFileStl :: ToSTL a => String -> a -> IO ()
writeFileStl filename stl = BL.writeFile filename . B.toLazyByteString . textSTL $ toSTL stl

dz :: Unit
dz = Unit 0 0 0 1

dy :: Unit
dy = Unit 0 0 1 0

dx :: Unit
dx = Unit 0 1 0 0

ds :: Unit
ds = Unit 1 0 0 0

cube :: Int -> Block Unit
cube n =
  let block = [0..(n-1)]
  in Block $ [Unit 0 a b c | a <- block,b <- block,c <- block]

-- -- house :: Block Unit
-- -- house = 

-- main :: IO ()
-- main = do
--   let block = mempty :: Block Unit
--   let block2 = (+) (-3*dx) <$> block  :: Block Unit
--   writeFileStl "hoge.stl" $ cube 2 + block2
