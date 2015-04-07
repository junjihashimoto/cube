{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE Rank2Types#-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}

module Language.Cube (
  ToSTL(..)
, Quaternion(..)
, Cube
, Block(..)
, smap
, block
, cube
, writeFileStl
, ds
, dx
, dy
, dz
, dr
, nCube
, surface'
, surface
, house
) where

import qualified Data.Serialize as C
import Data.Monoid
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import Graphics.Formats.STL

class ToSTL a where
  toSTL :: a -> STL

-- | Unit element of Cube.
-- 
-- This is the same as quaternion.
data Quaternion a = Quaternion {
  us :: a
, ux :: a
, uy :: a
, uz :: a
} deriving (Show,Eq,Ord)

type Cube = Quaternion Int

instance (Num a) => Monoid (Quaternion a) where
  mappend a b = a + b 
  mempty = Quaternion 0 0 0 0


instance (Num a) => Num (Quaternion a) where
  (+) (Quaternion ax ay az ar) (Quaternion bx by bz br) =
    Quaternion (ax+bx) (ay+by) (az+bz) (ar+br)
  (-) (Quaternion ax ay az ar) (Quaternion bx by bz br) =
    Quaternion (ax-bx) (ay-by) (az-bz) (ar-br)
  (*) (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) =
    Quaternion
     (a1*a2-b1*b2-c1*c2-d1*d2)
     (a1*b2+b1*a2+c1*d2-d1*c2)
     (a1*c2-b1*d2+c1*a2+d1*b2)
     (a1*d2+b1*c2-c1*b2+d1*a2)
  abs (Quaternion ax ay az ar) = Quaternion (abs ax) (abs ay) (abs az) (abs ar)
  signum (Quaternion ax ay az ar) = Quaternion (signum ax) (signum ay) (signum az) (signum ar)
  fromInteger a = Quaternion (fromIntegral a) 0 0 0

--norm2 :: Quaternion a -> a
--norm2 (Quaternion s x y z) = s*s + x**n + y**n + z**n

-- | Set of Cube.
-- This supports boolean operations on polygons.
-- (+) means or.
-- (-) means not.
-- (*) means convolution.
data Block a =
  Block {
    units :: S.Set a
  } deriving (Show,Eq,Ord)

instance (Ord a,Eq a,Num a) => Num (Block a) where
  (+) (Block a) (Block b) = Block $ a <> b
  (-) (Block a) (Block b) = Block $ (S.\\) a b
  (*) (Block a) (Block b) = Block $ S.fromList $ do
    au <- S.toList a
    bu <- S.toList b
    return (au + bu)
  abs (Block a) = Block $ S.map abs a
  signum (Block a) = Block $ S.map signum a
  fromInteger a = Block $ S.singleton $ fromInteger a

-- | map for Block.
smap :: (Ord a,Ord b) => (a -> b) -> Block a -> Block b
smap func (Block elems) = Block $ S.map func elems

-- | Utility function of generating Block from list of cube
block :: (Ord a) => [a] -> Block a
block elems = Block $ S.fromList elems

-- | Utility function of Cube 0 x y z
cube :: Int -> Int -> Int -> Cube
cube x y z = Quaternion 0 x y z

toCube :: [Int] -> Cube
toCube [s,a,b,c] = Quaternion s a b c
toCube [a,b,c] = cube a b c
toCube _ = error "toCube"



instance ToSTL (Quaternion Int) where
  toSTL v = STL "" $ flip map tri2 $ \[t0,t1,t2] ->
      Triangle Nothing (
         ve (t0 + v),
         ve (t1 + v),
         ve (t2 + v))
    where
      ve (Quaternion _s a b c) = (fromIntegral a,fromIntegral b,fromIntegral c)
      vec [a,b,c] =
        case (b-a)*(c-a) + a of
          Quaternion _s x y z | 0 <= x && x <=1  &&0 <= y && y <=1  && 0 <= z && z <=1 -> True
                              | otherwise -> False
      vec _ = error "vec"
      tri =  [map toCube [a,b,c] |
              a <- [[0,0,0],[0,1,1],[1,0,1],[1,1,0]],
              b <- cube0,
              c <- cube0,
              dist a b == 1,
              dist a c == 1,
              dist b c == 2,
              b < c ]
      
      tri2 = map (\l@[a,b,c] -> if vec l then [a,c,b] else [a,b,c]) tri
      
      cube0 = do
        a <- [0,1]
        b <- [0,1]
        c <- [0,1]
        return [a,b,c]
      dist a b =  sum $ map abs $ map (uncurry (-)) $ zip a b

-- instance (Real a,Fractional a, Num a) => ToSTL (Quaternion a) where
--   toSTL v@(Quaternion _s x y z) = STL "" $ flip map tri2 $ \[t0,t1,t2] ->
--       Triangle Nothing (
--          ve (t0 + v),
--          ve (t1 + v),
--          ve (t2 + v))
--     where
--       ve (Quaternion _s a b c) = (realToFrac a,realToFrac b,realToFrac c)
--       ve _ = error ""
--       xyz = [x,y,z]
--       cubeTriangle =  [(a,b,c) | a <- [[0,0,0],[0,1,1],[1,0,1],[1,1,0]],
--                                  b <- cube0,
--                                  c <- cube0,
--                                  dist a b == 1,
--                                  dist a c == 1,
--                                  dist b c == 2,
--                                  b < c ]
--       cube0 = do
--         a <- [0,1]
--         b <- [0,1]
--         c <- [0,1]
--         return [a,b,c]
--       dist a b =  sum $ map abs $ map (uncurry (-)) $ zip a b

instance (ToSTL a) => ToSTL (Block a) where
  toSTL (Block sets) = foldr (<>) mempty $ map toSTL $ S.toList sets

instance Monoid STL where
  mappend (STL an at) (STL _bn bt) = STL an (at<>bt)
  mempty = STL "emptry" []

instance (Ord a, Monoid a) => Monoid (Block a) where
  mappend (Block a) (Block b) = Block (a<>b)
  mempty = Block $ S.singleton mempty

-- | Generate STL file from Block
writeFileStl :: ToSTL a => String -> a -> IO ()
writeFileStl filename stl = BL.writeFile filename $ C.encodeLazy $ toSTL stl

-- | Unit vector of Z direction
dz :: Cube
dz = Quaternion 0 0 0 1

-- | Unit vector of Y direction
dy :: Cube
dy = Quaternion 0 0 1 0

-- | Unit vector of X direction
dx :: Cube
dx = Quaternion 0 1 0 0

-- | Unit scalar vector
ds :: Cube
ds = Quaternion 1 0 0 0

-- | Vector for generating routation vector
dr :: Float -- ^ radian
   -> Cube -- ^ axes of routation
   -> Cube
dr theta (Quaternion _s x y z) =  Quaternion (co 1) (si x) (si y) (si z)
  where
    co :: Int -> Int
    co v = round $ cos theta * fromIntegral v
    si :: Int -> Int
    si v = round $ sin theta * fromIntegral v

-- | This function genrates a cube of n-width.
nCube :: Int -> Block Cube
nCube n =
  let lst = [0..(n-1)]
  in Block $ S.fromList [Quaternion 0 a b c | a <- lst,b <- lst,c <- lst]

-- | Generate surface block.
-- This is fast function. But shape becomes little bit bigger.
surface' :: Block Cube -> Block Cube
surface' model = model * cube' - model
  where
    cube' :: Block Cube
    cube' = block $ [Quaternion 0 x y z| x<-[-1..1], y<-[-1..1], z<-[-1..1]]

-- | Generate surface block
surface :: Block Cube -> Block Cube
surface model = model - (model - (surface' model) * cube2)
  where
    cube2 :: Block Cube
    cube2 = block $ [Quaternion 0 x y z| x<-[-2..2], y<-[-2..2], z<-[-2..2]]

-- | Generate house which is for demo.
house :: Block Cube
house = let house''  = (house' + square)*line
        in smap (12 * dz +)  $ surface house''
  where
    house' :: Block Cube
    house' = block $ [Quaternion 0 1 x y| x<-[-10..10], y<-[-10..10], y < x , y < (-x)]
    
    square :: Block Cube
    square = smap ((+) (-12 * dz)) $ block $ [Quaternion 0 1 x y| x<-[-5..5], y<-[-5..5]]
    
    line :: Block Cube
    line = block $ [Quaternion 0 x 0 0 | x<-[-5..5]]
