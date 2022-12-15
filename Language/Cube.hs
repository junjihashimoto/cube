{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Cube (
  RSTL(..)
, Stl
, ToStl(..)
, Conjugate(..)
, Delta(..)
, RFunctor(..)
, Quaternion(..)
, Cube
, Block(..)
, toSTL
, toCube
, block
, cube
, compaction
, flipTriangle
, writeFileStl
, writeFileStlWithText
, printStl
, nCube
, surface'
, surface
) where

import qualified Data.Serialize as C
import Data.Monoid
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BL
import Graphics.Formats.STL
import Control.Monad

-- | Ristricted STL
-- This is the almost same as STL of Graphics.Formats.STL.
-- This is used to instantiate RFunctor of STL.
-- RFunctor provides a function(rfmap) like fmap of Functor.
data RSTL a =
  Stl {
    stlData :: STL
  }

-- | Wrapper of RSTL
type Stl = RSTL Triangle

-- | Generate Ristricted STL
class ToStl a where
  toStl :: a -> Stl

-- | Generate STL from Ristricted STL
toSTL :: ToStl a => a -> STL
toSTL a = stlData $ toStl a

-- | class type of mathematical conjugate
class Conjugate a where
  conjugate :: a -> a

-- | Delta move and rotation for quaternion
class (Num a) => Delta a where
  -- | delta for x axis
  dx :: a
  -- | delta for y axis
  dy :: a
  -- | delta for z axis
  dz :: a
  -- | delta for real part of quaternion
  ds :: a
  -- | cons (theta/2) + i sin (theta/2) + j sin (theta/2)  + k sin (theta/2)  of  quaternion
  dr :: Float -- ^ radian
     -> a    -- ^ axes of routation
     -> a
  -- | Routation for quaternion
  dR :: Float -- ^ radian
     -> a    -- ^ axes of routation
     -> a    -- ^ Input Vector
     -> a
  -- | Routation for quaternion
  -- This is the same as dR.
  rotate :: Float -- ^ radian
         -> a    -- ^ axes of routation
         -> a    -- ^ Input Vector
         -> a
  rotate = dR

-- | Ristricted Functor
class RFunctor f a b where
  rfmap :: (a -> b) -> f a -> f b

-- | Functor of Set
instance (Ord a, Ord b) => RFunctor S.Set a b where
  rfmap = S.map

instance (Functor m) => RFunctor m a b where
  rfmap = fmap

instance RFunctor RSTL Triangle Triangle where
  rfmap func (Stl stl) = Stl $ stl {triangles = map func (triangles stl)}

-- | Unit element of Cube.
-- 
-- This is the same as quaternion.
data Quaternion a = Quaternion {
  us :: a
, ux :: a
, uy :: a
, uz :: a
} deriving (Show,Eq,Ord)

type Cube = Quaternion Float

instance Functor Quaternion where
  fmap func Quaternion{..} = Quaternion (func us) (func ux) (func uy) (func uz)

instance Semigroup a => Semigroup (Quaternion a) where
  Quaternion s x y z <> (Quaternion s' x' y' z') = Quaternion (s <> s') (x <> x') (y <> y') (z <> z')

instance Num a => Conjugate (Quaternion a) where
  conjugate (Quaternion s x y z) = Quaternion s (-x) (-y) (-z)

instance Delta Cube where
  dx = Quaternion 0 1 0 0
  dy = Quaternion 0 0 1 0
  dz = Quaternion 0 0 0 1
  ds = Quaternion 1 0 0 0
  dr theta (Quaternion _s x y z) =  Quaternion (co 1) (si x) (si y) (si z)
    where
      co :: Float -> Float
      co v = cos (theta/2) * v
      si :: Float -> Float
      si v = sin (theta/2) * v
  dR theta a i = let r = (dr theta a)
                     tmp = fmap round (r * i * conjugate r) :: Quaternion Integer
                 in fmap fromIntegral tmp
instance (Num a, Semigroup a) => Monoid (Quaternion a) where
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
  abs (Block a) = Block $ rfmap abs a
  signum (Block a) = Block $ rfmap signum a
  fromInteger a = Block $ S.singleton $ fromInteger a

instance (Ord a, Ord b) => RFunctor Block a b where
  rfmap func Block{..} = Block $ rfmap func units

instance Num STL where
  (+) (STL an atri) (STL _bn btri) = STL an $ S.toList $ S.fromList (atri ++ btri)
  (-) (STL an atri) (STL _bn btri) = STL an $ S.toList $ S.fromList atri S.\\ S.fromList btri
  (*) _ _ = error "(*) is not defined for STL"
  abs _ = error "abs is not defined for STL"
  signum _ = error "signum is not defined for STL"
  fromInteger _ = error "fromInteger is not defined for STL"

instance Semigroup STL where
  STL an atri <> (STL bn btri) = STL (an <> bn) (atri <> btri)

instance Num Stl where
  (+) (Stl a) (Stl b) = Stl $ a + b
  (-) (Stl a) (Stl b) = Stl $ a - b
  (*) _ _ = error "(*) is not defined for STL"
  abs _ = error "abs is not defined for STL"
  signum _ = error "signum is not defined for STL"
  fromInteger _ = error "fromInteger is not defined for STL"

instance Semigroup Stl where
  Stl a <> (Stl b) = Stl $ a <> b

-- | Utility function of generating Block from list of cube
block :: (Ord a) => [a] -> Block a
block elems = Block $ S.fromList elems

-- | Utility function of Cube 0 x y z
cube :: Int -> Int -> Int -> Cube
cube x y z = Quaternion 0 (fromIntegral x) (fromIntegral y) (fromIntegral z)

-- | Utility function of generating Cube from list of Int
toCube :: [Int] -> Cube
toCube [s,x,y,z] = Quaternion (fromIntegral s) (fromIntegral x) (fromIntegral y) (fromIntegral z)
toCube [a,b,c] = cube a b c
toCube _ = error "toCube"

-- | Flip rotation of triangle
flipTriangle :: Triangle -> Triangle
flipTriangle (Triangle m (x,y,z)) = Triangle m (x,z,y)

deriving instance (Ord Triangle)
deriving instance (Eq Triangle)
deriving instance (Show Triangle)

instance ToStl STL where
  toStl a = Stl a

instance ToStl Stl where
  toStl a = a

instance Monoid STL where
  mappend (STL an at) (STL _bn bt) = STL an (at<>bt)
  mempty = STL "" []

instance Monoid Stl where
  mappend (Stl a) (Stl b) = Stl (a<>b)
  mempty = Stl mempty

instance Semigroup Float where
  _ <> _ = error "<> is not defined for Float"

instance ToStl Cube where
  toStl v = Stl $ STL "" $ flip map tri2 $ \[t0,t1,t2] ->
      Triangle Nothing (
         ve (t0 + v),
         ve (t1 + v),
         ve (t2 + v))
    where
      ve (Quaternion _s a b c) = (a,b,c)
      vec [a,b,c] =
        case (b-a)*(c-a) + a of
          Quaternion _s x y z | 0 <= x && x <=1  &&0 <= y && y <=1  && 0 <= z && z <=1 -> True
                              | otherwise -> False
      vec _ = error "vec"
      o1  = [mempty,mempty + dx,mempty + dy]
      o2 = [mempty+dx+dy,mempty + dx,mempty + dy]
      o3 = map (+ dz) o1
      o4 = map (+ dz) o2
      o5 = map (dR (pi/2) dx) o1
      o6 = map (dR (pi/2) dx) o2
      o7 = map (+ dy) o5
      o8 = map (+ dy) o6
      o9 = map (dR (-pi/2) dy) o1
      o10 = map (dR (-pi/2) dy) o2
      o11 = map (+ dx) o9
      o12 = map (+ dx) o10
      tri = [o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12]
      tri2 = map (\l@[a,b,c] -> if vec l then [a,c,b] else [a,b,c]) tri


instance (ToStl a) => ToStl (Block a) where
  toStl (Block sets) = compaction $ foldr (<>) mempty $ map toStl $ S.toList sets

instance (Semigroup a, Ord a) => Semigroup (Block a) where
  Block a <> (Block b) = Block (a <> b)

instance (Ord a, Monoid a) => Monoid (Block a) where
  mappend (Block a) (Block b) = Block (a<>b)
  mempty = Block $ S.singleton mempty

-- | Remove redundant triangles
compaction :: Stl -> Stl
compaction stl =  stl - (rfmap flipTriangle stl)

-- | Generate binary STL file from Block
writeFileStl :: ToStl a => String -> a -> IO ()
writeFileStl filename stl = BL.writeFile filename $ C.encodeLazy $ toSTL stl

-- | Generate text STL file from Block
writeFileStlWithText :: ToStl a => String -> a -> IO ()
writeFileStlWithText filename stl = BL.writeFile filename $ BL.toLazyByteString $ textSTL $ toSTL stl

-- | Print triangles of STL
printStl :: ToStl a => a -> IO ()
printStl stl = do
  let tris = triangles $ toSTL stl
  forM_ tris $ \t -> do
    print $ t


-- | This function genrates a cube of n-width.
nCube :: Int -> Block Cube
nCube n =
  let lst = [0..(n-1)]
  in Block $ S.fromList [cube a b c | a <- lst,b <- lst,c <- lst]

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

