{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE InstanceSigs#-}
{-#LANGUAGE Rank2Types#-}

module Language.Cube where
import Prelude
--hiding (map)
--import Data.Set
import Data.List
import qualified Data.Serialize as C
import Data.Monoid
import qualified Data.Set as S
import Control.Applicative
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Graphics.Formats.STL

class ToSTL a where
  toSTL :: a -> STL

-- | Unit element of Cube.
-- 
-- This is the same as quaternion.
data Cube = Cube {
  us :: Int
, ux :: Int
, uy :: Int
, uz :: Int
} deriving (Show,Eq,Ord)

instance Monoid Cube where
  mappend a b = a + b 
  mempty = Cube 0 0 0 0

instance Num Cube where
  (+) (Cube ax ay az ar) (Cube bx by bz br) =
    Cube (ax+bx) (ay+by) (az+bz) (ar+br)
  (-) (Cube ax ay az ar) (Cube bx by bz br) =
    Cube (ax-bx) (ay-by) (az-bz) (ar-br)
  (*) (Cube a1 b1 c1 d1) (Cube a2 b2 c2 d2) =
    Cube
     (a1*a2-b1*b2-c1*c2-d1*d2)
     (a1*b2+b1*a2+c1*d2-d1*c2)
     (a1*c2-b1*d2+c1*a2+d1*b2)
     (a1*d2+b1*c2-c1*b2+d1*a2)
  abs (Cube ax ay az ar) = Cube (abs ax) (abs ay) (abs az) (abs ar)
  signum (Cube ax ay az ar) = Cube (signum ax) (signum ay) (signum az) (signum ar)
  fromInteger a = Cube (fromIntegral a) 0 0 0

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
cube x y z = Cube 0 x y z

instance ToSTL Cube where
  toSTL (Cube _s x y z) = STL "" $ (flip map) cubeTriangle $ \(t0,t1,t2) ->
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
      cubeTriangle :: [([Int], [Int], [Int])]
      cubeTriangle =  [(a,b,c) | a <- [[0,0,0],[0,1,1],[1,0,1],[1,1,0]],
                                 b <- cube0,
                                 c <- cube0,
                                 dist a b == 1,
                                 dist a c == 1,
                                 dist b c == 2,
                                 b < c ]
      cube0 = do
        a <- [0,1]
        b <- [0,1]
        c <- [0,1]
        return [a,b,c]
      dist :: [Int] -> [Int] -> Int
      dist a b =  sum $ map abs $ map (uncurry (-)) $ zip a b

instance (ToSTL a) => ToSTL (Block a) where
  toSTL (Block sets) = foldr (<>) mempty $ map toSTL $ S.toList sets

instance Monoid STL where
  mappend (STL an at) (STL bn bt) = STL an (at<>bt)
  mempty = STL "emptry" []

instance (Ord a, Monoid a) => Monoid (Block a) where
  mappend (Block a) (Block b) = Block (a<>b)
  mempty = Block $ S.singleton mempty

-- | Generate STL file from Block
writeFileStl :: ToSTL a => String -> a -> IO ()
writeFileStl filename stl = BL.writeFile filename $ C.encodeLazy $ toSTL stl

-- | Unit vector of Z direction
dz :: Cube
dz = Cube 0 0 0 1

-- | Unit vector of Y direction
dy :: Cube
dy = Cube 0 0 1 0

-- | Unit vector of X direction
dx :: Cube
dx = Cube 0 1 0 0

-- | Unit scalar vector
ds :: Cube
ds = Cube 1 0 0 0

-- | Vector for generating routation vector
dr :: Float -- ^ radian
   -> Cube -- ^ axes of routation
   -> Cube
dr theta (Cube s x y z) =  Cube (co 1) (si x) (si y) (si z)
  where
    co :: Int -> Int
    co v = round $ cos theta * fromIntegral v
    si :: Int -> Int
    si v = round $ sin theta * fromIntegral v

-- | This function genrates a cube of n-width.
nCube :: Int -> Block Cube
nCube n =
  let block = [0..(n-1)]
  in Block $ S.fromList [Cube 0 a b c | a <- block,b <- block,c <- block]

-- | Generate surface block.
-- This is fast function. But shape becomes little bit bigger.
surface' :: Block Cube -> Block Cube
surface' model = model * cube' - model
  where
    cube' :: Block Cube
    cube' = block $ [Cube 0 x y z| x<-[-1..1], y<-[-1..1], z<-[-1..1]]

-- | Generate surface block
surface :: Block Cube -> Block Cube
surface model = model - (model - (surface' model) * cube2)
  where
    cube :: Block Cube
    cube = block $ [Cube 0 x y z| x<-[-1..1], y<-[-1..1], z<-[-1..1]]
    cube2 :: Block Cube
    cube2 = block $ [Cube 0 x y z| x<-[-2..2], y<-[-2..2], z<-[-2..2]]

-- | Generate house which is for demo.
house :: Block Cube
house = let house''  = (house' + square)*line
        in smap (12 * dz +)  $ surface house''
  where
    house' :: Block Cube
    house' = block $ [Cube 0 1 x y| x<-[-10..10], y<-[-10..10], y < x , y < (-x)]
    
    square :: Block Cube
    square = smap ((+) (-12 * dz)) $ block $ [Cube 0 1 x y| x<-[-5..5], y<-[-5..5]]
    
    line :: Block Cube
    line = block $ [Cube 0 x 0 0 | x<-[-5..5]]
