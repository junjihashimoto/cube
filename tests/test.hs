{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE ScopedTypeVariables#-}

import Test.Hspec
import Language.Cube
import Graphics.Formats.STL

-- | Generate house which is for demo.
house :: Block Cube
house = let house''  = (house' + square)*line
        in rfmap (12 * dz +) house''
  where
    house' :: Block Cube
    house' = block $ [cube 1 x y| x<-[-10..10], y<-[-10..10], y < x , y < (-x)]
    
    square :: Block Cube
    square = rfmap ((+) (-12 * dz)) $ block $ [cube 1 x y| x<-[-5..5], y<-[-5..5]]
    
    line :: Block Cube
    line = block $ [cube x 0 0 | x<-[-5..5]]

main :: IO ()
main = 
  hspec $ do
    describe "Cube(quoternion)" $ do
      it "dx" $ do
        cube 1 0 0 `shouldBe` dx
      it "dy" $ do
        cube 0 1 0 `shouldBe` dy
      it "dz" $ do
        cube 0 0 1 `shouldBe` dz
      it "dx +  dx" $ do
        dx + dx `shouldBe` cube 2 0 0 
      it "2 * dx" $ do
        2 * dx `shouldBe` cube 2 0 0 
      it "-2 * dx" $ do
        (-2) * dx `shouldBe` cube (-2) 0 0 
      it "route pi/2" $ do
        (dR (pi/2) dz dx) `shouldBe` cube 0 1 0
      it "route pi" $ do
        (dR (pi) dz dx) `shouldBe` cube (-1) 0 0
      it "route 3*pi/2" $ do
        (dR (3*pi/2) dz dx) `shouldBe` cube 0 (-1) 0
      it "route 2*pi" $ do
        (dR (2*pi) dz dx) `shouldBe` cube 1 0 0
      it "multi" $ do
        dx * dy `shouldBe` (dz :: Cube)
      it "multi" $ do
        dy * dx `shouldBe` (-dz :: Cube)
    describe "Block" $ do
      it "dx" $ do
        block [dx,dx] `shouldBe` (block [dx] :: Block Cube)
      it "dx+dy" $ do
        block [dx] + block [dy] `shouldBe` (block [dx,dy] :: Block Cube)
      it "[dx,dy]-dy" $ do
        block [dx,dy] - block [dy] `shouldBe` (block [dx] :: Block Cube)
      it "convolution(dx * [-2*dx,2*dx])" $ do
        block [dx] * block [-2*dx,2*dx] `shouldBe` (block [-dx,3*dx] :: Block Cube)
    describe "Compaction" $ do
      it "num triangle of one block" $ do
        length (triangles (toSTL (block [dx] :: Block Cube))) `shouldBe` 12
      it "num triangle of two block" $ do
        length (triangles (toSTL (block [dx,2*dx] :: Block Cube))) `shouldBe` 20
      it "flip triangle" $ do
        (flipTriangle (Triangle Nothing ((5.0,0,0),(1,0,0),(2,0,0)))) `shouldBe` (Triangle Nothing ((5.0,0,0),(2,0,0),(1,0,0)))
    describe "ToSTL" $ do
      it "Block Cube" $ do
        writeFileStl "tmp.stl" (block [dx] :: Block Cube) `shouldReturn` ()
        writeFileStl "house.stl" house `shouldReturn` ()
