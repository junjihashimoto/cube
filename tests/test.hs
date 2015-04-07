{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

import Test.Hspec
import Language.Cube

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
        (dr (pi/2) dz) * dx `shouldBe` cube 0 1 0
      it "route pi" $ do
        (dr (pi) dz) * dx `shouldBe` cube (-1) 0 0
      it "route 3*pi/2" $ do
        (dr (3*pi/2) dz) * dx `shouldBe` cube 0 (-1) 0
      it "route 2*pi" $ do
        (dr (2*pi) dz) * dx `shouldBe` cube 1 0 0
      it "multi" $ do
        dx * dy `shouldBe` dz
      it "multi" $ do
        dy * dx `shouldBe` (-dz)
    describe "Block" $ do
      it "dx" $ do
        block [dx,dx] `shouldBe` block [dx]
      it "dx+dy" $ do
        block [dx] + block [dy] `shouldBe` block [dx,dy]
      it "[dx,dy]-dy" $ do
        block [dx,dy] - block [dy] `shouldBe` block [dx]
      it "convolution(dx * [-2*dx,2*dx])" $ do
        block [dx] * block [-2*dx,2*dx] `shouldBe` block [-dx,3*dx]
    describe "ToSTL" $ do
      it "Block Cube" $ do
        writeFileStl "tmp.stl" (block [dx] :: Block Cube) `shouldReturn` ()
        writeFileStl "house.stl" house `shouldReturn` ()
