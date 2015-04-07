# Cube: Cubic DSL for 3D printing

[![Hackage version](https://img.shields.io/hackage/v/cube.svg?style=flat)](https://hackage.haskell.org/package/cube)  [![Build Status](https://travis-ci.org/junjihashimoto/cube.png?branch=master)](https://travis-ci.org/junjihashimoto/cube)

Cube is DSL for 3D printing.

This indents to make original blocks and prototypes for hobby.

This DSL is based on mathematical algebra.
Cube is the same as Quaternion.
Block is set of Cube. It allows boolian operations(and, subtruct and convolution).

## Getting started

Install this from Hackage.

    cabal update && cabal install cube

## Example

Block is converted to STL.
Block is the same as set of cube.
Following example shows example of DSL generating shape of house.


```
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
main :: IO ()
main = do
  writeFileStl "house.stl" $ house
```
