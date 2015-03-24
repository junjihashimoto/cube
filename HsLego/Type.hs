
data Rotate =
    R0
  | R90
  | R180

class Block a where
  sx ::  a ->  Int
  sy ::  a ->  Int
  sz ::  a ->  Int
  x  ::  a ->  Int
  y  ::  a ->  Int
  z  ::  a ->  Int
  rx ::  a ->  Rotate
  ry ::  a ->  Rotate
  rz ::  a ->  Rotate
  color ::  a ->  (Int,Int,Int)

