import Language.Cube

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
main = do
  writeFileStl "house.stl" $ house
