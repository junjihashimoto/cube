{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE InstanceSigs#-}
import Prelude
--hiding (map)
--import Data.Set
import Data.List
import Data.Monoid
import Control.Applicative
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Graphics.Formats.STL

import Language.Cube

main :: IO ()
main = do
  writeFileStl "house.stl" $ house +  (smap ((dr (pi/2) dz) *) (smap ((24 * dx) +) house))
