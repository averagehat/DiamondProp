module Lib
    ( someFunc
    , diamond
    ) where
import Data.List (transpose, intercalate)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

diamond c = intercalate "\n" $ (map f ['A'..c]) ++ (map f $ tail $ reverse ['A'..c])
  where f = (:"")
