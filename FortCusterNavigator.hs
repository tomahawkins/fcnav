module Main (main) where

import Text.Printf

main :: IO ()
main = writeFile "FortCusterNavigator.svg" $ unlines
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
  , "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
  , rect 10 10 565 740
  , matrix 25 25 maze
  , "</svg>"
  ]

rect :: Double -> Double -> Double -> Double -> String
rect = printf "<rect x=\"%f\" y=\"%f\" width=\"%f\" height=\"%f\" style=\"fill: none; stroke: black;\" />"

square :: Double -> Double -> String
square x y = rect x y 10 10

matrix :: Double -> Double -> [[Bool]] -> String
matrix x y m = unlines
  [ unlines
    [ square x y
    | (x, cell) <- zip [x, x + 25 ..] row, cell
    ]
  | (y, row) <- zip [y, y + 25 ..] m
  ]

row :: [Int] -> [Bool]
row a = concat [ replicate n a | (n, a) <- zip a $ concat $ repeat [True, False] ]

maze :: [[Bool]]
maze =
  [ row [10, 1, 11]
  , row [10, 1, 11]
  ] ++
  replicate 6 (row [6, 10, 6]) ++
  replicate 8 (replicate 22 True) ++
  replicate 4 (row [9, 4, 9]) ++
  replicate 9 (replicate 22 True)


