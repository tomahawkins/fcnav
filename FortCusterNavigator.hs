module Main (main) where

import Text.Printf

main :: IO ()
main = writeFile "FortCusterNavigator.svg" $ unlines
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
  , "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
  , svg $ scale 6 $ move (2, 2) maze
  , "</svg>"
  ]

data Rect = Rect
  { x
  , y
  , width
  , height :: Double
  , fill   :: String
  , stroke :: String
  }

type Image = [Rect]

svg :: Image -> String
svg img = unlines [ printf "<rect x=\"%f\" y=\"%f\" width=\"%f\" height=\"%f\" style=\"fill: %s; stroke: %s;\" />" x y w h f s | Rect x y w h f s <- img ]

scale :: Double -> Image -> Image
scale n img = [ Rect (x * n) (y * n) (w * n) (h * n) f s | Rect x y w h f s <- img ]

move :: (Double, Double) -> Image -> Image
move (x', y') img = [ Rect (x + x') (y + y') w h f s | Rect x y w h f s <- img ]

rect :: Double -> Double -> Double -> Double -> Image
rect x y w h = [Rect x y w h "none" "black"]

square :: Double -> Double -> Image
square x y = rect x y 2 2

matrix :: Double -> Double -> [[Bool]] -> Image
matrix x y m = concat
  [ concat
    [ square x y
    | (x, cell) <- zip [x, x + 5 ..] row, cell
    ]
  | (y, row) <- zip [y, y + 5 ..] m
  ]

row :: [Int] -> [Bool]
row a = concat [ replicate n a | (n, a) <- zip a $ concat $ repeat [True, False] ]

posts :: Image
posts = matrix 3 3 $
  [ row [10, 1, 11]
  , row [10, 1, 11]
  ] ++
  replicate 6 (row [6, 10, 6]) ++
  replicate 8 (replicate 22 True) ++
  replicate 4 (row [9, 4, 9]) ++
  replicate 9 (replicate 22 True)

walls :: Image
walls = rect 0 0 113 148

bridges :: Image
bridges = northBridge ++ westBridge ++ eastBridge
  where
  northBridge =
    [ Rect { x =  7 * 2 +  8 * 3, y = 11 * 2 + 12 * 3, width =  5, height =  2, fill = "dimgray",   stroke = "none" }
    , Rect { x = 14 * 2 + 14 * 3, y = 11 * 2 + 12 * 3, width =  5, height =  2, fill = "dimgray",   stroke = "none" }
    , Rect { x =  8 * 2 +  9 * 3, y = 11 * 2 + 12 * 3, width = 27, height =  2, fill = "lightgray", stroke = "none" }
    ]
  westBridge =
    [ Rect { x =  4 * 2 +  5 * 3, y = 15 * 2 + 16 * 3, width =  2, height =  5, fill = "dimgray",   stroke = "none" }
    , Rect { x =  4 * 2 +  5 * 3, y = 20 * 2 + 20 * 3, width =  2, height =  5, fill = "dimgray",   stroke = "none" }
    , Rect { x =  4 * 2 +  5 * 3, y = 16 * 2 + 17 * 3, width =  2, height = 17, fill = "lightgray", stroke = "none" }
    ]
  eastBridge = move (13 * 5, 0) westBridge

tower :: Image
tower = rect (10 * 2 + 10 * 3 + 1) (17 * 2 + 17 * 3 + 1) 11 11

maze :: Image
maze = bridges ++ posts ++ walls ++ tower

