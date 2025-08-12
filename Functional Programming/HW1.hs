{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise3

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

--botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: Integer -> Picture
trafficLight 3  = botCircle green & midCircle black & topCircle black & frame
trafficLight 2 = botCircle black & midCircle yellow & topCircle black & frame
trafficLight 1 = botCircle black & midCircle black & topCircle red & frame
trafficLight 0 = botCircle black & midCircle yellow & topCircle red & frame

trafficController :: Double -> Picture
trafficController t
  | round (t/2) `mod` 8 < 3  = trafficLight 3
  | round (t/2) `mod` 8 == 3 = trafficLight 2
  | round (t/2) `mod` 8 < 7  = trafficLight 1
  | otherwise                = trafficLight 0

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

flower :: Double ->Color -> Picture
flower t c = colored c (solidCircle ((min t 10)/50))

mixFlower :: Double -> Picture
mixFlower t
  | t < 5 = flower (t) yellow & flower (t/2) red
  | otherwise = flower (min (t/2 + 2.5) 7.5) yellow & flower (min (2*t - 7.5) 12.5) red 

tree :: Picture -> Integer -> Picture
tree f 0 = f
tree f n = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree f (n-1)) & rotated (- pi/10) (tree f (n-1)))

myTree :: Double -> Picture 
myTree t = tree (mixFlower t) 8

exercise2 :: IO ()
exercise2 = animationOf myTree

-- Exercise 3

floorColor, goalColor :: Color
floorColor = yellow
goalColor = red

wallTex, boxTex :: Picture
wallTex = polyline [((-0.5),(1/6)),((0.5),(1/6))] & polyline [((-0.5),(-1/6)),((0.5),(-1/6))] & 
          polyline [(-1/8,0.5),(-1/8,(1/6))] & polyline [(1/6,(1/6)),(1/6,(-1/6))] & polyline [(-1/4,(-1/6)),(-1/4,(-0.5))]
boxTex = polyline [(-3/8,-3/8),(3/8,3/8)] & polyline [(-3/8,3/8),(3/8,-3/8)] & 
          rectangle (3/4) (3/4) & colored (dark brown) (solidRectangle (3/4) (3/4))       

wall, ground, storage, box :: Picture
wall =    wallTex & rectangle 1 1 & (colored grey (solidRectangle 1 1))
ground =  colored floorColor (solidRectangle 1 1)
storage = colored goalColor (solidCircle 0.15) & ground 
box =     boxTex & rectangle 1 1 & (colored brown (solidRectangle 1 1))

maze_lim_x,maze_lim_y :: Integer
maze_lim_x = 5
maze_lim_y = 5
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

drawTile :: Integer -> Picture
drawTile t
 | t == 1    = wall
 | t == 2    = ground
 | t == 3    = storage
 | t == 4    = box
 | otherwise = blank

drawMaze :: Integer -> Integer -> Picture
drawMaze r c
  | r == (-maze_lim_x) && c == (-maze_lim_y) = drawTile (maze (-maze_lim_x) (-maze_lim_y))
  | c == (-maze_lim_y) = translated (-1) 10 (drawMaze (r-1) maze_lim_y) & drawTile (maze r (-maze_lim_y))
  | otherwise = translated 0 (-1) (drawMaze r (c-1)) & drawTile (maze r c)

pictureOfMaze :: Picture
pictureOfMaze = drawMaze 5 5

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
