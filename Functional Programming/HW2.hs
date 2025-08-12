{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main :: IO ()
main = exercise3

-- Previous Week

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

data Tile = Wall | Ground | Storage | Box | Blank
data Direction = R | U | L | D
data Coord = C Integer Integer

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjCoord :: Direction -> Coord -> Coord
adjCoord R (C x y) = C (x+1) y
adjCoord U (C x y) = C  x   (y+1)
adjCoord L (C x y) = C (x-1) y
adjCoord D (C x y) = C  x   (y-1)

maze_lim_x,maze_lim_y :: Integer
maze_lim_x = 5
maze_lim_y = 5
maze1 :: Coord -> Tile 
maze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

maze2 :: Coord -> Tile
maze2 (C x y)
  | x < -3 || x > 4 || y < -1 || y > 5 = Blank
  | (y == 2 && (x == 0 || x == -1)) || (y == 3 && x == -1) = Storage
  | (x == 1) && (y == 1 || y == 2)      = Box
  | y == 0 && x `elem` [-2, -1, 0, 1]   = Ground
  | y == 1 && x `elem` [-2,-1, 2, 3]    = Ground
  | y == 2 && x `elem` [-2, 2, 3]       = Ground
  | y == 3 && x == 3                    = Ground
  | y == 3 && x == 2                    = Box
  | y == 4 && x `elem` [-1,0,1,2]       = Ground
  -- Any remaining coordinate inside the boundary is a Wall
  | otherwise = Wall
 
-- Alias to maze
maze = maze2


drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (maze c))

draw21 :: (Integer -> Picture) -> Picture
draw21 something = go (-10)
  where
    go 11 = blank
    go n = something n & go (n+1)
{-
drawMaze :: Integer -> Integer -> Picture
drawMaze r c
  | r == (-maze_lim_x) && c == (-maze_lim_y) = drawTile (maze (-maze_lim_x) (-maze_lim_y))
  | c == (-maze_lim_y) = translated (-1) 10 (drawMaze (r-1) maze_lim_y) & drawTile (maze r (-maze_lim_y))
  | otherwise = translated 0 (-1) (drawMaze r (c-1)) & drawTile (maze r c)-}

pictureOfMaze = draw21 (\r -> draw21 (\c -> drawTileAt (C r c)))

exercise0 :: IO ()
exercise0 = drawingOf pictureOfMaze

-- Exercise 1 and 2
player :: Picture
player = translated 0 0.3 cranium
        & polyline [(0,0),(0.3,0.05)] 
      & polyline [(0,0),(0.3,-0.05)] 
      & polyline [(0,-0.2),(0,0.1)] 
      & polyline [(0,-0.2),(0.1,-0.5)]
      & polyline [(0,-0.2),(-0.1,-0.5)]
 where cranium = circle 0.18 & colored (dark blue) (sector (7/6*pi) (1/6*pi) 0.18)

move :: StatePair -> Picture -> Picture
move (P R c) pic = atCoord c (rotated 0 pic)
move (P L c) pic = atCoord c (reflected (pi/2) pic)
move (P U c) pic = atCoord c (rotated (pi/2) pic)
move (P D c) pic = atCoord c (rotated (-pi/2) pic)

handleEvent :: Event -> StatePair -> StatePair
handleEvent (KeyPress key) (P dir c)
  | key == "Right" = P R (goTo c R)
  | key == "Left" = P L (goTo c L)
  | key == "Up" = P U (goTo c U)
  | key == "Down" = P D (goTo c D)
handleEvent _ (P dir c) = P dir c

isOk :: Tile -> Bool
isOk Ground = True
isOk Storage = True
isOk _ = False

goTo :: Coord -> Direction -> Coord
goTo from dir
  | isOk (maze to) = to
  | otherwise = from
  where to = adjCoord dir from

drawState :: StatePair -> Picture
drawState p = move p player & pictureOfMaze

data StatePair = P Direction Coord

exercise2 :: IO ()
exercise2 = activityOf (P L (C 0 1)) handleEvent drawState

-- Exercise 3
resetableActivityOf :: StatePair -> (Event->StatePair->StatePair)->(StatePair->Picture)->IO()
resetableActivityOf initState handler drawer = activityOf initState resetHandler drawer
  where resetHandler (KeyPress "Esc") (P dir c) = initState
        resetHandler otherEvent state = handler otherEvent state

exercise3 :: IO ()
exercise3 = resetableActivityOf (P L (C 0 0)) handleEvent drawState
