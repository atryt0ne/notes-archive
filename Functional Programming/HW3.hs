{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

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

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))

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


-- Lists

data List a = Empty | Entry a (List a)
    
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates
data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x y) (C z w)
  | (x == z) && (y==w) = True
  | otherwise = False

moveThisTo :: Coord -> Coord -> Coord -> Coord
moveThisTo c1 c2 c
  | eqCoord c1 c = c2
  | otherwise = c

goTo :: State -> Direction -> State
goTo (S from _ boxes) dir
  = case cur to of
    Box -> case cur fut of
            Ground -> newState
            Storage -> newState
            _ -> noChange
    Ground -> newState
    Storage -> newState
    _ -> noChange
  where to = adjCoord dir from
        fut = adjCoord dir to
        cur = mazeWithBoxes boxes
        newBox = mapList (moveThisTo to fut) boxes
        newState = S to dir newBox 
        noChange = S from dir boxes


-- The maze       
maze :: Coord -> Tile 
maze (C x y)
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

noBoxMaze :: Coord -> Tile
noBoxMaze (C x y)
  | x < -3 || x > 4 || y < -1 || y > 5 = Blank
  | (y == 2 && (x == 0 || x == -1)) || (y == 3 && x == -1) = Storage
  | (x == 1) && (y == 1 || y == 2)      = Ground
  | y == 0 && x `elem` [-2, -1, 0, 1]   = Ground
  | y == 1 && x `elem` [-2,-1, 2, 3]    = Ground
  | y == 2 && x `elem` [-2, 2, 3]       = Ground
  | y == 3 && x == 3                    = Ground
  | y == 3 && x == 2                    = Ground
  | y == 4 && x `elem` [-1,0,1,2]       = Ground
  -- Any remaining coordinate inside the boundary is a Wall
  | otherwise = Wall          

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty query = noBoxMaze query
mazeWithBoxes (Entry c a) query
  | eqCoord c query = Box
  | otherwise = mazeWithBoxes a query

-- The state

data State = S Coord Direction (List Coord)

initialBoxes :: List Coord
initialBoxes = go (-10) (-10)
  where
    go 11 11 = Empty
    go x 11 = go (x+1) (-10)
    go x y = case maze (C x y) of
      Box -> Entry (C x y) (go x (y+1))
      _   ->                go x (y+1)   

initialState :: State
initialState = S (C 0 0) R initialBoxes

-- Event handling

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
  | isWon s = s
  | key == "Right" = goTo s R
  | key == "Left" = goTo s L
  | key == "Up" = goTo s U
  | key == "Down" = goTo s D
handleEvent _ s = s

player :: Direction -> Picture
player R = translated 0 0.3 cranium
        & polyline [(0,0),(0.3,0.05)] 
      & polyline [(0,0),(0.3,-0.05)] 
      & polyline [(0,-0.2),(0,0.1)] 
      & polyline [(0,-0.2),(0.1,-0.5)]
      & polyline [(0,-0.2),(-0.1,-0.5)]
 where cranium = circle 0.18 & colored (dark blue) (sector (7/6*pi) (1/6*pi) 0.18)
player L = reflected (pi/2) (player R)
player U = rotated (pi/2) (player R)
player D = rotated (-pi/2) (player R)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState (S c d boxes)
  = drawWin (isWon (S c d boxes))
  & atCoord c (player d)
  & pictureOfBoxes boxes
  & pictureOfMaze

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState handleEvent drawState

-- The general interaction type

data Interaction world = Interaction
        world
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 handle draw)
  = activityOf state0 handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 handle draw)
  = Interaction state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")
              & translated 0 (-4) (lettering "Press <Space> to Start")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 handle draw)
  = Interaction state0' handle' draw'
  where
    state0' = StartScreen
        
    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

-- Winning

allList :: List Bool -> Bool
allList Empty = True
allList (Entry a b) = (a == True) && (allList b) 

isOnStorage :: Coord -> Bool
isOnStorage c = case maze c of
                  Storage -> True
                  _ -> False

isWon :: State -> Bool
isWon (S _ _ boxes) = allList (mapList isOnStorage boxes)

winScreen = translated 1 (-3) (scaled 2 2 (lettering "You Win!"))

drawWin :: Bool -> Picture
drawWin True = winScreen
drawWin False = blank

-- The main function

main :: IO ()
main = runInteraction (resetable (withStartScreen sokoban))