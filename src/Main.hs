module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Monad
import System.Random
import System.IO

-- Game Structure
type Radius = Float
type Position = (Float, Float)
data Direction = UP | DOWN | NEUTRAL deriving(Show, Eq)
data Difficulty = EASY | MEDIUM | HARD | NIGHTMARE | AREUCRAZYMAN deriving(Show, Eq)
data WhichPlayer = P1 | P2 | NEITHER deriving(Show, Eq)
data GameState = Game{
    ballCoord :: (Float, Float),
    ballVeloc :: (Float, Float),
    player1H :: Float,
    player2H :: Float,
    player1Dir :: Direction,
    player2Dir :: Direction,
    aiDifficultyLevel :: Difficulty,
    score :: (Int, Int)
} deriving Show

initialState :: GameState
initialState = Game{
    ballCoord = (-10, 30),
    ballVeloc = (-100, 100),
    player1H = 40,
    player2H = 0,
    player1Dir = NEUTRAL,
    player2Dir = NEUTRAL,
    aiDifficultyLevel = EASY,
    score = (0,0)
}

-- Game Screen Parameters
fps :: Int
fps = 60

width :: Int
width = 640

height :: Int
height = 480

offset :: Int
offset = 100

border :: Float -> Color -> Picture
border x c = translate 0 x $ color c $ rectangleSolid 610 10

window :: Display
window = InWindow "Haskong" (width, height) (offset, offset)

background :: Color
background = black

-- Game object parameters
ballRadius :: Float
ballRadius = 8

paddleW :: Float
paddleW = 10

paddleH :: Float
paddleH = 80
-- Render game scene
render :: GameState -> IO Picture
render game = do
    return(allObjects)
    where
        allObjects = Pictures[  ball,
                                borders,
                                makePaddle white 300 $ player1H game,
                                makePaddle white (-300) $ player2H game,
                                makeScore 260 200 (fst(score game)),
                                makeScore (-260) 200 (snd(score game))
                            ]
        ball = uncurry translate (ballCoord game) $ color ballColor $ circleSolid ballRadius
        ballColor = white
        borders = Pictures [border 240 (dark $ dark blue), border (-240) (dark $ dark blue)]
        paddleColor = white

        makePaddle :: Color -> Float -> Float -> Picture
        makePaddle c x y = Pictures [   translate x y $ color c $ rectangleSolid paddleW paddleH,
                                        translate x y $ color c $ rectangleSolid paddleW paddleH]
        
        makeScore :: Float -> Float -> Int -> Picture
        makeScore x y s = translate x y $ scale 0.25 0.25 $ color white $ text(show s)

-- Game Update
moveBall :: Float -> GameState -> GameState
moveBall seconds game = game { ballCoord = (x', y'), ballVeloc = (vx', vy')}
    where
        (x, y) = ballCoord game
        (vx, vy) = ballVeloc game
        x' = x + vx * seconds
        y' = y + vy * seconds
        vx' = vx * 1.001
        vy' = vy * 1.001

moveP1Paddle :: GameState -> GameState
moveP1Paddle game = game { player1H = h}
    where
        h = if(currentMovement == UP)
            then currentHeight + 5
            else if(currentMovement == DOWN)
            then currentHeight - 5
            else currentHeight
        currentHeight = player1H game
        currentMovement = player1Dir game
{--
moveP2Paddle :: GameState -> GameState
moveP2Paddle game = game { player2H = h}
    where
        h = if(currentMovement == UP)
            then currentHeight + 5
            else if(currentMovement == DOWN)
            then currentHeight - 5
            else currentHeight
        currentHeight = player2H game
        currentMovement = player2Dir game

--}

-- AI TEST
moveP2Paddle :: GameState -> GameState
moveP2Paddle game = game { player2H = h}
    where
        h = if(currentHeight > snd(ballCoord game))
            then currentHeight - aiSpeed
            else currentHeight + aiSpeed
        currentHeight = player2H game
        aiSpeed = getAIspeed game

getAIspeed :: GameState -> Float
getAIspeed game = aiLevel
    where
        aiLevel =   if currentDifficultyLevel == EASY
                    then 1
                    else if currentDifficultyLevel == MEDIUM
                    then 1.5
                    else if currentDifficultyLevel == HARD
                    then 2
                    else if currentDifficultyLevel == NIGHTMARE
                    then 2.5
                    else 5
        currentDifficultyLevel = aiDifficultyLevel game

-- Update Method
update :: Float -> GameState -> IO GameState
update seconds = do
    updateScore . paddleBounce . wallBounce . moveP1Paddle . moveP2Paddle . moveBall seconds

-- Collision detection and treatment
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
        where
            topCollision = y - radius <= -fromIntegral height/2
            bottomCollision = y + radius >= fromIntegral height/2

wallBounce :: GameState -> GameState
wallBounce game = game { ballVeloc = (vx, vy') }
    where
        radius = ballRadius
        (vx, vy) = ballVeloc game
        vy' =   if wallCollision (ballCoord game) radius
                then
                    -vy
                else
                    vy

paddleCollision :: Position -> Radius -> Float -> Float -> Bool
paddleCollision (x, y) radius p1height p2height = p1PaddleCollision || p2PaddleCollision
        where
            p1PaddleCollision = (x + radius >= 300) && y < p1height+paddleH/2 && y > p1height-paddleH/2
            p2PaddleCollision = (x - radius <= -300) && y < p2height+paddleH/2 && y > p2height-paddleH/2
paddleBounce :: GameState -> GameState
paddleBounce game = game { ballVeloc = (vx', vy) }
    where
        radius = ballRadius
        (vx, vy) = ballVeloc game
        vx' =   if paddleCollision (ballCoord game) radius (player1H game) (player2H game) 
                then
                    -vx
                else
                    vx

{-- MORE DYNAMIC PADDLEBOUNCE TEST - IT IS TURNING THE BALL INTO A MAYBE GHOST
paddleBounce :: GameState -> GameState
paddleBounce game = game { ballVeloc = (vx', vy') }
    where
        radius = ballRadius
        (x, y) = ballCoord game
        (vx, vy) = ballVeloc game
        (vx', vy') =    if (paddleCollision (ballCoord game) radius (player1H game) (player2H game)) && x > 0 
                        then ( (modifierP1 * (-150) * cos(phiP1)) , (modifierP1 * 150 * sin(phiP1)) )
                        else if paddleCollision (ballCoord game) radius (player1H game) (player2H game)
                        then ( (modifierP2 * (150) * cos(phiP2)) , (modifierP2 * 150 * sin(phiP2)) )
                        else
                            (vx, vy)
        phiP1 =  (0.25 * pi * (2*n1 -1)) * pi/180
        n1 = (y + ballRadius - ((player1H game)+paddleH/2))/(paddleH + 2 * ballRadius)
        phiP2 = (0.25 * pi * (2*n2 -1)) * pi/180
        n2 = (y + ballRadius - ((player2H game)+paddleH/2))/(paddleH + 2 * ballRadius)
        modifierP1 =if(abs(phiP1) > 0.2*pi)
                    then 1.5
                    else 1
        modifierP2 =if(abs(phiP2) > 0.2*pi)
                    then 1.5
                    else 1
--}

-- Score Update
updateScore :: GameState -> IO GameState
updateScore game = do
    return(game { score = (x', y') })
    where
        (x', y') = (p1Score, p2Score)
        p1Score =   if outOfBounds (ballCoord game) == P2
                    then currentP1Score+1
                    else currentP1Score
        p2Score =   if outOfBounds (ballCoord game) == P1
                    then currentP2Score+1
                    else currentP2Score 
        (currentP1Score, currentP2Score) = score game  

outOfBounds :: Position -> WhichPlayer
outOfBounds (x, _)
    | round x > ( width `div` 2 ) = P1
    | round x < -( width `div` 2 ) = P2
    | otherwise = NEITHER


-- User input handling
handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (Char 'r') _ _ _) game = do
    return(game {ballCoord = (0,0)})
handleKeys (EventKey (Char 'w') Down _ _) game = do
    return(game {player1Dir = UP})
handleKeys (EventKey (Char 's') Down _ _) game = do
    return(game {player1Dir = DOWN})
handleKeys (EventKey (Char 'w') Up _ _) game = do
    return(game {player1Dir = NEUTRAL})
handleKeys (EventKey (Char 's') Up _ _) game = do
    return(game {player1Dir = NEUTRAL})
handleKeys (EventKey (Char '1') _ _ _) game = do
    return(game {aiDifficultyLevel = EASY})
handleKeys (EventKey (Char '2') _ _ _) game = do
    return(game {aiDifficultyLevel = MEDIUM})
handleKeys (EventKey (Char '3') _ _ _) game = do
    return(game {aiDifficultyLevel = HARD})
handleKeys (EventKey (Char '4') _ _ _) game = do
    return(game {aiDifficultyLevel = NIGHTMARE})
handleKeys (EventKey (Char '5') _ _ _) game = do
    return(game {aiDifficultyLevel = AREUCRAZYMAN})
handleKeys _ game = do
    return(game)

-- Main event loop
main :: IO ()
main = do 
    --p1Score <- newMVar 0
    --p2Score <- newMVar 0
    --reloadBall <- newMVar 1
    --forkIO(scoreBoard p1Score p2Score)
    playIO window background fps initialState render handleKeys update

-- Scoreboard Thread - responsible for keeping the score updated and creating scoreboard graphics
{-
scoreBoard :: MVar Int -> MVar Int -> IO Pictures
scoreBoard mvp1 mvp2 = do
    s1 <- takeMVar mvp1
    s2 <- takeMvar mvp2
-}    


{- OLD AI MANUAL CONTROLS - USE IN CASE OF ROBOT INSURGENCY
handleKeys (EventKey (Char 'o') Down _ _) game = game {player2Dir = UP}
handleKeys (EventKey (Char 'l') Down _ _) game = game {player2Dir = DOWN}
handleKeys (EventKey (Char 'o') Up _ _) game = game {player2Dir = NEUTRAL}
handleKeys (EventKey (Char 'l') Up _ _) game = game {player2Dir = NEUTRAL}
-}