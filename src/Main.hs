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
type P1SCORE = MVar Int
type P2SCORE = MVar Int
type PICSARRAY = MVar Picture
type GameControl = (P1SCORE, P2SCORE, PICSARRAY)
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
    score :: (Int, Int),
    resetVelocity :: Bool
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
    score = (0,0),
    resetVelocity = False
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
render :: (GameState, GameControl) -> IO Picture
render (game, (_,_,infos)) = do
    additionalScreenInfo <- readMVar infos
    return( Pictures[allObjects, additionalScreenInfo])
    where
        allObjects = Pictures[  ball,
                                borders,
                                makePaddle white 300 $ player1H game,
                                makePaddle white (-300) $ player2H game
                            ]
        ball = uncurry translate (ballCoord game) $ color ballColor $ circleSolid ballRadius
        ballColor = white
        borders = Pictures [border 240 (dark $ dark blue), border (-240) (dark $ dark blue)]
        paddleColor = white

        makePaddle :: Color -> Float -> Float -> Picture
        makePaddle c x y = Pictures [   translate x y $ color c $ rectangleSolid paddleW paddleH,
                                        translate x y $ color c $ rectangleSolid paddleW paddleH]
        


-- Game Update
moveBall :: Float -> GameState -> GameState
moveBall seconds game = game { ballCoord = (x', y'), ballVeloc = (vx', vy')}
    where
        (x, y) = ballCoord game
        (vx, vy) = ballVeloc game
        x' = x + vx * seconds
        y' = y + vy * seconds
        (vx', vy') = if not(resetVelocity game)
                     then (vx * 1.0008, vy * 1.0001)
                     else (-100, 100)


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
                    then 2.25
                    else if currentDifficultyLevel == NIGHTMARE
                    then 3.75
                    else 6
        currentDifficultyLevel = aiDifficultyLevel game

-- Update Method
update :: Float -> (GameState, GameControl) -> IO (GameState, GameControl)
update seconds (gs, gc)= do
    updateScore(newGameState, gc)
    where
        newGameState = afterPaddleBounce
        afterPaddleBounce = paddleBounce afterWallBounce
        afterWallBounce = wallBounce afterMoveP1Paddle
        afterMoveP1Paddle = moveP1Paddle afterMoveP2Paddle
        afterMoveP2Paddle = moveP2Paddle afterMoveBall
        afterMoveBall = moveBall seconds gs

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
updateScore :: (GameState, GameControl) -> IO (GameState, GameControl)
updateScore (game, (p1s, p2s, fieldPics) ) = do
    score1 <- takeMVar p1s
    score2 <- takeMVar p2s
    
    if p1Scored
    then putMVar p1s (score1+1)
    else putMVar p1s score1

    if p2Scored
    then putMVar p2s (score2+1)
    else putMVar p2s score2
    
    return( (game { ballCoord = (a, b), resetVelocity = someoneScored}, (p1s, p2s, fieldPics)) )
    where
        (a, b) =    if someoneScored
                    then (0,0)
                    else ballCoord game
        p1Scored =  if outOfBounds (ballCoord game) == P2
                    then True
                    else False
        p2Scored =  if outOfBounds (ballCoord game) == P1
                    then True
                    else False
        someoneScored = p1Scored || p2Scored

outOfBounds :: Position -> WhichPlayer
outOfBounds (x, _)
    | round x > ( width `div` 2 ) = P1
    | round x < -( width `div` 2 ) = P2
    | otherwise = NEITHER


-- User input handling
handleKeys :: Event -> (GameState, GameControl) -> IO (GameState, GameControl)
handleKeys (EventKey (Char 'r') _ _ _) (game, control) = do
    return( (game {ballCoord = (0,0)}, control) )
handleKeys (EventKey (Char 'w') Down _ _) (game, control) = do
    return( (game {player1Dir = UP}, control) )
handleKeys (EventKey (Char 's') Down _ _) (game, control) = do
    return( (game {player1Dir = DOWN}, control) )
handleKeys (EventKey (Char 'w') Up _ _) (game, control) = do
    return( (game {player1Dir = NEUTRAL}, control) )
handleKeys (EventKey (Char 's') Up _ _) (game, control) = do
    return( (game {player1Dir = NEUTRAL}, control) )
handleKeys (EventKey (Char '1') _ _ _) (game, control) = do
    return( (game {aiDifficultyLevel = EASY}, control) )
handleKeys (EventKey (Char '2') _ _ _) (game, control) = do
    return( (game {aiDifficultyLevel = MEDIUM}, control) )
handleKeys (EventKey (Char '3') _ _ _) (game, control) = do
    return( (game {aiDifficultyLevel = HARD}, control) )
handleKeys (EventKey (Char '4') _ _ _) (game, control) = do
    return( (game {aiDifficultyLevel = NIGHTMARE}, control) )
handleKeys (EventKey (Char '5') _ _ _) (game, control) = do
    return( (game {aiDifficultyLevel = AREUCRAZYMAN}, control) )
handleKeys _ (game, control) = do
    return( (game, control) )

-- Main event loop
main :: IO ()
main = do 
    p1Score <- newMVar 0
    p2Score <- newMVar 0
    reloadBall <- newMVar False
    infos <- newMVar middleFieldStrip
    forkIO(scoreBoard (p1Score, p2Score, infos))
    playIO window background fps (initialState, (p1Score, p2Score, infos)) render handleKeys update

-- Scoreboard Thread - responsible for keeping the score updated and creating scoreboard graphics
{-
scoreBoard :: MVar Int -> MVar Int -> IO Pictures
scoreBoard mvp1 mvp2 = do
    s1 <- takeMVar mvp1
    s2 <- takeMvar mvp2
-}

middleFieldStrip :: Picture
middleFieldStrip = Pictures[
                            translate 0 180 $ color white $ rectangleSolid 3 40,
                            translate 0 120 $ color white $ rectangleSolid 3 40,
                            translate 0 60 $ color white $ rectangleSolid 3 40,   
                            color white $ rectangleSolid 3 40,
                            translate 0 (-60) $ color white $ rectangleSolid 3 40,
                            translate 0 (-120) $ color white $ rectangleSolid 3 40,
                            translate 0 (-180) $ color white $ rectangleSolid 3 40
                        ]

scoreBoard :: GameControl -> IO()
scoreBoard (p1s, p2s, infos) = do
    scoreP1 <- takeMVar p1s
    scoreP2 <- takeMVar p2s
    screenInfos <- takeMVar infos
    
    if(scoreP1 > 7 || scoreP2 > 7)
    then putMVar p1s 0
    else putMVar p1s scoreP1

    if(scoreP2 > 7 || scoreP1 > 7)
    then putMVar p2s 0
    else putMVar p2s scoreP2

    updP1Score <- readMVar p1s
    updP2Score <- readMVar p2s

    putMVar infos (generateNewImage screenInfos updP1Score updP2Score)

    -- Dorme por 600ms (reduz Lag causado pela thread do placar)
    threadDelay 600000

    scoreBoard (p1s, p2s, infos)
    where
        generateNewImage :: Picture -> Int -> Int -> Picture
        generateNewImage pic p1s p2s = fullPicture
            where
                fullPicture = Pictures[
                                        makeScore 260 200 p1s,
                                        makeScore (-260) 200 p2s,
                                        middleFieldStrip
                                        ]

                makeScore :: Float -> Float -> Int -> Picture
                makeScore x y s = translate x y $ scale 0.25 0.25 $ color white $ text(show s)


{- OLD AI MANUAL CONTROLS - USE IN CASE OF ROBOT INSURGENCY
handleKeys (EventKey (Char 'o') Down _ _) game = game {player2Dir = UP}
handleKeys (EventKey (Char 'l') Down _ _) game = game {player2Dir = DOWN}
handleKeys (EventKey (Char 'o') Up _ _) game = game {player2Dir = NEUTRAL}
handleKeys (EventKey (Char 'l') Up _ _) game = game {player2Dir = NEUTRAL}
-}