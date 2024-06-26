{-# OPTIONS -Wall -Werror #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.GL.Compatibility30
import Graphics.UI.GLFW as GLFW
import Control.Monad (forM_)
import System.Exit (exitWith, ExitCode(..))
import Data.Time.Clock
import System.Random
import Vector2

-- PARAMETERS --

windowSize :: Int
windowSize = 1080

particleRadius :: Float
particleRadius = 0.01

numParticles :: Int
numParticles = 500

gravity :: Float
gravity = 0

timeStep :: TimeStep
timeStep = RealTime
-- timeStep = Fixed 0.02

clearColor :: (Float, Float, Float)
clearColor = (0, 0, 0.1)

collisionDamping :: Float
collisionDamping = 0.7

slowdownConstant :: Float
slowdownConstant = 0.5

setGlColor :: Color -> IO ()
setGlColor = \case
  Red    -> glColor3f 1.0 0.4 0.2
  Green  -> glColor3f 0.2 1.0 0.5
  Blue   -> glColor3f 0.2 0.5 1.0
  Yellow -> glColor3f 0.9 0.9 0.2

-- should be between 0 and 1
repulsionRadius :: Float
repulsionRadius = 0.3

data Color = Red | Green | Blue | Yellow
  deriving (Eq, Show, Enum, Bounded)

attractionFactor :: Color -> Color -> Float
attractionFactor a b = case (a, b) of
  (Red   , Red)    ->  0.6
  (Red   , Green)  -> -0.3
  (Red   , Blue)   ->  0.2
  (Red   , Yellow) -> -0.4
  (Green , Green)  ->  2.0
  (Green , Blue)   ->  0.3
  (Green , Yellow) -> -0.2
  (Blue  , Blue)   -> -0.4
  (Blue  , Yellow) ->  0.5
  (Yellow, Yellow) -> -0.2
  -- repeats of above, but want to enumerate all cases for totality checking
  (Green , Red)   -> swap
  (Blue  , Red)   -> swap
  (Blue  , Green) -> swap
  (Yellow, Red)   -> swap
  (Yellow, Green) -> swap
  (Yellow, Blue)  -> swap
  where
    swap = attractionFactor b a

-- END PARAMETERS --

main :: IO ()
main = do
  _ <- GLFW.init
  GLFW.defaultWindowHints
  -- we don't support non-square aspect ratios
  Just window <- GLFW.createWindow windowSize windowSize "Particle Simulation" Nothing Nothing
  GLFW.makeContextCurrent (Just window)
  GLFW.setKeyCallback window (Just keyPressed)
  GLFW.setWindowCloseCallback window (Just shutdown)
  let (r, g, b) = clearColor in glClearColor r g b 1
  glClear GL_COLOR_BUFFER_BIT

  time <- utctDayTime <$> getCurrentTime
  particles <- initialParticles
  loop particles time window

  GLFW.destroyWindow window
  GLFW.terminate

keyPressed :: GLFW.KeyCallback
keyPressed window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown window
keyPressed window GLFW.Key'Space  _ GLFW.KeyState'Pressed _ = shutdown window
keyPressed _      _               _ _                     _ = pure ()

shutdown :: GLFW.WindowCloseCallback
shutdown window = do
  GLFW.destroyWindow window
  GLFW.terminate
  exitWith ExitSuccess

loop :: [Particle] -> DiffTime -> Window -> IO ()
loop oldParticles oldTime window = do
  GLFW.pollEvents
  time <- utctDayTime <$> getCurrentTime
  particles <- draw window oldParticles $ case timeStep of
    Fixed t -> t
    RealTime -> time - oldTime
  loop particles time window

data TimeStep = Fixed DiffTime | RealTime

data Particle = Particle
  { pos :: Vector2
  , vel :: Vector2
  , color :: Color
  }
  deriving (Eq, Show)

drawParticle :: Particle -> IO ()
drawParticle p = do
  let vertices = 12

  glBegin GL_TRIANGLE_FAN
  setGlColor p.color
  glVertex2f p.pos.x p.pos.y

  forM_ [0..vertices] $ \i -> do
    let angle = i * 2 * pi / vertices
    glVertex2f (p.pos.x + particleRadius * cos angle) (p.pos.y + particleRadius * sin angle)

  glEnd

resolveWallCollisions :: Particle -> Particle
resolveWallCollisions p = p
  { pos = p.pos
    { x = clamp p.pos.x
    , y = clamp p.pos.y
    }
  , vel = p.vel
    { x = adjustVelocity p.pos.x p.vel.x
    , y = adjustVelocity p.pos.y p.vel.y
    }
  }
  where
    upperBound = 1
    lowerBound = negate upperBound

    realLowerBound = lowerBound + particleRadius
    realUpperBound = upperBound - particleRadius

    adjustVelocity position velocity =
      if isCollision position
      then -velocity * collisionDamping
      else velocity

    isCollision n = n < realLowerBound || n > realUpperBound

    clamp n =
      if
        | n < realLowerBound -> realLowerBound
        | n > realUpperBound -> realUpperBound
        | otherwise -> n

updateParticle :: [Particle] -> Particle -> DiffTime -> Particle
updateParticle allParticles p deltaTime =
  resolveWallCollisions $ p
    { pos = p.pos `plus` (newVel `scale` realToFrac deltaTime)
    , vel = newVel
    }
  where
    newVel =
      sumVec
        [ p.vel
        , scale downVec $ gravity * realToFrac deltaTime -- gravity
        , attractionAcceleration allParticles p `scale` realToFrac deltaTime -- particle attractions
        ]
        `scale` slowdownConstant

attractionAcceleration :: [Particle] -> Particle -> Vector2
attractionAcceleration particles p =
  sumVec $ (flip map) otherParticles $ \other ->
    let dist = distance p.pos other.pos
        dir = scale (other.pos `minus` p.pos) (1 / dist)
    in
      if
        | dist < repulsionRadius -> scale dir $ dist / repulsionRadius - 1
        | dist < attractionRadius -> scale dir $ attractionFactor p.color other.color * (1 - (abs $ 2 * dist - 1 - repulsionRadius) / (1 - repulsionRadius))
        | otherwise -> zeroVec

  where
    attractionRadius = 1
    otherParticles = filter (/=p) particles

initialParticles :: IO [Particle]
initialParticles = sequence $ replicate numParticles randomParticle
  where
    randomParticle = do
      randomX :: Float <- randomRIO (-1, 1)
      randomY :: Float <- randomRIO (-1, 1)
      randomColor :: Color <- toEnum <$> randomRIO
        ( fromEnum (minBound :: Color)
        , fromEnum (maxBound :: Color)
        )
      pure $ Particle
        { pos = Vector2 randomX randomY
        , vel = zeroVec
        , color = randomColor
        }

draw :: Window -> [Particle] -> DiffTime -> IO [Particle]
draw window oldParticles deltaTime = do
  let particles = map (\particle -> updateParticle oldParticles particle deltaTime) oldParticles

  putStrLn $ "fps = " <> show (truncate $ 1 / deltaTime :: Int)

  glClear GL_COLOR_BUFFER_BIT
  forM_ particles drawParticle
  GLFW.swapBuffers window

  pure particles
