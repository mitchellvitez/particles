{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Graphics.GL.Compatibility30
import Graphics.UI.GLFW as GLFW
import qualified Data.Vector.Storable as VS
import Control.Monad (forever, forM_)
import Foreign.Ptr (castPtr)
import System.Exit (exitWith, ExitCode(..))
import Data.Word (Word8)
import Foreign.Marshal.Alloc (malloc)
import Foreign.Storable (peek)
import GHC.Float (int2Float)
import Data.Time.Clock
import Data.IORef
import Control.Concurrent (threadDelay)
import Debug.Trace
import System.Random

keyPressed :: GLFW.KeyCallback
keyPressed window GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown window
keyPressed window GLFW.Key'Space  _ GLFW.KeyState'Pressed _ = shutdown window
keyPressed _      _               _ _                     _ = pure ()

shutdown :: GLFW.WindowCloseCallback
shutdown window = do
  GLFW.destroyWindow window
  GLFW.terminate
  exitWith ExitSuccess

width :: Int
width = 1920 `div` 2

height :: Int
height = 1920 `div` 2

-- TODO: mouse interaction

-- we don't support window resizing
main :: IO ()
main = do
  GLFW.init
  GLFW.defaultWindowHints
  Just window <- GLFW.createWindow width height "Haskell Fluid Simulation" Nothing Nothing
  GLFW.makeContextCurrent (Just window)
  GLFW.setKeyCallback window (Just keyPressed)
  GLFW.setWindowCloseCallback window (Just shutdown)
  glClearColor 0 0 0.1 1 -- dark blue

  time <- utctDayTime <$> getCurrentTime
  -- time <- newIORef =<< utctDayTime <$> getCurrentTime
  -- particles <- newIORef initialParticles
  ps <- initialParticles
  loop ps time window

  -- main loop
  -- TODO: convert this to a loop function without the IORefs
  -- forever $ do
  --   GLFW.pollEvents
  --   time <- utctDayTime <$> getCurrentTime
  --   oldTimeValue <- readIORef oldTime
  --   particlesValue <- readIORef particles
  --   newParticles <- draw window particlesValue (time - oldTimeValue)
  --   atomicWriteIORef oldTime time
  --   atomicWriteIORef particles newParticles

  GLFW.destroyWindow window
  GLFW.terminate

loop :: [Particle] -> DiffTime -> Window -> IO ()
loop particles oldTime window = do
  GLFW.pollEvents
  time <- utctDayTime <$> getCurrentTime
  newParticles <- draw window particles (time - oldTime)
  loop newParticles time window

data Particle = Particle { pos :: Vector2, vel :: Vector2 }
  deriving (Eq, Show)

data Vector2 = Vector2 { x :: Float, y :: Float }
  deriving (Eq, Show)

zeroes :: Vector2
zeroes = Vector2 0 0

radius = 0.01

drawParticle :: Particle -> IO ()
drawParticle p = do
  let
    x = p.pos.x
    y = p.pos.y
    triangleAmount = 36
    pi = 3.14159365358979

  -- pure $ traceId "drawParticle"
  glBegin GL_TRIANGLE_FAN
  glColor3f 0.25 0.5 1
  glVertex2f x y
  forM_ [0..triangleAmount] $ \i -> do
    let z = i * 2 * pi / triangleAmount
    glVertex2f (x + radius * cos z) (y + radius * sin z)
  glEnd

times :: Vector2 -> Float -> Vector2
times (Vector2 x y) f = Vector2 (f * x) (f * y)

plus :: Vector2 -> Vector2 -> Vector2
plus a b = Vector2 (a.x + b.x) (a.y + b.y)

minus :: Vector2 -> Vector2 -> Vector2
minus a b = Vector2 (a.x - b.x) (a.y - b.y)

negateY :: Vector2 -> Vector2
negateY (Vector2 x y) = Vector2 x (-y)

down :: Vector2
down = Vector2 0 (-0.1)

-- returns a point clamped within collision bounds, and a bool for if a hit was detected
resolveWallCollisions :: Particle -> Particle
resolveWallCollisions p = Particle
  { pos = p.pos
    { x = clamp p.pos.x
    , y = clamp p.pos.y
    }
  , vel = p.vel
    { x = if xCollision then -p.vel.x * damping else p.vel.x
    , y = if yCollision then -p.vel.y * damping else p.vel.y
    }
  }
  where
    low = -1 :: Float
    high = 1 :: Float
    damping = 0.7
    clamp a = if a < low + radius then low + radius else if a > high - radius then high - radius else a
    xCollision = p.pos.x < low + radius || p.pos.x > high - radius
    yCollision = p.pos.y < low + radius || p.pos.y > high - radius

updateParticle :: [Particle] -> Particle -> DiffTime -> Particle
updateParticle allParticles p deltaTime =
  resolveWallCollisions $ Particle
    { pos = p.pos `plus` (newVel `times` realToFrac deltaTime)
    , vel = (newVel `plus` (attractionAcceleration allParticles p `times` realToFrac deltaTime)) `times` drag
    }
  where
    newVel = p.vel `plus` (down `times` (gravity * realToFrac deltaTime))
    drag = 0.9

distance :: Vector2 -> Vector2 -> Float
distance a b = sqrt $ (b.x - a.x)^2 + (b.y - a.y)^2

attractionAcceleration :: [Particle] -> Particle -> Vector2
attractionAcceleration particles p =
  sumVec $ (flip map) otherParticles $ \other ->
    let dist = distance p.pos other.pos
        dir = (other.pos `minus` p.pos) `times` (1 / dist)
    in
      if
        | dist < repulsionRadius -> dir `times` (-1 * scale * 0.5 / max 0.01 (dist * dist))
        -- | dist < nothingRadius -> zeroes
        | dist < attractionRadius -> dir `times` (scale / (dist * dist))
        | otherwise -> zeroes

  where
    scale = 0.05
    repulsionRadius = 0.24
    -- nothingRadius = 0.14
    attractionRadius = 0.25
    otherParticles = filter (/=p) particles

gravity = 10

initialParticles :: IO [Particle]
initialParticles = sequence $ replicate 500 randomParticle

randomParticle :: IO Particle
randomParticle = do
  r1 :: Float <- randomRIO (-1, 1)
  r2 :: Float <- randomRIO (-1, 1)
  pure $ Particle { pos = Vector2 r1 r2, vel = zeroes}

magnitude :: Vector2 -> Float
magnitude (Vector2 x y) = sqrt (x*x + y*y)

sumVec :: [Vector2] -> Vector2
sumVec = foldl plus zeroes

right :: Vector2
right = Vector2 1 0

up :: Vector2
up = Vector2 0 1

draw :: Window -> [Particle] -> DiffTime -> IO [Particle]
draw window oldParticles deltaTime = do
  let particles = map (\particle -> updateParticle oldParticles particle deltaTime) oldParticles

  putStrLn $ "deltaTime = " <> show deltaTime

  glClear GL_COLOR_BUFFER_BIT

  forM_ particles $ \particle -> do
    drawParticle particle

  GLFW.swapBuffers window

  pure particles
