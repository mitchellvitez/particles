{-# LANGUAGE OverloadedRecordDot #-}

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

  oldTime <- newIORef 0

  -- main loop
  forever $ do
    GLFW.pollEvents
    time <- utctDayTime <$> getCurrentTime
    oldTimeValue <- readIORef oldTime
    draw window initialParticles (time - oldTimeValue)
    atomicWriteIORef oldTime time

  GLFW.destroyWindow window
  GLFW.terminate

data Particle = Particle { pos :: Vector2, vel :: Vector2 }
  deriving (Eq, Show)

data Vector2 = Vector2 { x :: Float, y :: Float }
  deriving (Eq, Show)

zeroes :: Vector2
zeroes = Vector2 0 0

drawParticle :: Particle -> IO ()
drawParticle p = do
  let
    x = p.pos.x
    y = p.pos.y
    triangleAmount = 36
    radius = 0.01
    pi = 3.14159365358979

  glBegin GL_TRIANGLE_FAN
  glColor3f 0.25 0.5 1
  glVertex2f x y
  forM_ [0..triangleAmount] $ \i -> do
    let z = i * 2 * pi / triangleAmount
    glVertex2f (x + radius * cos z) (y + radius * sin z)
  glEnd

times :: Vector2 -> Float -> Vector2
times (Vector2 x y) f = Vector2 (f * x) (f * y)

down :: Vector2
down = Vector2 0 (-1)

updateParticle :: Particle -> DiffTime -> Particle
updateParticle p deltaTime =
  Particle
    { pos = p.vel `times` realToFrac deltaTime
    , vel = down `times` gravity `times` realToFrac deltaTime
    }

gravity = -9.81

initialParticles = [Particle zeroes zeroes]

draw :: Window -> [Particle] -> DiffTime -> IO ()
draw window oldParticles deltaTime = do
  let particles = map (\particle -> updateParticle particle deltaTime) oldParticles

  putStrLn $ "deltaTime = " <> show deltaTime

  glClear GL_COLOR_BUFFER_BIT

  forM_ particles $ \particle -> do
    drawParticle particle

  GLFW.swapBuffers window
