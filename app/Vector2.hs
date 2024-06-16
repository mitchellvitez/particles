{-# LANGUAGE OverloadedRecordDot #-}

module Vector2 where

data Vector2 = Vector2 { x :: Float, y :: Float }
  deriving (Eq, Show)

scale :: Vector2 -> Float -> Vector2
scale (Vector2 x y) f = Vector2 (f * x) (f * y)

plus :: Vector2 -> Vector2 -> Vector2
plus a b = Vector2 (a.x + b.x) (a.y + b.y)

minus :: Vector2 -> Vector2 -> Vector2
minus a b = Vector2 (a.x - b.x) (a.y - b.y)

magnitude :: Vector2 -> Float
magnitude (Vector2 x y) = sqrt (x*x + y*y)

distance :: Vector2 -> Vector2 -> Float
distance a b = sqrt $ (b.x - a.x)^2 + (b.y - a.y)^2

sumVec :: [Vector2] -> Vector2
sumVec = foldl plus zeroVec

zeroVec :: Vector2
zeroVec = Vector2 0 0

downVec :: Vector2
downVec = Vector2 0 (-1)
