module Shapes
( Point (..)
, Shape (..)
, area
, nudge
, baseCircle
, baseRect
) where

data Point = Point Float Float
    deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point px py) r) x y                = Circle (Point (x + px) (y + py)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x y = Rectangle (Point (x1 + x) (y1 + y)) (Point (x2 + x) (y2 + y))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 h) (Point w 0)
