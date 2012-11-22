module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume x y z = x * y * z

area :: Float -> Float -> Float -> Float
area x y z = (2 * rectArea x y) + (2 * rectArea y z) + (2 * rectArea x z)

rectArea :: Float -> Float -> Float
rectArea x y = x * y
