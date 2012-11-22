module Geometry.Cube
( Volume
, Area
) where

import qualified Geometry.Cuboid as Cuboid

Volume :: Float -> Float
Volume side = Cuboid.volume side side side

Area :: Float -> Float
Area side = Cuboid.area side side side

