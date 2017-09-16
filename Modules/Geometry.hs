-- Module names should match the file name
module Geometry 
( sphereVolume
, sphereArea
-- *
) where

sphereVolume :: Float -> Float 
sphereVolume r = (4.0 / 3.0) * pi * (r^3)

sphereArea :: Float -> Float 
sphereArea r = 4 * pi * (r^2)

-- Can't get to this
something :: Float
something = 123.0

-- Can put modules in subdirectories and import than as <directory-name>.<file-name>
-- E.g.:
--  - Geometry/
--  -- Sphere.hs
--  -- Cuboid.hs
--  ...
--  import Geometry.Sphere
