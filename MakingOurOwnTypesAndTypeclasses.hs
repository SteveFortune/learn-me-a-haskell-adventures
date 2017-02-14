module MakingOurOwnTypesAndTypeclasses
  ( Shape (..)
  , Point (..)
  , Person (..)
  , surface
  , nudge
  , baseCircle
  , baseRectangle
  ) where

-- Bool is a data type that can have 2 values
-- - `data <data-type>` defines the data type
-- - `= <value> | <value> ...` is the value constructor and defines
--   the values that this type can have
-- When comparing two values of the same type that are `Ord`-able made
-- by different type ctors, the one defined first are considered smallest.
data Bool' = No | Yes deriving (Eq, Ord)

-- Shape is defined as either being a `Circle`, which takes 2 values
-- for its origin and 1 for its radius, or a `Rectangle`, which takes
-- 2 values for its ul corner and 2 for its lr.
-- `Circle` and `Rectangle` value constructors are functions which take
-- float parameters and return `Shape`s
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- We can pattern match on different value contructors! Same as pattern
-- matching against [], False, etc. just those values didn't have
-- fields.
-- Also, note that the fn returns a `Shape` not a `Circle` or `Rectangle`
-- in the same way that you might define a fn which returns `Bool`, not `True`
-- you might return
surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point tlx tly) (Point lrx lry)) = (abs $ tlx - lrx) * (abs $ tly - lry)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle w h = Rectangle (Point 0 0) (Point w h)

-- Record syntax! Equivalent of defining a value constructor and lots
-- of functions that return each attribute through pattern matching.
-- `firstName`, etc are all functions!
-- Haskell can automatically derive the typeclass behaviour based on
-- your values.
--  - It can figure out how to implement `Eq` behaviour by comparing
--    each value contained in the type.
--  - Provided those values also conform to `Eq`.
--  - Works with `Eq, Ord, Enum, etc.`
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Int
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show, Eq, Read)

data Car = Car { company :: String
               , modelNo :: Int
               } deriving (Eq, Read, Show)

makeAFord :: (Int) -> Car
makeAFord modelNo = Car {modelNo=modelNo, company="Ford"}

-- Value constructors take values and produce a new value, type
-- constructors take type parameters and produce new types.
data Maybe' a = Nothing' | Just' a
data Pair' a b = Pair' { first :: a, second :: b } deriving (Show)

-- Good example of how to avoid putting type classes in data delcaration
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (m + j) (k + n)

smult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `smult` (Vector l m n) = i*l + j*m + k*n

-- Enum lets you do stuff like `[Mon..Sat]`
-- Bounded lets you do stuff like `minBound :: Day`
data Day = Mon | Tues | Weds | Thurs | Fri | Sat | Sun deriving (Show, Read, Eq, Ord, Bounded, Enum)
