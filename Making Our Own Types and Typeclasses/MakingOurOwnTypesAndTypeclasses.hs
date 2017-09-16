module MakingOurOwnTypesAndTypeclasses
  ( Shape (..)
  , Point (..)
  , Person (..)
  , surface
  , nudge
  , baseCircle
  , baseRectangle
  , IntMap
  , AssocList
  , Either'
  , Maybe'
  , NonEq
  , Tree
  , singleton
  , treeInsert
  , Equals (..)
  ) where

import qualified Data.Map as Map

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

-- Type synonym
type String' = [Char]
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

-- Type synonyms can be parameterised
type AssocList k v = [(k,v)]
-- Partially applied type constructor
type IntMap = Map.Map Int -- v = Map Int v

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup num lockers =
  case Map.lookup num lockers of
    Nothing -> Left $ "Locker number " ++ show num ++ " does not exist."
    Just (Taken, code) -> Left $ "Locker number " ++ show num ++ " is already taken."
    Just (Free, code) -> Right code

lockers :: LockerMap
lockers = Map.fromList
  [(100,(Taken,"ZD39I"))
  ,(101,(Free,"JAH3I"))
  ,(103,(Free,"IQSA9"))
  ,(105,(Free,"QOTSA"))
  ,(109,(Taken,"893JJ"))
  ,(110,(Taken,"99292"))
  ]

-- Recursive data structure
-- data List a = Empty | Cons { listHead :: a, listTail :: List a } deriving (Show, Read, Eq, Ord)
-- Infix declaration defines how tightly it binds and whether its left or right
-- associative.
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
-- Pattern matching actually matches against constructors!
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x tree@(Node y left right)
  | x == y  = tree
  | x < y   = Node y (treeInsert x left) right
  | x > y   = Node y left (treeInsert x right)

treeElm :: (Ord a) => a -> Tree a -> Bool
treeElm _ EmptyTree = False
treeElm x (Node y left right)
  | x == y  = True
  | x < y   = treeElm x left
  | x > y   = treeElm x right

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree

class Equals a where
  -- These are the type delcarations of the functions which Eq
  -- instances must conform to
  (.==) :: a -> a -> Bool
  (./=) :: a -> a -> Bool
  -- These are the implementations - not mandatory. They are defined
  -- in terms of mutual recursion, so that instances don't have to
  -- implement all variations themselves
  a .== b = not (a ./= b)
  a ./= b = not (a .== b)

data TrafficLight = Red | Yellow | Green

-- Because we defined the functions of Equals in terms of mutual
-- recursion earlier, we only have to specify the difference
-- instances of == that result in True to satisfy the minimal complete
-- definition (the minimum functions that should be implemented so
-- that the type behaves as advertised).
instance Equals TrafficLight where
  Red .== Red = True
  Green .== Green = True
  Yellow .== Yellow = True
  _ .== _ = False

instance Show TrafficLight where
  -- Pattern matching used to specify different implementations of
  -- Show for TrafficLight
  show Red = "A red traffic light"
  show Green = "A green traffic light"
  show Yellow = "Speed up!!"

-- Subclassing is just placing class constraints on class declarations:
--  class (Eq a) => Num a where ...

data NonEq = A | B;

-- Eq implementation for type constructors - type variable m is
-- used to denote the type, its like pattern matching on type
-- constructors with type variables
-- We also need to add the Eq class constraint to the m variable
-- so that we can compare it!
instance Eq m => Eq (Maybe' m) where
  Just' x == Just' y = x == y
  Nothing' == Nothing' = True
  _ == _ = False

-- This doesn't work - throws interesting type error. What if I
-- want to define different instances of a typeclass based on
-- different concrete evaluations of the same type constructor?
-- Something to do with the newtype keyword?
--
-- instance Eq (Maybe' NonEq) where
--   Just' _ == Just' _ = False
--   Nothing' == Nothing' = True
--   _ == _ = False

-- Class for types that can act as a Bool, e.g. can be truthy
-- or falsy
class Booly a where
  b :: a -> Bool

instance Booly Int where
  b 0 = False
  b _ = True

instance Booly [a] where
  b [] = False
  b _  = True

instance Booly Bool where
  -- `id` is a standard library function which takes a value and
  -- returns it.
  b = id

instance Booly (Maybe a) where
  b (Just _)  = True
  b Nothing   = False

-- Our tree is truthy if and only if there exists some element in
-- the tree which is truthy
instance Booly a => Booly (Tree a) where
  b EmptyTree = False
  b (Node x left right) = b x || b left || b right

instance Booly TrafficLight where
  b Red = False
  b _ = True

ifTruthy :: Booly a => a -> b -> b -> b
ifTruthy truthyVal trueBranch falseBranch = if b truthyVal then trueBranch else falseBranch

-- Redefinition of the Functor typeclass. A functor is applied
-- to a type constructor and maps one object of a concrete type
-- of that constructor to another object of another conrete type
-- of that constructor, e.g.:
-- - Mapping a [Char] to [Int]
-- - Mapping a Maybe Char to Maybe Int
-- - etc
-- It expects a type constructor with one type parameter, so stuff
-- like Either requires a little more thought.
-- Some references:
-- - https://en.wikipedia.org/wiki/Functor
-- - https://en.wikipedia.org/wiki/Category_(mathematics)
-- - https://en.wikipedia.org/wiki/Homomorphism
--
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor Maybe' where
  fmap f (Just' x) = Just' (f x)
  fmap f Nothing' = Nothing'

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- Because Functor takes a type ctor that takes one type param, we
-- partially apply Either with a type variable a.
-- This results in an fmap implementation that takes a function with
-- its first param of type b, meaning that it can only map the right
-- hand value.
-- This fits in with the philosophy of the Left value representing
-- an empty box with a value in it explaining why its empty and the
-- right being the actual value.
instance Functor (Either' a) where
  fmap f (Right' x) = Right' (f x)
  fmap f (Left' x) = Left' x

instance (Ord k1) => Functor' (Map.Map k1) where
  fmap' f = Map.foldrWithKey (\k -> (Map.insert k) . f) Map.empty

-- Probably over complicated..

orderSubtrees :: Ord a => Tree a -> Tree a
orderSubtrees EmptyTree = EmptyTree
orderSubtrees (Node x l r) = (Node x (orderTree l) (orderTree r))

swapLeft :: Ord a => Tree a -> Tree a
swapLeft (Node x (Node y l r) right) = (Node y (orderSubtrees (Node x l r)) (orderSubtrees right))

swapRight :: Ord a => Tree a -> Tree a
swapRight (Node x left (Node y l r)) = (Node y (orderSubtrees left) (orderSubtrees (Node x l r)))

nodeCompare :: Ord a => Tree a -> Tree a -> Ordering
nodeCompare _ EmptyTree = EQ
nodeCompare (Node x _ _) (Node y _ _) = compare x y

orderTree :: Ord a => Tree a -> Tree a
orderTree EmptyTree = EmptyTree
orderTree root@(Node x left right)
  | (nodeCompare root left) == GT   = swapLeft root
  | (nodeCompare root right) == LT  = swapRight root
  | otherwise = (Node x (orderSubtrees left) (orderSubtrees right))

-- If it implemented foldable or something...
--  orderTree2 :: Ord a => Tree a -> Tree a
--  orderTree2 = foldr treeInsert

-- `a` must be of kind * because we pass it to `j` to produce
-- a concrete type.
-- `j` must be of kind * -> * because it takes `a` and produces
-- a concrete type.
-- `t` must be of type * -> (* -> *) -> * because it takes a
-- concrete type `a` and a type constructor which takes a concrete
-- type and produces a concrete type, `j`, and produces a conrete
-- type.
class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a } deriving (Show)
instance Tofu Frank where
  tofu x = Frank x
-- Frank { frankField = Just "Test" } -- is of type Frank String Maybe!


