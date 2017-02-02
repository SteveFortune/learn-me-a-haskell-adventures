# Notes

- Functions in Haskell only take one parameter
- The space is the function application.
  - E.g. defining `max :: Ord a => a -> a -> a` is equivalent to defining it is `max :: Ord a => a -> (a -> a)`
  - Curried functions
- Running `divTen = (/10)` in GCHI throws an error as it returns a function of type `Factional a => a -> a` and functions aren't part of the `Show` typeclass, so haskell doesn't know how to print it.
- Due to Haskell's lazy evaluation, when mapping and filtering over a list several times, it will only ever be enumerated once
- Function application with a space is left-associative (`f a b c` = `(((f a) b) c)`) whereas with $ its right-associative (`f $ a $ b $ c` = `(f $ (a $ (b $ c)))`)
- Function application operator allows us to treat function application like any other function, e.g. mapping application over a list of functions: `map ($ 3) [(4+), (5*), (/6), (^3)]`
- Because function composition is right-associative, you can chain functions that you want to compose! E.g.
  - `map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]]`
  - Vs. `map (negate . sum . tail) [[1..5], [3..6], [1..7]]` etc
- Can use compose functions which take several parameters by evaluating them with parameters in the correct order. E.g.
  - `sum (replicate 5 (max 6.7. 8.9))` vs `sum . replicate 5 . max 6.7 $ 8.9`
  - `replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,1,7,8])))` vs `replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,1,7,8]` 
- Omitting a parameter because of currying in a function definition is called 'point free style'. E.g `sum xs = foldl (+) 0 xs` vs `sum = foldl (+) 0`
- Searching for functions, etc:
  - [Hoogle](https://www.haskell.org/hoogle/)
  - [HHL](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/)
