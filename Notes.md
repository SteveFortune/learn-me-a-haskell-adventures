# Notes

- Functions in Haskell only take one parameter
- The space is the function application.
  - E.g. defining `max :: Ord a => a -> a -> a` is equivalent to defining it is `max :: Ord a => a -> (a -> a)`
  - Curried functions
- Running `divTen = (/10)` in GCHI throws an error as it returns a function of type `Factional a => a -> a` and functions aren't part of the `Show` typeclass, so haskell doesn't know how to print it.
- Due to Haskell's lazy evaluation, when mapping and filtering over a list several times, it will only ever be enumerated once
