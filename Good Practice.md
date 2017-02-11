# Good Practice 

- Never declare ranges with floating point numbers - they aren't precise.
- Declare functions with types, unless they are small.
- Check the length of lists to avoid 'element index out of bounds' errors.
- Using `map` is more readable for cases where you only apply some function to the elements of a list (e.g. `map (+3) [1,2,3]` vs `[x + 3 | x <- [1,2,3]]`
- Best to use folds when you want to traverse a list to return something. They are basically wrappers around recursively operating on the head and tail
- Use `$` to avoid writing so many parentheses, e.g.:
  - `sum (map sqrt [1..130]) vs `sum $ map sqrt [1..130]``
  - `sqrt (1 + 5 + 6)` vs `sqrt $ 1 + 5 + 6`
  - Its sort of equivalent of to wrapping the proceeding expression in parentheses
  - `sum (filter (>10) (map (*2) [2..10]))` vs `sum $ filter (>10) $ map (*2) [2..10]`
- Function composition can be used for making functions on the fly to pass around to other functions - sometimes in a more clean manner than lambdas.
- Writing a function in point free style can be less readable if a function is too complex. Don't make long function composition chains. Better style is to use let bindings to give labels to intermediary results. E.g. `sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]` Would be more readable as...
```
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares 
  in sum belowLimit
```
- Use `foldl'` and `foldl1'` on big lists to avoid stack overflows - they are non-lazy, which means that they actually compute the accumulator value as they churn through the list, whereas their lazy counterparts use 'thunks' to defer computing the value until its required, which will fill up the stack.
- When dealing with *By* functions which test for equality, you'll usually do `(==) \`on\` ...`; when dealing with *By* functions which test for ordering, you'll usually do `compare \`on\` something`
- Better to use folds than list recursion because its easier to read.
- Better to work with `Data.Map` than list association.
- Sets are ordered because they are implemented as trees - they are faster than maps and lists.
- Sets are often used to get a list of duplicates from a list by making a list into a set and then converting back again - its faster, but requires elements to be of type `Ord`, whereas `nub` only requires `Eq`.
