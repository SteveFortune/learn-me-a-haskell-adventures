# Good Practice 

- Never declare ranges with floating point numbers - they aren't precise.
- Declare functions with types, unless they are small.
- Check the length of lists to avoid 'element index out of bounds' errors.
- Using `map` is more readable for cases where you only apply some function to the elements of a list (e.g. `map (+3) [1,2,3]` vs `[x + 3 | x <- [1,2,3]]` 
