# Questions

- Error handling patterns and techinques in Haskell (and in functional programming languages in general). IN PROGRESS - Either <result> <error type>
- Why not use unsigned types for values such as `length` of a list? See [this SO question](https://stackoverflow.com/questions/12432154/int-vs-word-in-common-use).
- Not clear on the associativity of function composition operator, e.g. why is

```
sum . takeWhile (< 10000) . filter odd . map (^2) $ [1..]
```

equivalent to...

```
sum . (takeWhile (< 10000)) . (filter odd) . (map (^2)) $ [1..]
```
DONE:
- Space has the highest precedence and $ has the lowest.
- Precedence of . < precence of space.
- `sum . takeWhile (< 10000) . filter odd` 
  <=> `((.) sum ((.) (filter odd) (takeWhile (<1000)))`

- Need to understand how these concepts are all used in practice. Find some open source examples, web frameworks, ORMs (?) etc.

### Project Ideas

- Write an ORM in Haskell?
- ElasticSearch / Haskell project?
