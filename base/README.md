Basic and utility functions
===========================

### `->` lambda syntax

Specify functions using the syntax:

```r
fun = x -> x^2
fun(3)
# 9
```

### `%or%`, `%OR%`

Will return `rhs` if `lhs` is `FALSE`, `NULL`, `NA`, etc.

`%OR%` will also catch exceptions.

### `%catch%`

Will catch exceptions from `lhs`.

### `intersect()`

Intersect all arguments, not just two.

```r
intersect(c(1:5), c(3:10), (3,5,6))
# c(3,5)
```

### `duplicated()`

Allows specifying the parameters `all` (return `TRUE` for all duplicated
indices, not only starting at the second) and `random` (randomly select
which index out of the duplicated entries to keep).
