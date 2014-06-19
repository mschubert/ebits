# Functional tools

#' This module re-creates quite a few fundamental tools from `pryr`. Why not
#' `require(pryr)` instead? The only reason is that pryr contains many more
#' utilities which have no place at all in this module. In addition, it does
#' some things subtly different.
#' Once I manage to put proper name isolation into place, this decision should
#' be revisited.

# Basic helpers {{{

id = function (x) x

# This uses R's peculiarities in argument matching explained here:
# <http://stat.ethz.ch/R-manual/R-devel/doc/manual/R-lang.html#Argument-matching>
# `.expr` starts with a dot to allow `expr` being used in the actual
# expression.
let = function (.expr, ...)
    eval(substitute(.expr), list2env(list(...), parent = parent.frame()))

#' Create a closure over a given environment for the specified formals and body.
closure = function (formals, body, env)
    eval(call('function', as.pairlist(formals), body), env)

#' A shortcut to create a function
#'
#' @note Using \code{.(params -> body)} is analogous to using
#' \code{function (params) body}.
. = function (...) {
    params = match.call(expand.dots = FALSE)$...
    len = length(params)

    # Parameters either have default values or not. There are three cases:
    #  1. No default values: `names(params)` is `NULL`, `params` contains
    #   parameter names
    #  2. Some default values: `names(params)` is a vector of appropriate length,
    #   `names(params)[len]` is `''`; `params[i]` contains default value
    #  3. Last parameter has default: `params[[len]]` is `default -> body`.

    if (! inherits(params[[len]], '<-'))
        stop('Must be invoked as `.(params -> expr)`')

    names = if (is.null(names(params))) rep('', len) else names(params)

    # Ensure that last parameter is a name
    if (names[len] == '' && ! inherits(params[[len]][[3]], 'name'))
        stop('Must be invoked as `.(params -> expr)`')

    name_from = function (name, value)
        if (name == '') value else name

    default_from = function (name, value)
        if (name == '') quote(expr = ) else value

    param_names = c(mapply(name_from, names[-len], params[-len]),
                    if (names[len] == '') params[[len]][[3]] else names[len])
    param_defaults = c(mapply(default_from, names[-len], params[-len]),
                       if (names[len] == '') quote(expr = ) else params[[len]][[3]])

    formals  = setNames(param_defaults, param_names)
    body = params[[len]][[2]]
    closure(formals, body, parent.frame())
}

# }}}

# Tools for function composition and chaining {{{

#' Partial function application from right to left.
#'
#' @note This is the opposite from the (wrongly-named) \code{roxygen::Curry}:
#'
#'   \code{minus = function (x, y) x - y
#'   partial(minus, 5)(1) == -4}
#'
#' But:
#'
#'   \code{partial(minus, x = 5)(1) == 4}
#'
partial = function (f, ...)
    let(capture = list(...),
        function (...) do.call(f, c(list(...), capture)))

lpartial = function(f, ...)
    let(capture = list(...),
        function (...) do.call(f, c(capture, list(...))))

ppartial = function (f, arg, ...)
    let(capture = list(...), arg = as.character(substitute(arg)),
        function (x) do.call(f, c(setNames(x, arg), capture)))


# Define shortcuts because these functions are so commonly used and constitute
# syntactic noise.
# Not something I would normally do but there's precedence in R; consider `c`.

p = partial
lp = lpartial
pp = ppartial

#' Compose functions \code{g} and \code{f}.
#'
#' \code{compose(g, f)(...) = g(f(...))}.
#'
#' @note Functions are applied in the inverse order of \code{roxygen::Compose}:
#' \url{http://tolstoy.newcastle.edu.au/R/e9/help/10/02/4529.html}
compose = function (g, f)
    function (...) g(f(...))

#' Dot operator (as in Haskell)
`%.%` = compose

#' Function chaining operator (as in F#)
`%|>%` = function (g, f) compose(f, g)

#' Pipe operator as in Bash
`%|%` = function (x, f) f(x)

# }}}

# Higher-order list functions {{{

#' Applies a list of functions to the same argument.
#' @TODO Extend to more than one argument
fapply = function (x, ...)
    lapply(list(...), function (f) f(x))

# What is up with the naming of these (standard R) functions?

map = base::Map

reduce = base::Reduce

# Hides `stats::filter`.
filter = base::Filter

# }}}

# Helpers for working with ranges {{{

#' @TODO Handle negative indices?
boolmask = function (indices, length)
    is.element(1 : length, indices)

indices = seq_along

#' Conditionally count elements.
count = length %.% which

#' Wrapper around \{order} that returns the ordered data rather than the index
#' permutation.
#'
#' Like \code{sort}, but allows specifying multiple sort keys.
sorted = function (data, ..., decreasing = FALSE)
    let(key = if (length(list(...)) == 0) colnames(data) else list(...),
        data[do.call(order, c(lapply(key, lp(`[[`, data)), decreasing = decreasing)), ])

#' Like \code{c}, for dictionaries (\code{list}s with names).
#'
#' @examples
#' cdict(list(a=1, b=NULL), list(a=NULL, b=2), list(c=3)) # list(a=1, b=2, c=3)
cdict = function (...) {
    lists = list(...)
    names = reduce(union, map(names, lists))

    nonnull = .(n, a, b -> if (is.null(a[[n]])) b[[n]] else a[[n]])
    reduce(.(a, b -> map(.(n -> nonnull(n, a, b)), names)), lists)
}

# }}}

#' Create an item selector function for a given item
item = lp(p, `[[`)

items = lp(p, `[`)

#' Negate a function.
#'
#' Similar to \code{base::Negate}
neg = p(compose, `!`)

#' @TODO Add `and` and `or` analogously

#' Use the first value if present, else the second
#'
#' Corresponds to the null-coalesce operator \code{??} in C#
`%or%` = function (a, b)
    if(is.null(a) || is.na(a) || is.nan(a) || length(a) == 0) b else a
