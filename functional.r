# Functional tools

#' This module re-creates quite a few fundamental tools from `pryr`. Why not
#' `require(pryr)` instead? The only reason is that pryr contains many more
#' utilities which have no place at all in this module. In addition, it does
#' some things subtly different.
#' Once I manage to put proper name isolation into place, this decision should
#' be revisited.

# Basic helpers {{{

id <- function (x) x

# This uses R's peculiarities in argument matching explained here:
# <http://stat.ethz.ch/R-manual/R-devel/doc/manual/R-lang.html#Argument-matching>
# `.expr` starts with a dot to allow `expr` being used in the actual
# expression.
let <- function (.expr, ...)
    eval(substitute(.expr), list2env(list(...), parent = parent.frame()))

#' Create a closure over a given environment for the specified formals and body.
closure <- function (formals, body, env)
    eval(call('function', as.pairlist(formals), body), env)

#' Create a list of empty symbols, with names set
symlist <- function (names)
    setNames(Map(function (p) quote(expr = ), names), names)

#' A shortcut to create a function
#'
#' @note Using \code{.(args = body)} is analogous to using
#' \code{function (args) body} with one exception: \code{.} arguments do not
#' support defaults.
#' Since its purpose is mainly for lambdas in higher-order list functions, this
#' functionality is not needed.
. <- function (...) {
    args <- match.call(expand.dots = FALSE)$...
    last <- length(args)
    params <- symlist(c(args[-last], names(args)[[last]]))
    if (length(args) > 1 && length(params) != length(args))
        stop('Must be of the form `fun(a, b = expr)`')
    for (arg in args[-last])
        if (! is.name(arg))
            stop('Invalid argument specifier: ', arg)

    closure(params, args[[last]], parent.frame())
}

# }}}

# Tools for function composition and chaining {{{

#' Partial function application from right to left.
#'
#' @note This is the opposite from the (wrongly-named) \code{roxygen::Curry}:
#'
#'   \code{minus <- function (x, y) x - y
#'   partial(minus, 5)(1) == -4}
#'
#' But:
#'
#'   \code{partial(minus, x = 5)(1) == 4}
#'
partial <- function (f, ...)
    let(capture = list(...),
        function (...) do.call(f, c(list(...), capture)))

lpartial <- function(f, ...)
    let(capture = list(...),
        function (...) do.call(f, c(capture, list(...))))

ppartial <- function (f, arg, ...)
    let(capture = list(...), arg = as.character(substitute(arg)),
        function (x) do.call(f, c(setNames(x, arg), capture)))


# Define shortcuts because these functions are so commonly used and constitute
# syntactic noise.
# Not something I would normally do but there's precedence in R; consider `c`.

p <- partial
lp <- lpartial
pp <- ppartial

#' Compose functions \code{g} and \code{f}.
#'
#' \code{compose(g, f)(...) = g(f(...))}.
#'
#' @note Functions are applied in the inverse order of \code{roxygen::Compose}:
#' \url{http://tolstoy.newcastle.edu.au/R/e9/help/10/02/4529.html}
compose <- function (g, f)
    function (...) g(f(...))

#' Dot operator (as in Haskell)
`%.%` <- compose

#' Function chaining operator (as in F#)
`%|>%` <- function (g, f) compose(f, g)

#' Pipe operator as in Bash
`%|%` <- function (x, y) y(x)

# }}}

# Higher-order list functions {{{

#' Applies a list of functions to the same argument.
#' @TODO Extend to more than one argument
fapply <- function (x, ...)
    lapply(list(...), function (f) f(x))

# What is up with the naming of these (standard R) functions?

map <- base::Map

reduce <- base::Reduce

# Hides `stats::filter` but I don't care.
filter <- base::Filter

# }}}

# Helpers for working with ranges {{{

#' @TODO Handle negative indices?
boolmask <- function (indices, length)
    is.element(1 : length, indices)

indices <- seq_along

#' Conditionally count elements.
count <- length %.% which

#' Wrapper around \{order} that returns the ordered data rather than the index
#' permutation.
#'
#' Like \code{sort}, but allows specifying multiple sort keys.
sorted <- function (data, ..., decreasing = FALSE)
    let(key = if (length(list(...)) == 0) colnames(data) else list(...),
        data[do.call(order, c(lapply(key, lp(`[[`, data)), decreasing = decreasing)), ])

#' Like \code{c}, for dictionaries (\code{list}s with names).
#'
#' @examples
#' cdict(list(a=1, b=NULL), list(a=NULL, b=2), list(c=3)) # list(a=1, b=2, c=3)
cdict <- function (...) {
    lists <- list(...)
    names <- reduce(union, map(names, lists))

    nonnull <- function (n, a, b) if (is.null(a[[n]])) b[[n]] else a[[n]]
    reduce(function (a, b) map(function (n) nonnull(n, a, b), names), lists)
}

# }}}

#' Create an item selector function for a given item
item <- lp(p, `[[`)

items <- lp(p, `[`)

#' Negate a function.
#'
#' Similar to \code{base::Negate}
neg <- p(compose, `!`)

#' @TODO Add %or% and %and% analogously

#' Use the first value if present, else the second
#'
#' Corresponds to the null-coalesce operator \code{??} in C#
`%else%` <- function (a, b)
    if(is.null(a) || is.na(a) || is.nan(a) || length(a) == 0) b else a
