#' Function call helpers
#' @name call
NULL

unpack = structure(numeric(), class = 'ebits$base$call$unpack')

`[<-.ebits$base$call$unpack` = function (x, ..., value) {
    names = vapply(match.call(expand.dots = FALSE)$..., deparse, character(1))
    stopifnot(length(names) == length(value))
    parent = parent.frame()
    Map(function (n, x) assign(n, x, envir = parent), names, value)
    x
}

register_S3_method('[<-', 'ebits$base$call$unpack', `[<-.ebits$base$call$unpack`)

