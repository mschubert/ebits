.idx = import('./indexing')

#' An lapply() function keeping names
#'
#' @param x    An iterable to apply a function on
#' @param fun  A function to apply
#' @param fail_on_error  Whether to `stop()` or `warning()` on failed calls
#' @param ...  Parameters passed to `fun`
#' @return     Results of the function call
lnapply = function(x, fun, fail_on_error=TRUE, ..., simplify=FALSE, USE.NAMES=TRUE) {
#    fun = function(...) try(fun(...))
    re = base::sapply(x, fun, ..., simplify=simplify, USE.NAMES=USE.NAMES)
#    errors = sapply(re, function(x) clast(x) == "try-error")
#
#    error_msg = .idx$descriptive_index()
#
#    if (any(errors) && fail_on_error) {
#        stop("Error(s): ")
#    } else
#        re
}
