#' An lapply() function keeping names
#'
#' @param x    An iterable to apply a function on
#' @param fun  A function to apply
#' @param ...  Parameters passed to sapply()
#' @return     Results of the function call
lnapply = function(x, fun, ..., simplify=FALSE, USE.NAMES=TRUE) {
    base::sapply(x, fun, simplify=simplify, USE.NAMES=USE.NAMES)
}
