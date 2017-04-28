#' intersect() function that takes an arbitrary number of elements
#'
#' @param ...  Arbitrary elements that `base::intersect` should be called on
intersect = function(...) {
    Reduce(base::intersect, list(...))
}
