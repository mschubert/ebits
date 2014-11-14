import('./functional', attach = TRUE)

#' Create a logical vector from a numeric index vector
#'
#' Compute the inverse of \code{\link{base::which}}: given an index vector and
#' the output length, create a logical vector of the output length, with the
#' specified elements set to \code{TRUE}.
#'
#' @param indices numeric vector of selected indices
#' @param length of the output vecotr
#' @return A logical vector of length \code{length} with the elements specified
#'  by \code{indices} set to \code{TRUE}.
#' @seealso \code{\link{base::which}}
# TODO: Handle negative indices?
# TODO: handle factors, equivalent to unname(apply(model.matrix(~0 + factor), 2, as.logical))
index_mask = function (indices, length)
    is.element(1 : length, indices)

#' Conditionally count elements.
count = length %.% which

#' Sort by a one or more given keys
#'
#' Apply an \code{order} directly to a target.
#'
#' @param data a vector, matrix or \code{data.frame} to sort
#' @param ... one or more keys to sort by; if missing, use the \code{names} or
#'  \code{colnames}
#' @param decreasing logical value indicating whether to invert the sort order
#'  (default: \code{FALSE})
#' @return The sorted data in the same format (preserving names, etc).
sort_by = function (data, ..., decreasing = FALSE)
    UseMethod('sort_by')

sort_by.data.frame = function (data, ..., decreasing = FALSE)
    let(key = if (length(list(...)) == 0) colnames(data) else list(...),
        data[do.call(order, c(lapply(key, lp(`[[`, data)),
                              decreasing = decreasing)), ])

sort_by.default = function (data, ..., decreasing = FALSE)
    let(key = if (length(list(...)) == 0) names(data) else list(...),
        data[do.call(order, c(lapply(key, lp(`[[`, data)),
                              decreasing = decreasing))])
