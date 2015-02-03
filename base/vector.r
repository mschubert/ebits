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


#' Like \code{table}, but allows specifying a vector of labels
#'
#' \code{table} does not know what values to expect; as a consequence, it does
#' not tabulate non-existent values (see Examples). \code{full_table} amends
#' this, and yields complete count tables, which are sometimes more useful.
#' @param data vector to tabulate, which is internally passed to \code{table}
#' @param labels complete vector of all possible values being counted
#' @return A complete count table of the \code{data}, where the possible values
#' are in the order specified by \code{labels}. Values which do not occur in
#' \code{data} are set to \code{0}.
#' @examples
#' a = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
#' b = rep(TRUE, 4)
#' table(a)
#' # a
#' # FALSE  TRUE
#' #     4     2
#' table(b)
#' # b
#' # TRUE
#' #    4
#' full_table(a, c(TRUE, FALSE))
#' # TRUE FALSE
#' #    2     4
#' full_table(b, c(TRUE, FALSE))
#' # TRUE FALSE
#' #    4     0
full_table = function (data, labels)
    table(factor(data, labels))

#' Sort by a one or more given keys
#'
#' Apply an \code{order} directly to a target.
#'
#' @param data a vector or \code{data.frame} to sort
#' @param ... one or more keys to sort by; if missing, use the values themselves
#'  (for a vector), or the values of the first column
#' @param decreasing logical value indicating whether to invert the sort order
#'  (default: \code{FALSE})
#' @return The sorted data in the same format (preserving names, etc).
sort_by = function (data, ..., decreasing = FALSE)
    UseMethod('sort_by')

sort_by.data.frame = function (data, ..., decreasing = FALSE)
    let(key = if (length(list(...)) == 0) list(data[, 1]) else list(...),
        data[do.call(order, c(key, decreasing = decreasing)), ])

sort_by.default = function (data, ..., decreasing = FALSE)
    let(key = if (length(list(...)) == 0) list(data) else list(...),
        data[do.call(order, c(key, decreasing = decreasing))])
