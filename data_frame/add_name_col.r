#' Adds list element names to each data.frame in the list
#'
#' @param x    A list of data.frames
#' @param col  The name of the column to be added
#' @return     The data frame with an added name per list element
add_name_col = function(x, col="name") {
    stopifnot(is.list(x))
    stopifnot(is.data.frame(x[[1]]))

    mapply(function(xi, nxi) {
        xi[[col]] = nxi
        xi
    }, x, names(x), SIMPLIFY=FALSE, USE.NAMES=TRUE)
}
