#' Adds list element names to each data.frame in the list
#'
#' @param x     A list of data.frames
#' @param col   The name of the column to be added
#' @param bind  Whether or not to bind the list together
#' @return      The data frame with an added name per list element
add_name_col = function(x, col="name", bind=TRUE) {
    stopifnot(is.list(x))
    stopifnot(is.data.frame(x[[1]]))

    re = mapply(function(xi, nxi) {
        xi[[col]] = nxi
        xi
    }, x, names(x), SIMPLIFY=FALSE, USE.NAMES=TRUE)

    if (bind)
        dplyr::bind_rows(re)
    else
        re
}
