`%<>%` = magrittr::`%<>%`

#' `rbind` with option to add list names as column
#'
#' @param x_list    A list to bind row-wise
#' @param name_col  Name of the column to add list names to
#' @param ...       Arguments to be passed to `rbind`
#' @return          A data.frame
rbind = function(x_list, add_col=NULL, ...) {
    if (!is.null(add_col))
        x_list %<>%
            mapply(function(x,s) { x[[add_col]] = s; x },
                   x = .,
                   s = names(x_list),
                   SIMPLIFY = FALSE) %>%
            unname()

    do.call(base::rbind, c(x_list, list(...)))
}
