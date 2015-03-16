`%<>%` = magrittr::`%<>%`

#' `rbind` with option to add list names as column
#'
#' x_list    A list to bind row-wise
#' name_col  Name of the column to add list names to
#' ...       Arguments to be passed to `rbind`
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
