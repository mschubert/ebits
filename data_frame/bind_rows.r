.omit = import('../base/omit')

#' Adds list element names to each data.frame in the list
#'
#' @param x         A list of data.frames
#' @param name_col  The name of the column to be added
#' @param drop      Drop empty elements (TRUE) or fill with NA (FALSE)
#' @return          A tibble with an added name per list element
bind_rows = function(x, name_col=NULL, drop=TRUE) {
    if (!is.data.frame(x[[1]]))
        x = lapply(x, as.data.frame, stringsAsFactors=FALSE, check.names=FALSE)

    if (is.null(names(x)))
        names(x) = seq_along(x)

    if (drop)
        x = .omit$empty(x)

    x = mapply(function(xi, nxi) {
        xi = as.list(xi)
        if (!is.null(name_col))
            xi[[name_col]] = nxi
        xi$`  keep  ` = TRUE
        args = list(xi, stringsAsFactors=FALSE, check.names=FALSE)
        dplyr::tbl_df(do.call(data.frame, args))
    }, x, names(x), SIMPLIFY=FALSE, USE.NAMES=TRUE)

    # calling directly had a bug in some cases
    re = do.call(dplyr::bind_rows, x)
    re$`  keep  ` = NULL
    re
}

add_name_col = function(x, col="name", bind=TRUE) {
    warning("data_frame/add_name_col is deprecated; use bind_rows instead")
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

if (is.null(module_name())) {
    library(testthat)

    x = list(a=data.frame(), b=c(a=5))

    expect_equal(bind_rows(x, name_col=NULL, drop=TRUE),
                 tibble::tibble(a=5))
    expect_equal(bind_rows(x, name_col=NULL, drop=FALSE),
                 tibble::tibble(a=c(NA, 5)))

    expect_equal(bind_rows(x, name_col="n", drop=TRUE),
                 tibble::tibble(a=5, n="b"))
    expect_equal(bind_rows(x, name_col="n", drop=FALSE),
                 tibble::tibble(n=c("a", "b"), a=c(NA, 5)))
}
