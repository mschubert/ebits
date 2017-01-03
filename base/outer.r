import('./lnapply', attach=TRUE)

#' Function to get elements from the parent of another function
#'
#' This is a shortcut to get(x, envir=parent.env(environment))
#'
#' @param ...    The objects to get
#' @param envir  The environment to look for those
#' @param drop   Whether to discard list if only one object queried
#' @return       An object or list of objects
outer = function(..., envir=parent.env(environment()), drop=TRUE) {
    dots = names(pryr::named_dots(...))
    get_fun = function(x) get(x, envir=envir, inherits=TRUE)

    re = lnapply(dots, get_fun)

    if (drop && length(re) == 1)
        re[[1]]
    else
        re
}

if (is.null(module_name())) {
    library(testthat)

    test = 5
    fx = function(y, x = outer(test)) x + y
    testthat::expect_equal(test + 3, fx(3))
}
