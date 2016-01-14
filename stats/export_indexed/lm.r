import_('../../base/operators')
.ar = import_('../../array/map')

#' Function to calculate and return a cleaned linear model
#'
#' @param formula   A formula specifying the relationship between variables
#' @param data      A data.frame, list, or by default the formula env containing vars      
#' @param min_pts   Minimum number of points to create a model for
#' @param return_intercept  Whether the result data.frame should contain the intercept
#' @param atomic_class  Variable classes that should not be split by default
#' @return          A data.frame containing the model result
lm = function(formula, data=environment(formula), min_pts=3, return_intercept=FALSE,
              atomic_class='vector') {
    pts = model.frame(formula, data)
    size = lapply(pts, function(x) {
        if (is.logical(x) || is.factor(x))
            as.list(table(x))
        else
            sum(!(is.na(x) | x==0))
    })
    names(size) = names(pts)
    size$'(Intercept)' = nrow(pts)

    # that's a bit clumsy, but it works -
    # better would be to use delimiter "" in unlist
    orig_names = names(size)
    size = unlist(size)
    for (n in orig_names)
        names(size) = sub(paste0(n,"\\."), n, names(size))

    if (nrow(pts) < min_pts)
        return(NULL)

    re = stats::lm(formula, data) %>%
        broom::tidy() %>%
        cbind(size = sapply(.$term, function(x) size[x])) %>%
        dplyr::as_data_frame()
    if (return_intercept)
        re
    else
        dplyr::filter(re, term != "(Intercept)")
}

if (is.null(module_name())) {
    re1 = lm(Sepal.Width ~ Sepal.Length, data=iris)
    testthat::expect_equal(unname(unlist(re1['term'])), 'Sepal.Length')
    testthat::expect_equal(unname(unlist(re1['size'])), 150)

    re2 = lm(Sepal.Width ~ Sepal.Length, data=iris, return_intercept=TRUE)
    ref2 = broom::tidy(stats::lm(Sepal.Width ~ Sepal.Length, data=iris))
    testthat::expect_equal(dplyr::select(re2, -size), ref2)
    
    re3 = lm(Sepal.Width ~ Sepal.Length, data=iris, min_pts=200)
    testthat::expect_null(re3)

    # r uses first factor level as reference, others to compare -> 2 rows
    re4 = lm(Petal.Length ~ Species, data=iris)
    testthat::expect_equal(nrow(re4), 2)
}
