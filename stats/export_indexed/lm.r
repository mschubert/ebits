import('../../base/operators')

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
    pts = nrow(na.omit(do.call(cbind, data)))
    if (pts < min_pts)
        NULL
    else {
        re = stats::lm(formula, data) %>%
            broom::tidy() %>%
            cbind(size = pts)
        if (return_intercept)
            re
        else
            dplyr::filter(re, term != "(Intercept)")
    }
}

if (is.null(module_name())) {
    re1 = lm(Sepal.Width ~ Sepal.Length, data=iris)
    testthat::expect_equal(re1['1','term'], 'Sepal.Length')
    testthat::expect_equal(re1['1','size'], 150)

    re2 = lm(Sepal.Width ~ Sepal.Length, data=iris, return_intercept=TRUE)
    ref2 = broom::tidy(stats::lm(Sepal.Width ~ Sepal.Length, data=iris))
    testthat::expect_equal(dplyr::select(re2, -size), ref2)
    
    re3 = lm(Sepal.Width ~ Sepal.Length, data=iris, min_pts=200)
    testthat::expect_null(re3)
}
