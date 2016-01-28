import_('../../base/operators')

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
        # could potentially convert to factor first as well
        if (is.logical(x) || is.factor(x) || is.character(x))
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
    library(testthat)

    re1 = lm(Sepal.Width ~ Sepal.Length, data=iris)
    expect_equal(unname(unlist(re1['term'])), 'Sepal.Length')
    expect_equal(unname(unlist(re1['size'])), 150)

    re2 = lm(Sepal.Width ~ Sepal.Length, data=iris, return_intercept=TRUE)
    ref2 = broom::tidy(stats::lm(Sepal.Width ~ Sepal.Length, data=iris))
    expect_equal(dplyr::select(re2, -size), ref2)
    
    re3 = lm(Sepal.Width ~ Sepal.Length, data=iris, min_pts=200)
    expect_null(re3)

    # r uses first factor level as reference, others to compare -> 2 rows
    re4 = lm(Petal.Length ~ Species, data=iris)
    expect_equal(nrow(re4), 2)
    expect_equal(sum(is.na(re4$size)), 0)

    # make sure we catch character factors as well
    iris2 = iris
    iris2$Species = as.character(iris2$Species)
    re5 = lm(Petal.Length ~ Species, data=iris2)
    expect_equal(nrow(re5), 2)
    expect_equal(sum(is.na(re5$size)), 0)
}
