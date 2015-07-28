import('../base/operators')

lm = function(formula, data=environment(formula), min_pts=3, return_intercept=FALSE) {
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

coxph = function(formula, data=environment(formula), min_pts=3) {
    pts = nrow(na.omit(do.call(cbind, data)))
    if (pts < min_pts)
        NULL
    else {
        fstr = strsplit(sub("\\+", ",", deparse(formula)), "~")[[1]]
        formula = formula(paste("survival::Surv(", fstr[1], ") ~", fstr[-1]))
        survival::coxph(formula, data) %>%
            broom::tidy() %>%
            cbind(size = pts)
    }
}

if (is.null(module_name())) {
    # lm models
    re1 = lm(Sepal.Width ~ Sepal.Length, data=iris)
    testthat::expect_equal(re1['1','term'], 'Sepal.Length')
    testthat::expect_equal(re1['1','size'], 150)

    re2 = lm(Sepal.Width ~ Sepal.Length, data=iris, return_intercept=TRUE)
    ref2 = broom::tidy(stats::lm(Sepal.Width ~ Sepal.Length, data=iris))
    testthat::expect_equal(dplyr::select(re2, -size), ref2)
    
    re3 = lm(Sepal.Width ~ Sepal.Length, data=iris, min_pts=200)
    testthat::expect_null(re3)

    # coxph models
    # ...
}
