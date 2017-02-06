if (is.null(module_name())) {
    library(testthat)

    a = matrix(c(2,0,1,0,1,0), ncol=2, dimnames=list(c('x','y','z'), c('a','b')))
    b = setNames(c(1,2,0), c('x','y','z'))
    fit1 = lm(b ~ a)
    expect_equal(nrow(fit1), 2)
    expect_equal(fit1$a, c('a','b')) # column indices
    expect_equal(fit1$term, c('a','a')) # rhs of equation

    # fit a as one coefficients matrix
    fit2 = lm(b ~ a, atomic="a")
    expect_equal(nrow(fit2), 2)
    expect_equal(fit2$term, c('aa','ab'))
}
