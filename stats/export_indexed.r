import(".", attach=TRUE)

if (is.null(module_name())) {
    library(testthat)
    library(dplyr)

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

    c = matrix(c(1,1,2,2,1,1), ncol=2, dimnames=dimnames(a))
    fit3 = lm(a ~ b + c)
    idx = tidyr::crossing(c=colnames(c), a=colnames(a), term=c('b','c'))[c('a','c','term')]
    expect_equal(tbl_df(fit3[c('a','c','term')]), idx)

    fit4 = lm(a ~ c + b, group=c('a', 'c'))
    expect_equal(fit4 %>% arrange(a,c,term),
                 dplyr::filter(fit3, a==c) %>% arrange(a,c,term))
}
