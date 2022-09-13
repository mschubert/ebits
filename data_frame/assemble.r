import('../base/operators')

#' Creates a data.frame from named vectors
#'
#' @param ...  Multiple named vectors. If arguments are named, those
#'             will be the names used in the result columns. If not,
#'             the variable names will be used for columns.
#' @return     A data_frame with stacked rows
assemble = function(...) {
    l. = list(...)
    names(l.) = names(l.) %or% unlist(match.call(expand.dots=FALSE)$...)

    myclasses = sapply(l., class)

    re = as.data.frame(narray::stack(l., along=2), stringsAsFactors=FALSE)
    for (i in seq_along(re)) {
        cc = myclasses[names(re)[i]]
        if (cc == "factor")
            re[[i]] = factor(re[[i]], levels=levels(l.[[names(re)[i]]]))
        else
            class(re[[i]]) = cc
    }
    re
}

if (is.null(module_name())) {
    library(testthat)

    a = setNames(c(1L,3L,5L), c("A","C","E"))
    b = setNames(c(1.1,2.2,3.3,4.4), LETTERS[1:4])
    x = setNames("2", "B")
    y = setNames(factor(2), "A")

#    df = assemble(a=a, b, x, y)
#    classes = unname(sapply(df, class))
#
#    expect_equal(sum(is.na(df)), 11)
#    expect_equal(rownames(df), c("A","C","E","B","D"))
#    expect_equal(classes, sapply(list(a,b,x,y), class))
#    expect_equal(unname(y), df$y[1])
}
