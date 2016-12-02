import_('./operators')

#' Creates a factor with ordered levels
#' 
#' @param x     A vector to be converted in a factor
#' @param ...   Ordered values or assignments of different levels
#' @param drop  Drop unreferenced levels
#' @return      A factor with levels exchanged and reordered
relevel = function(.data, ..., drop=FALSE) {
    dots = pryr::named_dots(...)
    new_name = names(dots)
    old_name = as.character(dots)
    lookup = setNames(new_name, old_name)
 
    .data = as.character(.data)
    update_idx = .data %in% old_name
    .data[update_idx] = lookup[.data[update_idx]]

    if (drop)
        factor(.data, levels = new_name)
    else
        factor(.data, levels = c(new_name, unique(.data[!update_idx])))
}

if (is.null(module_name())) {
    library(testthat)

    chars = c('a', 'b', 'c', 'c', 'a')
    f1 = factor(chars)

    # rename "a" to "x", keeping same order of labels
    expect_equal(relevel(f1, x=a),
                 relevel(f1, x=a, drop=FALSE),
                 relevel(f1, x="a"),
                 relevel(f1, x="a", drop=FALSE),
                 factor(sub("a", "x", chars), levels=c('x', 'b', 'c')))

    # rename "b" to "x", making it the first level
    expect_equal(relevel(f1, x=b),
                 relevel(f1, x=b, drop=FALSE),
                 relevel(f1, x="b"),
                 relevel(f1, x="b", drop=FALSE),
                 factor(sub("b", "x", chars), levels=c('x', 'a', 'c')))

    # replace all levels
    expect_equal(relevel(f1, x=b, y=c, w=a, drop=FALSE),
                 relevel(f1, x=b, y=c, w=a, drop=TRUE),
                 factor(c("w", "x", "y", "y", "w"), levels=c("x", "y", "w")))

    # replace 2 levels, drop rest
    expect_equal(relevel(f1, w=c, x=b, drop=TRUE),
                 factor(c(NA, "x", "w", "w", NA), levels=c('w', 'x')))
}
