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
    cdata = as.character(.data)

    unmapped_levels = setdiff(old_name, c(levels(.data), cdata))
    if (length(unmapped_levels) > 0)
        stop("Referenced levels that are not part of the original: ",
             paste(unmapped_levels, sep=", "))
 
    update_idx = cdata %in% old_name
    cdata[update_idx] = lookup[cdata[update_idx]]

    if (drop)
        factor(cdata, levels = new_name)
    else {
        old_levels = setdiff(cdata[!update_idx], new_name)
        factor(cdata, levels = c(new_name, old_levels))
    }
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

    # rename "c" to "x", making it the first level
    expect_equal(relevel(f1, x=c),
                 relevel(f1, x=c, drop=FALSE),
                 relevel(f1, x="c"),
                 relevel(f1, x="c", drop=FALSE),
                 factor(sub("c", "x", chars), levels=c('x', 'a', 'b')))

    # replace all levels
    expect_equal(relevel(f1, x=b, y=c, w=a, drop=FALSE),
                 relevel(f1, x=b, y=c, w=a, drop=TRUE),
                 factor(c("w", "x", "y", "y", "w"), levels=c("x", "y", "w")))

    # replace 2 levels, drop rest
    expect_equal(relevel(f1, w=c, x=b, drop=TRUE),
                 factor(c(NA, "x", "w", "w", NA), levels=c('w', 'x')))

    # raise error when ref'ing non-existant label
    expect_error(relevel(f1, x=d))

    # but allow empty level
    f2 = f1[1:2]
    expect_equal(relevel(f2, x=c)
}
