#' Returns indices along an object, either numeric or names if available
#'
#' @param x      An object to be indexed
#' @param along  The axis along which to index for array-like objects; default: last dimension
descriptive_index = function(x, along=NULL) {
    if (!is.null(names(x)))
        names(x)
    else if ((is.character(x) || is.numeric(x) || is.logical(x)) &&
             (is.vector(x) || length(dim(x))==1))
        x
    else if (is.factor(x))
        as.character(x)
    else if (!is.null(along) && (is.matrix(x) || is.data.frame(x))) {
        dn = dimnames(x)[[along]]
        if (is.null(dn))
            1:dim(x)[along]
        else
            dn
    } else if (is.list(x)) { # list and data.frame
        if (is.null(names(x)))
            seq_along(x)
        else
            names(x)
    } else
        stop("Not sure how to get indices on that object")
}

#' Subsets an object, either with numeric indices or names if available
#'
#' @param x       An object to get a subset from
#' @param index   The index to subset with
#' @param atomic  Character vector of classes that should not be split ('vector','matrix','list')
#' @param along   The axis along which to index for array-like objects; default: last dimension
subset = function(x, index, along=NULL, atomic=NULL, drop=FALSE) {
    if (is.null(dim(x)) && !is.list(x) && !'vector' %in% atomic)
        x[index, drop=drop]
    else if (is.list(x) && !is.data.frame(x) && !'list' %in% atomic) {
        if (drop && length(index) == 1)
            x[[index]]
        else if (drop && is.logical(index) && sum(index == 1))
            x[index][[1]]
        else
            x[index]
    } else if (is.array(x) || is.data.frame(x) && !'matrix' %in% atomic)
        narray::subset(x, index, along, drop)
    else
        stop("Not sure how to subset that object")
}

if (is.null(module_name())) {
    library(testthat)

    A = matrix(rnorm(20), dimnames=list(letters[1:5],LETTERS[1:4]), nrow=5, ncol=4)

    # names vectors, matrices
    expect_equal(colnames(A),
                 descriptive_index(A[1,]),
                 descriptive_index(A, along=2))

    # subsetting vectors
    expect_equal(A[1,c(1:2,4)],
                 subset(A[1,], c(1:2,4), drop=TRUE),
                 subset(A[1,], c(1:2,4), drop=FALSE),
                 subset(A[1,], LETTERS[c(1:2,4)], drop=TRUE),
                 subset(A[1,], LETTERS[c(1:2,4)], drop=FALSE),
                 subset(A[1,], c(TRUE, TRUE, FALSE, TRUE), drop=TRUE),
                 subset(A[1,], c(TRUE, TRUE, FALSE, TRUE), drop=FALSE))

    # subsetting matrices
    expect_equal(A[,2:3],
                 subset(A, 2:3, along=2, drop=TRUE),
                 subset(A, 2:3, along=2, drop=FALSE),
                 subset(A, LETTERS[2:3], along=2, drop=TRUE),
                 subset(A, LETTERS[2:3], along=2, drop=FALSE),
                 subset(A, c(FALSE, TRUE, TRUE, FALSE, drop=TRUE)),
                 subset(A, c(FALSE, TRUE, TRUE, FALSE, drop=FALSE)))

    expect_equal(A[,3,drop=FALSE],
                 subset(A, 3, along=2, drop=FALSE),
                 subset(A, LETTERS[3], along=2, drop=FALSE),
                 subset(A, c(FALSE, FALSE, TRUE, FALSE, along=2, drop=FALSE)))

    # subsetting lists
    ll = list(a=1:5, b=letters[1:5])
    expect_equal(ll[[2]],
                 subset(ll, 2, drop=TRUE),
                 subset(ll, c(FALSE, TRUE), drop=TRUE),
                 subset(ll, "b", drop=TRUE))
    expect_equal(ll[1],
                 subset(ll, 1, drop=FALSE),
                 subset(ll, c(TRUE, FALSE), drop=FALSE),
                 subset(ll, "a", drop=FALSE),
                 tolerance = 1e-8, scale=NULL) #FIXME: why is this req'd only here?

    # how about factors, data.frames?
}
