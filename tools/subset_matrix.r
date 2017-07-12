#' Subset a big matrix to maximise contained value with rows and columns
#'
#' @param A      The matrix to subset
#' @param nrows  Number of rows to keep in the subset
#' @param ncols  Number of columns to keep in subset
#' @param symmetric  Consider A symmetric (and subset only same indices)
#' @param return_indices  Return indices instead of subsetted matrix
#' @return       A submatrix of mat which maximises its content
subset_matrix = function(A, nrows=2, ncols=nrows, symmetric=FALSE,
                         return_indices=FALSE) {

    if (symmetric == FALSE)
        stop("not implemented")
    else
        stopifnot(A == t(A))

    idx = list()
    iterative_remove = function(mat) {
        if (nrow(mat) > 1 && ncol(mat) > 1) {
            ri = which.min(rowSums(mat))
            ci = which.min(colSums(mat))

            nri = rownames(mat)[ri]
            nci = colnames(mat)[ci]
#            print(c(nri,nci))

            assign("idx", c(idx, list(as.integer(c(nri,nci)))),
                   envir=parent.env(environment()))

            iterative_remove(mat[-ri,-ci,drop=FALSE])
        }
    }

    shrink = A
    rownames(shrink) = 1:nrow(shrink)
    colnames(shrink) = 1:ncol(shrink)
    iterative_remove(shrink)

    idx = rev(idx)
    rows = sapply(idx, function(x) x[1])
    cols = sapply(idx, function(x) x[2])

    if (return_indices && symmetric)
        rows
    else if (return_indices && symmetric)
        idx
    else
        A[head(rows,nrows), head(cols,ncols)]
}

if (is.null(module_name())) {
    library(testthat)

    dims = 25
    A = matrix(rnorm(dims^2), ncol=dims, nrow=dims)
    A = abs(A + t(A))
    rownames(A) = colnames(A) = sample(letters, nrow(A), replace=TRUE)

    n_subs = 2

    subA = subset_matrix(A, n_subs, symmetric=TRUE, return_indices=FALSE)
    idx = subset_matrix(A, symmetric=TRUE, return_indices=TRUE)

    expect_equal(sort(rownames(subA)), sort(rownames(A)[head(idx, n_subs)]))
}
