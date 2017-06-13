#' Subset a big matrix to maximise contained value with rows and columns
#'
#' @param A      The matrix to subset
#' @param nrows  Number of rows to keep in the subset
#' @param ncols  Number of columns to keep in subset
#' @param symmetric  Consider A symmetric (and subset only same indices)
#' @param return_indices  Return indices instead of subsetted matrix
#' @return       A submatrix of mat which maximises its content
subset_matrix = function(A, nrows, ncols=nrows, symmetric=FALSE,
                         return_indices=FALSE) {

    if (symmetric == FALSE)
        stop("not implemented")

    iterative_remove = function(mat, n) {
        if (nrow(mat) > n) {
            idx = which.min(rowSums(mat))
            iterative_remove(mat[-idx,-idx], n)
        } else 
            mat
    }

    shrink = A
    rownames(shrink) = 1:nrow(shrink)
    shrink = iterative_remove(shrink, nrows)
    idx = as.integer(rownames(shrink))

    if (return_indices && symmetric)
        idx
    else if (return_indices)
        list(idx, idx)
    else
        A[idx, idx]

}

if (is.null(module_name())) {
    dims = 100
    A = matrix(rnorm(dims^2), ncol=dims, nrow=dims)
    subs = 10
    idx = subset_matrix(abs(A), subs, symmetric=TRUE, return_indices=TRUE)
}
