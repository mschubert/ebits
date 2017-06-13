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
        if (nrow(mat) > n && ncol(mat) > n) {
            ri = which.min(rowSums(mat))
            ci = which.min(colSums(mat))
            iterative_remove(mat[-ri,-ci], n)
        } else 
            mat
    }

    shrink = A
    rownames(shrink) = 1:nrow(shrink)
    colnames(shrink) = 1:ncol(shrink)
    shrink = iterative_remove(shrink, nrows)

    ri = as.integer(rownames(shrink))
    ci = as.integer(colnames(shrink))

    if (return_indices)
        list(ri, ci)
    else
        A[ri, ci]

}

if (is.null(module_name())) {
    dims = 100
    A = matrix(rnorm(dims^2), ncol=dims, nrow=dims)
    subs = 10
    idx = subset_matrix(abs(A), subs, symmetric=TRUE, return_indices=TRUE)
}
