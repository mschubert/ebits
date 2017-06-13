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

    clust = hclust(dist(A), method="complete")
    idx = which(clust$order <= nrows)

    if (return_indices && symmetric)
        idx
    else if (return_indices)
        list(idx, idx)
    else
        A[idx, idx]

}

if (is.null(module_name())) {
    # this needs hours of CPU and > 40 GB memory
    dims = 100
    A = matrix(rnorm(dims^2), ncol=dims, nrow=dims)
    subs = 10
    idx = subset_matrix(abs(A), subs, symmetric=TRUE, return_indices=TRUE)
}
