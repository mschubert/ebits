#' Perform a voom transformation (for RNA-seq data)
#'
#' @param mat  The untransformed matrix [genes x samples]
#' @return     A matrix of gene expression values [genes x samples]
voom = function(mat) UseMethod("voom")

#' @rdname voom
voom.default = function(mat) {
    # apply voom transformation
    mat = limma::voom(mat)$E
    rownames(mat) = sub("\\|[0-9]+$", "", rownames(mat))
    mat = mat[rownames(mat) != "?",]
    limma::avereps(mat)
}

#' @rdname voom
voom.list = function(mlist) {
	re = lapply(mlist, function(x) try(vst(x, ...)))
    errors = sapply(re, function(x) class(x) == "try-error")
    if (any(errors)) {
        for (i in which(errors))
            warning("In element ", i, " ", names(re)[i], ": ", re[[i]])
        stop("Errors occurred in processing")
    }
    re
}
