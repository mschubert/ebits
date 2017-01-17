#' Perform a voom transformation (for RNA-seq data)
#'
#' @param mat  The untransformed matrix [genes x samples]
#' @param ids  The ID type of genes (only "hgnc" allowed right now)
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
	lapply(mlist, voom)
}
