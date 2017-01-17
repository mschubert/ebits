#' Perform variance-stabilizing transformation
#'
#' @param mat  The untransformed matrix [genes x samples]
#' @return     A matrix of gene expression values [genes x samples]
vst = function(mat, colData=data.frame(id=colnames(mat)), design=~1) {
    UseMethod("vst")
}

#' @rdname vst
vst.matrix = function(mat, colData=data.frame(id=colnames(mat)), design=~1) {
    cds = DESeq2::DESeqDataSetFromMatrix(mat, colData, design)
    vst = vst(cds)
}

#' @rdname vst
vst.DESeqDataSet = function(cds) {
    cds = DESeq2::estimateSizeFactors(cds)
    cds = DESeq2::estimateDispersions(cds)
    vst = DESeq2::getVarianceStabilizedData(cds)
}

#' @rdname vst
vst.list = function(mlist, ...) {
	lapply(mlist, vst, ...)
}
