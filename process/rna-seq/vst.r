#' Perform variance-stabilizing transformation
#'
#' @param mat  The untransformed matrix [genes x samples]
#' @return     A matrix of gene expression values [genes x samples]
vst = function(mat, colData=data.frame(id=colnames(mat)), design=~1, ...) {
    UseMethod("vst")
}

#' @rdname vst
vst.matrix = function(mat, colData=data.frame(id=colnames(mat)), design=~1, ...) {
    cds = DESeq2::DESeqDataSetFromMatrix(mat, colData, design)
    vst = vst(cds, ...)
}

#' @rdname vst
vst.DESeqDataSet = function(cds, fitType="parametric") {
    cds = DESeq2::estimateSizeFactors(cds)
    cds = DESeq2::estimateDispersions(cds, fitType=fitType)
    vst = DESeq2::getVarianceStabilizedData(cds)
}

#' @rdname vst
vst.list = function(mlist, ...) {
	re = lapply(mlist, function(x) try(vst(x, ...)))
    errors = sapply(re, function(x) class(x) == "try-error")
    if (any(errors)) {
        for (i in which(errors))
            warning("In element ", i, " ", names(re)[i], ": ", re[[i]])
        stop("Errors occurred in processing")
    }
    re
}
