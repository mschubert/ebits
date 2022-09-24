#' Compute prcomp for a DESeq2 data set
#'
#' @param ntop  Number of top variance genes to consider
#' @return  Result of stats::prcomp
prcomp = function(obj, ...) {
    UseMethod("prcomp")
}

prcomp.DESeqDataSet = function(eset, ntop=500) {
    vst = DESeq2::varianceStabilizingTransformation(eset)
    prcomp(vst, ntop=ntop)
}

prcomp.DESeqTransform = function(vst, ntop=500) {
    # same as DESeq2::plotPCA.DESeqTransform
    mat = SummarizedExperiment::assay(vst)
    rv = genefilter::rowVars(mat)
    keep = order(rv, decreasing=TRUE)[seq_len(min(ntop, length(rv)))]

    stats::prcomp(t(mat[keep,]))
}
