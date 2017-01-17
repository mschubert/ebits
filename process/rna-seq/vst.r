#' Perform variance-stabilizing transformation
#'
#' @param mat  The untransformed matrix [genes x samples]
#' @return     A matrix of gene expression values [genes x samples]
vst = function(mat, conditions=NULL) UseMethod("vst")

#' @rdname vst
vst.default = function(mat, conditions) {
	cds = DEseq2::newCountDataSet(countData=dat, conditions=factor(conditions))
	cds = DEseq2::estimateSizeFactors(cds)
	cds = DEseq2::estimateDispersions(cds, sharingMode="gene-est-only",
		method = "pooled", fitType = "local")
	vst = DEseq2::getVarianceStabilizedData(cds)
}

#' @rdname vst
vst.list = function(mlist) {
	lapply(mlist, vst)
}
