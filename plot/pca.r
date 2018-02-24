#' GGplot2 PCA plot of different base R objects
#'
#' @param obj    A PCA object (e.g. from prcomp function)
#' @param aes    Aesthetics mapping; use "PC<num>" for principal components
#' @param annot  Additional data.frame with sample annotation
#' @return       ggplot2 object including PCs and sample annotations
pca = function(obj, aes, annot=NULL) {
    UseMethod("pca")
}

pca.prcomp = function(obj, aes, annot=NULL) {
    ggplot(cbind(annot, obj$x), aes=aes)
}
