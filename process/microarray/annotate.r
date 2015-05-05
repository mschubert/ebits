#' mapping of annotation identifier to annotation package
.gene = list(
    # A-AFFY-33, GPL96
    "pd.hg.u133a" = "hgu133a.db",
    # A-AFFY-34, GPL97
    "pd.hg.u133b" = "hgu133b.db",
    # A-AFFY-44, GPL570
    "pd.hg.u133.plus.2" = "hgu133plus2.db",
    # A-AFFY-37, GPL571
    "pd.hg.u133a.2" = "hgu133a2.db",
    # A-GEOD-13667, GPL13667
    "pd.hg.u219" = "hgu219.db",
    # A-AFFY-141, GPL6244
    "pd.hugene.1.0.st.v1" = "hugene10sttranscriptcluster.db"
)

#' Function to annotate expression objects
#'
#' @param normData   A normalized data object, or a list thereof
#' @param summarize  IDs to annotate with: hgnc_symbol
#' @return           The annotated data
annotate = function(normData, summarize="hgnc_symbol") {
    UseMethod("annotate")
}

annotate.list = function(normData, summarize="hgnc_symbol") {
    lapply(normData, annotate)
}

annotate.ExpressionSet = function(normData, summarize="hgnc_symbol") {
    stopifnot(summarize == "hgnc_symbol")

    # load annotation package
    pkg = .gene[[normData@annotation]]
    if (!require(pkg, character.only=TRUE)) {
        source("http://bioconductor.org/biocLite.R")
        biocLite(pkg)
    }
    sym = annotate::getSYMBOL(featureNames(normData), pkg)

    # work on expression matrix, summarize using limma
    mat = as.matrix(exprs(normData))
    rownames(mat) = sym
    mat = limma::avereps(mat[!is.na(rownames(mat)),])

    # set matrix to be expression data of object
    exprs(normData) = mat
    normData
}
