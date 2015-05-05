#' mapping of annotation identifier to annotation package
#' there should be a mapping between the two available somewhere?
.gene = list(
    "pd.hg.u95a" = "hgu95a.db",
    "pd.hg.u95b" = "hgu95b.db",
    "pd.hg.u95av2" = "hgu95av2.db", # A-AFFY-1, GPL8300
    "pd.hg.u133a" = "hgu133a.db", # A-AFFY-33, GPL96
    "pd.hg.u133b" = "hgu133b.db", # A-AFFY-34, GPL97
    "pd.hg.u133.plus.2" = "hgu133plus2.db", # A-AFFY-44, GPL570
    "pd.hg.u133a.2" = "hgu133a2.db", # A-AFFY-37, GPL571
    "pd.hg.u219" = "hgu219.db", # A-GEOD-13667, GPL13667
    "pd.hugene.1.0.st.v1" = "hugene10sttranscriptcluster.db", # A-AFFY-141, GPL6244
    "pd.hugene.1.0.st.v1" = "hugene10sttranscriptcluster.db",
    "pd.hugene.1.1.st.v1" = "hugene11sttranscriptcluster.db",
    "pd.hugene.2.0.st" = "hugene20sttranscriptcluster.db",
    "pd.hugene.2.1.st" = "hugene21sttranscriptcluster.db",
    "pd.huex.1.0.st.v1" = "huex10sttranscriptcluster.db", # A-AFFY-143
    "pd.huex.1.0.st.v2" = "huex10sttranscriptcluster.db",
    "pd.ht.hg.u133a" = "hthgu133a.db", # A-AFFY-76
#    "pd.ht.hg.u133.plus.pm" = ???,
    "pd.hta.2.0" = "hta20sttranscriptcluster.db"
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
    if (is.null(pkg))
        stop(paste("No annotation mapping for:", normData@annotation))
    if (!require(pkg, character.only=TRUE)) {
        source("http://bioconductor.org/biocLite.R")
        biocLite(pkg)
        library(pkg, character.only=TRUE)
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
