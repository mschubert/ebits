.b = import('base/operators')
.idmap = import('../idmap')
.guess = import('../idmap/guess')

#' Mapping of annotation identifier to annotation package
#'
#' there should be a mapping between the two available somewhere?
#' List of BioC annots: https://www.bioconductor.org/packages/3.3/data/annotation/
mapping = list(
    "pd.hg.u95a" = "hgu95a.db",
    "pd.hg.u95b" = "hgu95b.db",
    "pd.hg.u95av2" = "hgu95av2.db", # A-AFFY-1, GPL8300
    "hgu133a" = "hgu133a.db", # A-GEOD-14668
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
    "pd.hta.2.0" = "hta20transcriptcluster.db",
    "u133aaofav2" = "hthgu133a.db", # needs to be manually processed by affy
    "pd.mouse430.2" = "mouse4302.db"
)

#' Function to annotate expression objects
#'
#' @param normData   A normalized data object, or a list thereof
#' @param summarize  IDs to annotate with: external_gene_name
#' @param drop       For lists, drop arrays that could not be mapped instead of error
#' @param dset       Annotation data set (ensembl biomart, e.g. hsapiens_gene_ensembl)
#' @return           The annotated data
annotate = function(normData, summarize="external_gene_name", ..., dset=NULL) {
    UseMethod("annotate")
}

annotate.list = function(normData, summarize="external_gene_name", dset=NULL, drop=FALSE) {
    re = lapply(normData, function(x, ...) annotate(x, ...) %catch% NA, summarize=summarize, dset=dset)
    if (all(is.na(re)))
        stop("All annotations failed")
    if (any(is.na(re))) {
        fails = paste(names(re)[is.na(re)], collapse=", ")
        if (drop)
            warning("Dropping ", fails, immediate.=TRUE)
        else
            stop("Arrays could not be mapped: ", fails)
    }
    re[!is.na(re)]
}

annotate.ExpressionSet = function(normData, summarize="external_gene_name", dset=NULL) {
    if (is.null(dset)) {
        anno = Biobase::annotation(normData)
        if (grepl("mouse", anno)) {
            dset = "mmusculus_gene_ensembl"
        } else {
            dset = "hsapiens_gene_ensembl" # assume human if it's not clearly mouse, fix if required
        }
        message("[microarray/annotate] mapping ", sQuote(anno), " -> ", sQuote(dset))
    }
    emat = annotate(as.matrix(normData), summarize=summarize, dset=dset)
    Biobase::ExpressionSet(assayData = emat,
                           phenoData = Biobase::phenoData(normData))
}

annotate.NChannelSet = function(normData, summarize="external_gene_name", dset=NULL) {
    if (! "E" %in% ls(assayData(normData)))
        stop("only single-channel implemented atm")

    # note: there are agilent annotation packages available, but the
    # array ID is not saved in normData@annotation
    map_channel = function(expr, ids, from="agilent", to=summarize) {
        rownames(expr) = ids
        .idmap$probeset(expr, from=from, to=summarize)
    }
    ad = as.list(Biobase::assayData(normData))
    mapped = sapply(ad, map_channel, ids=fData(normData)$ProbeName,
                    simplify=FALSE, USE.NAMES=TRUE)

    es = ExpressionSet(mapped$E)
    phenoData(es) = phenoData(normData)
    es
}

annotate.matrix = function(normData, summarize="external_gene_name", dset=NULL) {
    if (is.null(dset))
        dset = .guess$dset(rownames(normData))
    rownames(normData) = .idmap$probeset(rownames(normData), to=summarize, dset=dset)
    limma::avereps(normData[!is.na(rownames(normData)),])
}
