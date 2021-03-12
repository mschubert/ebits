library(dplyr)
.b = import('../../base')
.io = import('../../io')
.guess = import('./guess')
.gene_table = import('../../seq/gene_table')$gene_table

#' Gene ID mapping function
#'
#' @param obj   a character vector or named object to be mapped
#' @param from  the type of ids to map from; if NULL will try regex matching
#'              this can be: 'hgnc_symbol', 'entrezgene', 'ensembl_gene_id',
#'              'band', 'chromosome_name', 'transcript_start', 'transcript_end',
#'              'transcription_start_site'
#' @param to    the type of ids to map to (same types as for 'from')
#' @param dset  Ensembl data set, e.g. '{hsapiens,mmusculus}_gene_ensembl'
#' @param summarize  the function to use to aggregate ids
#' @return      the mapped and optionally summarized object
gene = function(obj, from=NULL, to, dset, summarize=mean) {
    UseMethod("gene")
}

gene.character = function(obj, to, from=.guess$id_type(obj), dset=.guess$dset(obj), summarize=mean) {
    if (to %in% c("hgnc_symbol", "mgi_symbol"))
        to = "external_gene_name"

    lookup = .gene_table(dset)
    df = na.omit(data.frame(from = as.character(lookup[[from]]),
                            to = as.character(lookup[[to]])))
    df = df[!duplicated(df),]
    .b$match(obj, from=df$from, to=df$to)
}

gene.matrix = function(obj, to, from=.guess$id_type(narray::dimnames(obj, along=1)),
                       dset=.guess$dset(narray::dimnames(obj, along=1)), summarize=mean) {
    if (to %in% c("hgnc_symbol", "mgi_symbol"))
        to = "external_gene_name"

    lookup = .gene_table(dset)
    df = na.omit(data.frame(from = as.character(lookup[[from]]),
                            to = as.character(lookup[[to]])))
    df = df[!duplicated(df),]

    # remove versions of gene ids
    if (from == "ensembl_gene_id") {
        if (is.array(obj))
            dimnames(obj)[[1]] = sub("\\.[0-9]+$", "", dimnames(obj)[[1]])
        else
            names(obj) = sub("\\.[0-9]+$", "", dimnames(obj)[[1]])
    }

    narray::translate(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

gene.ExpressionSet = function(obj, to, from=.guess$id_type(rownames(exprs(obj))),
                              dset=.guess$dset(rownames(exprs(obj))), summarize=mean) {
    exprs(obj) = gene(Biobase::exprs(obj), from=from, to=to, dset=dset, summarize=summarize)
    obj
}

gene.list = function(obj, to, from, dset, summarize=mean) {
    lapply(obj, gene, to=to, from=from, dset=dset, summarize=summarize)
}

gene.default = function(obj, to, from, dset, summarize=mean) {
    stop("Trying to map ", mode(obj), " object, did you forget as.character()?")
}

if (is.null(module_name())) {
    library(testthat)

    re = gene("10009", to="hgnc_symbol")
    expect_equal("ZBTB33", unname(re))
}
