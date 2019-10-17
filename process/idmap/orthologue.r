library(dplyr)
.b = import('../../base')
.io = import('../../io')
.guess = import('./guess')
.ortho_table = import('../../seq/orthologue_table')$ortho_table

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
orthologue = function(obj, from=NULL, to, dset=NULL, summarize=mean) {
    UseMethod("orthologue")
}

orthologue.character = function(obj, to, from=.guess$id_type(obj),
                          dset=.guess$dset(obj), summarize=mean) {
    if (to == "mgi_symbol")
        to = "mmusculus_homolog_associated_gene_name"
    else if (to == "hgnc_symbol")
        to = "hsapiens_homolog_associated_gene_name"
    if (from %in% c("hgnc_symbol", "mgi_symbol"))
        from = "external_gene_name"

    lookup = .ortho_table(dset, to=sub("^([^_]+).*", "\\1", to))
    df = na.omit(data.frame(from=lookup[[from]], to=lookup[[to]]))
    df = df[!duplicated(df),]
    .b$match(obj, from=df$from, to=df$to)
}

orthologue.default = function(obj, to, from=.guess$id_type(narray::dimnames(obj, along=1)),
                              dset=.guess$dset(obj), summarize=mean) {
    if (to == "mgi_symbol")
        to = "mmusculus_homolog_associated_gene_name"
    else if (to == "hgnc_symbol")
        to = "hsapiens_homolog_associated_gene_name"

    lookup = .ortho_table(dset, to=sub("^([^_]+).*", "\\1", to))
    df = na.omit(data.frame(from=lookup[[from]], to=lookup[[to]]))
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

orthologue.ExpressionSet = function(obj, to, from=.guess$id_type(rownames(exprs(obj))),
                              dset=.guess$dset(obj), summarize=mean) {
    exprs(obj) = orthologue(Biobase::exprs(obj), from=from, to=to, dset=dset, summarize=summarize)
    obj
}

orthologue.list = function(obj, to, from, dset, summarize=mean) {
    lapply(obj, orthologue, to=to, from=from, dset=dset, summarize=summarize)
}

if (is.null(module_name())) {
    library(testthat)

    re = orthologue("TP53", to="mgi_symbol")
    expect_equal("Trp53", unname(re))

    re = orthologue("Trp53", to="hgnc_symbol")
    expect_equal("TP53", unname(re))
}
