library(dplyr)
.b = import('../../base')
.io = import('../../io')
.guess_id_type = import('./guess_id_type')$guess_id_type
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
orthologue = function(obj, from=NULL, to, dset="hsapiens_gene_ensembl", summarize=mean) {
    UseMethod("orthologue")
}

orthologue.character = function(obj, to, from=.guess_id_type(obj),
                          dset="hsapiens_gene_ensembl", summarize=mean) {
    if (to == "mgi_symbol")
        to = "mmusculus_homolog_associated_gene_name"
    else if (to == "hgnc_symbol")
        to = "hsapiens_homolog_associated_gene_name"

    lookup = .ortho_table(dset, to=sub("^([^_]+).*", "\\1", to))
    df = na.omit(data.frame(from=lookup[[from]], to=lookup[[to]]))
    df = df[!duplicated(df),]
    .b$match(obj, from=df$from, to=df$to)
}

orthologue.default = function(obj, to, from=.guess_id_type(narray::dimnames(obj, along=1)),
                        dset="hsapiens_gene_ensembl", summarize=mean) {
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

orthologue.ExpressionSet = function(obj, to, from=.guess_id_type(rownames(exprs(obj))),
                              dset="hsapiens_gene_ensembl", summarize=mean) {
    exprs(obj) = orthologue(Biobase::exprs(obj), from=from, to=to, dset=dset, summarize=summarize)
    obj
}

orthologue.list = function(obj, to, from, dset="hsapiens_gene_ensembl", summarize=mean) {
    lapply(obj, orthologue, to=to, from=from, dset=dset, summarize=summarize)
}
