library(dplyr)
.b = import_('../../base')
.io = import_('../../io')
.ar = import_('../../array')
.guess_id_type = import('./guess_id_type')$guess_id_type

#' Gene ID mapping function
#'
#' @param obj        a character vector or matrix with ids (rownames) to be mapped
#' @param from       the type of ids to map from; if NULL will try regex matching
#' @param to         the type of ids to map to
#' @param summarize  the function to use to aggregate ids
#' @return           the mapped and optionally summarized object
gene = function(obj, from=NULL, to, summarize=mean) {
    UseMethod("gene")
}

gene.character = function(obj, to, from=.guess_id_type(obj), summarize=mean) {
    lookup = gene_table()
    df = na.omit(data.frame(from=lookup[[from]], to=lookup[[to]]))
    df = df[!duplicated(df),]
    .b$match(obj, from=df$from, to=df$to)
}

gene.numeric = function(obj, to, from=.guess_id_type(names(obj)), summarize=mean) {
    lookup = gene_table()
    df = na.omit(data.frame(from=lookup[[from]], to=lookup[[to]]))
    df = df[!duplicated(df),]
    .ar$summarize(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

gene.matrix = function(obj, to, from=.guess_id_type(rownames(obj)), summarize=mean) {
    lookup = gene_table()
    df = na.omit(data.frame(from=lookup[[from]], to=lookup[[to]]))
    df = df[!duplicated(df),]
    .ar$summarize(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

gene.ExpressionSet = function(obj, to, from=.guess_id_type(rownames(exprs(obj))),  summarize=mean) {
    lookup = gene_table()
    df = na.omit(data.frame(from=lookup[[from]], to=lookup[[to]]))
    df = df[!duplicated(df),]
    exprs(obj) = .ar$summarize(exprs(obj), along=1, from=df$from, to=df$to, FUN=summarize)
    obj
}

gene.list = function(obj, to, from, summarize=mean) {
    lapply(obj, gene)
}

#' Creates a table of different identifiers and caches it
#'
#' @param force  Re-generate table if it already exists
#' @return       A data.frame with the following columns:
#'     hgnc_symbol, affy, illumina, genbank, entrezgeen, ensembl_gene_id
gene_table = function(force=FALSE) {
    cache = file.path(module_file(), "gene_table.RData")
    if (file.exists(cache) && !force)
        return(.io$load(cache))

    mart = biomaRt::useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")
    ids = c('hgnc_symbol', 'entrezgene', 'ensembl_gene_id')
    mapping = biomaRt::getBM(attributes=ids, mart=mart)

    save(mapping, file=cache)
    mapping
}

if (is.null(module_name())) {
    library(testthat)

    expect_equal(gene('683_at', to="hgnc_symbol"),
                 setNames("OTC", "683_at"))

    #FIXME: colnames gets converted from 'x' to '1' for single-column
#    m = matrix(1, nrow=2, ncol=1, dimnames=list(c('683_at','683_at'), 'x'))
#    M = gene(m, to="hgnc_symbol")
#    Mref = matrix(1, ncol=1, nrow=1, dimnames=list("OTC", "1"))
#    expect_equal(M, Mref)

    m = matrix(1, nrow=2, ncol=2,
               dimnames=list(c('683_at','683_at'), c('x','y')))
    M = gene(m, to="hgnc_symbol")
    Mref = structure(c(1, 1), .Dim = 1:2,
                     .Dimnames = list("OTC", c("x", "y")))
    expect_equal(M, Mref)
}
