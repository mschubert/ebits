library(dplyr)
.b = import_('../../base')
.io = import_('../../io')
.ar = import_('../../array')

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

gene.character = function(obj, to, from=guess_id_type(obj), summarize=mean) {
    lookup = .lookup[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    .b$match(obj, from=df$from, to=df$to)
}

gene.numeric = function(obj, to, from=guess_id_type(names(obj)), summarize=mean) {
    lookup = .lookup[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    .ar$summarize(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

gene.matrix = function(obj, to, from=guess_id_type(rownames(obj)), summarize=mean) {
    lookup = .lookup[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    .ar$summarize(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

gene.ExpressionSet = function(obj, to, from=guess_id_type(rownames(exprs(obj))),  summarize=mean) {
    lookup = .lookup[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    exprs(obj) = .ar$summarize(exprs(obj), along=1, from=df$from, to=df$to, FUN=summarize)
    obj
}

gene.list = function(obj, to, from, summarize=mean) {
    lapply(obj, gene)
}

#' Guesses the ID type from a character vector of IDs
#'
#' @param from_ids  Character vector of IDs
#' @return          Character string describing the ID type
guess_id_type = function(from_ids) {
    if (sum(grepl("^ENSG", from_ids)) > length(from_ids)/2)
        'ensembl_gene_id'
    else if (sum(grepl("_at$", from_ids)) > length(from_ids)/2)
        'affy'
    else if (sum(grepl("^ILMN", from_ids)) > length(from_ids)/2)
        'illumina'
    else if (sum(suppressWarnings(!is.na(as.numeric(from_ids)))) > length(from_ids)/2)
        'entrezgene'
    else if (sum(grepl("^[A-Z]{1,3}[0-9]{5,6}$", from_ids)) > length(from_ids)/2)
        'genbank'
    else
        stop("need to specify 'from' id type")
}

#' Creates a table of different identifiers and caches it
#'
#' @param force  Re-generate table if it already exists
#' @return       A data.frame with the following columns:
#'     hgnc_symbol, affy, illumina, genbank, entrezgeen, ensembl_gene_id
cache_lookup_table = function(force=FALSE) {
    cache = file.path(module_file(), "cache_lookup_table.RData")
    if (file.exists(cache) && !force)
        return(.io$load(cache))

    warning("no cached file found, biomart query will take ~ 1h", immediate.=TRUE)

    mart = biomaRt::useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")

    probes = list(
        affy = grep("^affy_", biomaRt::listAttributes(mart)$name, value=TRUE),
        illumina = grep("^illumina_", biomaRt::listAttributes(mart)$name, value=TRUE),
        agilent = grep("agilent_", biomaRt::listAttributes(mart)$name, value=TRUE)
    )

    tablecols = c("hgnc_symbol", "entrezgene", "ensembl_gene_id")
    getPS = function(p) {
        df = biomaRt::getBM(attributes=c(tablecols, p), mart=mart)
        colnames(df)[ncol(df)] = "probe_id"
        df[!is.na(df$probe_id) & df$probe_id!="",]
        as.data.frame(sapply(df, as.character, simplify=FALSE, USE.NAMES=TRUE))
    }
    assemblePS = function(p) {
        re = sapply(p, getPS, simplify=FALSE, USE.NAMES=TRUE) %>%
            dplyr::bind_rows() %>%
            dplyr::filter(probe_id != "" & !is.na(probe_id)) %>%
            dplyr::distinct()
        re$hgnc_symbol[re$hgnc_symbol == ""] = NA
        re$entrezgene[re$entrezgene == ""] = NA
        re$ensembl_gene_id[re$ensembl_gene_id == ""] = NA
        re
    }
    mapping = sapply(probes, assemblePS, simplify=FALSE, USE.NAMES=TRUE)

    save(mapping, file=cache)
    mapping
}
.lookup = cache_lookup_table()

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
