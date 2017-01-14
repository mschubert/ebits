library(dplyr)
.b = import_('../../base')
.io = import_('../../io')
.ar = import_('../../array')
.guess_id_type = import('./guess_id_type')$guess_id_type

#' probeset ID mapping function
#'
#' @param obj        a character vector or matrix with ids (rownames) to be mapped
#' @param from       the type of ids to map from; if NULL will try regex matching
#' @param to         the type of ids to map to
#' @param summarize  the function to use to aggregate ids
#' @return           the mapped and optionally summarized object
probeset = function(obj, from=NULL, to, summarize=mean) {
    UseMethod("probeset")
}

probeset.character = function(obj, to, from=.guess_id_type(obj), summarize=mean) {
    lookup = probeset_table()[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    .b$match(obj, from=df$from, to=df$to)
}

probeset.numeric = function(obj, to, from=.guess_id_type(names(obj)), summarize=mean) {
    lookup = probeset_table()[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    .ar$summarize(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

probeset.matrix = function(obj, to, from=.guess_id_type(rownames(obj)), summarize=mean) {
    lookup = probeset_table()[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    .ar$summarize(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

probeset.ExpressionSet = function(obj, to, from=.guess_id_type(rownames(exprs(obj))),  summarize=mean) {
    lookup = probeset_table()[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    exprs(obj) = .ar$summarize(exprs(obj), along=1, from=df$from, to=df$to, FUN=summarize)
    obj
}

probeset.list = function(obj, to, from, summarize=mean) {
    lapply(obj, probeset)
}

#' Creates a table of different identifiers and caches it
#'
#' @param force  Re-probesetrate table if it already exists
#' @return       A data.frame with the following columns:
#'     hgnc_symbol, affy, illumina, genbank, entrezgeen, ensembl_probeset_id
probeset_table = function(force=FALSE) {
    cache = module_file("probeset_table.RData")
    if (file.exists(cache) && !force)
        return(.io$load(cache))

    warning("no cached file found, biomart query will take ~ 1h", immediate.=TRUE)

    mart = biomaRt::useMart(biomart="ensembl", dataset="hsapiens_probeset_ensembl")

    probes = list(
        affy = grep("^affy_", biomaRt::listAttributes(mart)$name, value=TRUE),
        illumina = grep("^illumina_", biomaRt::listAttributes(mart)$name, value=TRUE),
        agilent = grep("agilent_", biomaRt::listAttributes(mart)$name, value=TRUE)
    )

    tablecols = c("hgnc_symbol", "entrezprobeset", "ensembl_probeset_id")
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
        re$entrezprobeset[re$entrezprobeset == ""] = NA
        re$ensembl_probeset_id[re$ensembl_probeset_id == ""] = NA
        re
    }
    mapping = sapply(probes, assemblePS, simplify=FALSE, USE.NAMES=TRUE)

    save(mapping, file=cache)
    mapping
}

if (is.null(module_name())) {
    library(testthat)

    expect_equal(probeset('683_at', to="hgnc_symbol"),
                 setNames("OTC", "683_at"))

    #FIXME: colnames gets converted from 'x' to '1' for single-column
#    m = matrix(1, nrow=2, ncol=1, dimnames=list(c('683_at','683_at'), 'x'))
#    M = probeset(m, to="hgnc_symbol")
#    Mref = matrix(1, ncol=1, nrow=1, dimnames=list("OTC", "1"))
#    expect_equal(M, Mref)

    m = matrix(1, nrow=2, ncol=2,
               dimnames=list(c('683_at','683_at'), c('x','y')))
    M = probeset(m, to="hgnc_symbol")
    Mref = structure(c(1, 1), .Dim = 1:2,
                     .Dimnames = list("OTC", c("x", "y")))
    expect_equal(M, Mref)
}
