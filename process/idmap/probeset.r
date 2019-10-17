library(dplyr)
.b = import('../../base')
.guess = import('./guess')
.probeset_table = import('../../seq/probeset_table')$probeset_table

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

probeset.character = function(obj, to, from=.guess$id_type(obj), summarize=mean) {
    lookup = .probeset_table()[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    .b$match(obj, from=df$from, to=df$to)
}

probeset.numeric = function(obj, to, from=.guess$id_type(names(obj)), summarize=mean) {
    lookup = .probeset_table()[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    narray::translate(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

probeset.matrix = function(obj, to, from=.guess$id_type(rownames(obj)), summarize=mean) {
    lookup = .probeset_table()[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    narray::translate(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

probeset.ExpressionSet = function(obj, to, from=.guess$id_type(rownames(exprs(obj))),  summarize=mean) {
    lookup = .probeset_table()[[from]]
    df = na.omit(data.frame(from=lookup$probe_id, to=lookup[[to]]))
    df = df[!duplicated(df),]
    exprs(obj) = narray::translate(exprs(obj), along=1, from=df$from, to=df$to, FUN=summarize)
    obj
}

probeset.list = function(obj, to, from, summarize=mean) {
    lapply(obj, probeset, to=to, from=from, summarize=summarize)
}

if (is.null(module_name())) {
    library(testthat)

    cache = file.path("../../seq/cache", "probeset_table.RData")
    if (!file.exists(cache)) {
        warning("no cache available: skipping probeset test")
        quit(save="no", status=0)
    }

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

    M2 = probeset(m, to="ensembl_gene_id")
    M2ref = structure(c(1, 1), .Dim = 1:2,
                      .Dimnames = list("ENSG00000036473", c("x", "y")))
    expect_equal(M2, M2ref)
}
