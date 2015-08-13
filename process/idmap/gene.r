.b = import('../../base')
.io = import('../../io')
.ar = import('../../array')

#' Gene ID mapping function
#'
#' @param obj        a character vector or matrix with ids (rownames) to be mapped
#' @param from       the type of ids to map from; if NULL will try regex matching
#' @param to         the type of ids to map to
#' @param summarize  the function to use to aggregate ids
gene = function(obj, from=NULL, to, summarize=mean) {
    UseMethod("gene")
}

gene.character = function(obj, to, from=.guess(obj), summarize=mean) {
    df = na.omit(data.frame(from=.lookup[[from]], to=.lookup[[to]]))
    df = df[!duplicated(df),]
    .b$match(obj, from=df$from, to=df$to)
}

gene.numeric = function(obj, to, from=.guess(names(obj)), summarize=mean) {
    df = na.omit(data.frame(from=.lookup[[from]], to=.lookup[[to]]))
    df = df[!duplicated(df),]
    .ar$summarize(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

gene.matrix = function(obj, to, from=.guess(rownames(obj)), summarize=mean) {
    df = na.omit(data.frame(from=.lookup[[from]], to=.lookup[[to]]))
    df = df[!duplicated(df),]
    .ar$summarize(obj, along=1, from=df$from, to=df$to, FUN=summarize)
}

gene.ExpressionSet = function(obj, to, from=.guess(rownames(exprs(obj))),  summarize=mean) {
    df = na.omit(data.frame(from=.lookup[[from]], to=.lookup[[to]]))
    df = df[!duplicated(df),]
    exprs(obj) = .ar$summarize(exprs(obj), along=1, from=df$from, to=df$to, FUN=summarize)
    obj
}

gene.list = function(obj, to, from, summarize=mean) {
    lapply(obj, gene)
}

.guess = function(from_ids) {
    if (sum(grepl("^ENSG", from_ids)) > length(from_ids)/2)
        'ensembl_gene_id'
    else if (sum(grepl("_at$", from_ids)) > length(from_ids)/2)
        'probe_id'
    else if (sum(grepl("^ILMN", from_ids)) > length(from_ids)/2)
        'probe_id'
    else if (sum(suppressWarnings(!is.na(as.numeric(from_ids)))) > length(from_ids)/2)
        'entrezgene'
    else
        stop("need to specify 'from' id type")
}
guess = .guess

.geneTable = function() {
    cache = file.path(module_file(), "geneTable.RData")
    if (file.exists(cache))
        return(.io$load(cache))

    warning("no cached file found, biomart query will take ~ 1h")

    mart = biomaRt::useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")
    probes = c(grep("^affy_", biomaRt::listAttributes(mart)$name, value=T),
               grep("^illumina_", biomaRt::listAttributes(mart)$name, value=T))

    tablecols = c("entrezgene", "ensembl_gene_id", "hgnc_symbol")
    getPS = function(p) {
        df = biomaRt::getBM(attributes=c(tablecols, p), mart=mart)
        colnames(df)[ncol(df)] = "probe_id"
        df[!is.na(df$probe_id) & df$probe_id!="",]
    }
    psdf = lapply(probes, getPS)

    allfrom_idsets = do.call(rbind, psdf)
    allfrom_idsets$entrezgene = as.character(allfrom_idsets$entrezgene)
    allfrom_idsets = allfrom_idsets[!duplicated(allfrom_idsets),]
    allfrom_idsets[allfrom_idsets == ""] = NA

    save(allfrom_idsets, file=cache)
    allfrom_idsets
}
geneTable = .geneTable
.lookup = .geneTable()

if (is.null(module_name())) {
    testthat::expect_equal(gene('683_at', to="hgnc_symbol"),
                           setNames("OTC", "683_at"))

    #FIXME: column name is dropped, should not be
    #m = matrix(1, nrow=2, ncol=1, dimnames=list(c('683_at','683_at'), 'x'))
    #gene(m, to="hgnc_symbol")

    m = matrix(1, nrow=2, ncol=2,
               dimnames=list(c('683_at','683_at'), c('x','y')))
    M = gene(m, to="hgnc_symbol")
    Mref = structure(c(1, 1), .Dim = 1:2,
                     .Dimnames = list("OTC", c("x", "y")))
    testthat::expect_equal(M, Mref)
}
