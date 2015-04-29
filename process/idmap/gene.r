.b = import('../base')
.io = import('../io')
.ar = import('../array')
.lookup = NULL

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

#' Gene ID mapping function
#'
#' @param obj            a character vector or matrix with ids (rownames) to be mapped
#' @param from           the type of ids to map from; if NULL will try regex matching
#' @param to             the type of ids to map to
#' @param fun.aggregate  the function to use to aggregate ids
gene = function(obj, from=NULL, to, fun.aggregate=mean) {
    if (!is.null(from) && from == to)
        return(x) #TODO: where not duplicated entries?

    if (is.matrix(obj)) # remove version numbers from identifiers
        from_ids = rownames(obj) = .b$grep("^([a-zA-Z0-9_]+)", rownames(obj))
    else
        from_ids = obj

    if (is.null(.lookup))
        assign('.lookup', .geneTable(), envir=parent.env(environment()))

    if (is.null(from)) {
        if (sum(grepl("^ENSG", from_ids)) > length(from_ids)/2)
            from = 'ensembl_gene_id'
        else if (sum(grepl("_at|ILMN", from_ids)) > length(from_ids)/2)
            from = 'probe_id'
        else if (sum(suppressWarnings(!is.na(as.numeric(from_ids)))) > length(from_ids)/2)
            from = 'entrezgene'
        else
            from = 'hgnc_symbol'
    }

    .ar$summarize(obj, from=.lookup[[from]],
                  to=.lookup[[to]], FUN=fun.aggregate)
}
