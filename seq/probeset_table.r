io = import('io')

#' Creates a table of different identifiers and caches it
#'
#' @param force  Re-probesetrate table if it already exists
#' @return       A data.frame with the following columns:
#'     hgnc_symbol, affy, illumina, genbank, entrezgene, ensembl_gene_id
probeset_table = function(force=FALSE) {
    cache = file.path(module_file(), "cache", "probeset_table.RData")
    if (file.exists(cache) && !force)
        return(io$load(cache))

    warning("no cached file found, biomart query will take ~ 1h", immediate.=TRUE)

    mart = biomaRt::useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")

    probes = list(
        affy = grep("^affy_", biomaRt::listAttributes(mart)$name, value=TRUE),
        illumina = grep("^illumina_", biomaRt::listAttributes(mart)$name, value=TRUE),
        agilent = grep("agilent_", biomaRt::listAttributes(mart)$name, value=TRUE)
    )

    tablecols = c("hgnc_symbol", "entrezgene", "ensembl_gene_id")
    getPS = function(p) {
        message(p)
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

    dir.create(dirname(cache), showWarnings=FALSE)
    save(mapping, file=cache)
    mapping
}
