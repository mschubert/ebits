`%>%` = magrittr::`%>%`

#' Creates a table of different identifiers and caches it
#'
#' @param dset   Ensembl organism, e.g.: 'hsapiens_gene_ensembl', 'mmusculus_gene_ensembl'
#' @param force  Re-generate table if it already exists
#' @return       A data.frame with the following columns:
#'     external_gene_name, affy, illumina, genbank, entrezgene, ensembl_gene_id
probeset_table = function(dset="hsapiens_gene_ensembl", version="latest", force=FALSE) {
    printv = function(dset) message(sprintf("Using Ensembl %s (%s)",
        attr(dset, "ensembl_version"), attr(dset, "dataset_version")))

    if (version == "latest")
        version = 103 #TODO: get this + be robust offline

    fname = sprintf("probeset-%s-ens%s.rds", dset, version)
    cache = file.path(module_file(), "cache", fname)
    if (file.exists(cache) && !force) {
        mapping = readRDS(cache)
        printv(mapping)
        return(mapping)
    }

    mart = biomaRt::useMart(biomart="ensembl", dataset=dset)
    marts = biomaRt::listMarts(mart)
    vstring = marts$version[marts$biomart == "ENSEMBL_MART_ENSEMBL"]
    version = as.integer(sub(".* ([0-9]+)$", "\\1", vstring))
    datasets = biomaRt::listDatasets(mart, version)
    dataset_version = datasets$version[datasets$dataset == dset]

    # if biomart has newer ensembl update cache file name
    fname = sprintf("probeset-%s-ens%s.rds", dset, version)
    cache = file.path(module_file(), "cache", fname)
    message("Generating cache file ", sQuote(fname))

    probes = list(
        affy = grep("^affy_", biomaRt::listAttributes(mart)$name, value=TRUE),
        illumina = grep("^illumina_", biomaRt::listAttributes(mart)$name, value=TRUE),
        agilent = grep("agilent_", biomaRt::listAttributes(mart)$name, value=TRUE)
    )

    tablecols = c("external_gene_name", "entrezgene_id", "ensembl_gene_id")
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
        re$external_gene_name[re$external_gene_name == ""] = NA
        re$entrezgene[re$entrezgene == ""] = NA
        re$ensembl_gene_id[re$ensembl_gene_id == ""] = NA
        tibble::as_tibble(re)
    }
    mapping = sapply(probes, assemblePS, simplify=FALSE, USE.NAMES=TRUE)

    attr(mapping, "ensembl_version") = version
    attr(mapping, "dataset_version") = dataset_version

    dir.create(dirname(cache), showWarnings=FALSE)
    saveRDS(mapping, file=cache)
    printv(mapping)
    mapping
}

if (is.null(module_name())) {
    probeset_table("hsapiens_gene_ensembl")
    probeset_table("mmusculus_gene_ensembl")
}
