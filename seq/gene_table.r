#' Creates a table of different identifiers and caches it
#'
#' @param dset  Ensembl data set, e.g. '{hsapiens,mmusculus}_gene_ensembl'
#' @param assembly  Genome assembly version (default: GRCh38/GRCm39)
#' @param version   Ensembl version (integer); NULL: latest for assembly
#' @param force  Re-generate table if it already exists
#' @return       A data.frame with gene and transcript-level information
gene_table = function(dset="hsapiens_gene_ensembl", assembly=NULL, version=NULL, force=FALSE) {
    printv = function(dset) message(sprintf("Using Ensembl %s (%s)",
        attr(dset, "ensembl_version"), attr(dset, "dataset_version")))

    if (is.null(assembly))
        assembly = switch(dset, hsapiens_gene_ensembl="GRCh38", mmusculus_gene_ensembl="GRCm39")
    if (is.null(version))
        version = switch(assembly, GRCm39="latest", GRCm38="102", GRCh38="latest", GRCh37="GRCh37")

    # https://www.ensembl.org/info/website/archives/index.html
    if (version == "latest") {
        version = "104"
        host = "https://www.ensembl.org"
    } else if (tolower(version) == "grch37") {
        version = "104"
        host = "http://grch37.ensembl.org"
    } else if (version == "102") {
        host = "https://nov2020.archive.ensembl.org"
    } else
        stop("add archive link here if required")

    fname = sprintf("gene_table-%s-ens%s-%s.rds", dset, version, assembly)
    cache = file.path(module_file(), "cache", fname)
    if (file.exists(cache) && !force) {
        mapping = readRDS(cache)
        printv(mapping)
        return(mapping)
    }

    ensembl = biomaRt::useMart("ENSEMBL_MART_ENSEMBL", dataset=dset, host=host)
    marts = biomaRt::listMarts(ensembl)
    vstring = marts$version[marts$biomart == "ENSEMBL_MART_ENSEMBL"]
    version = as.integer(sub(".* ([0-9]+)$", "\\1", vstring))
    datasets = biomaRt::listDatasets(ensembl, version)
    dataset_version = datasets$version[datasets$dataset == dset]

    # if biomart has newer ensembl update cache file name
    fname = sprintf("gene_table-%s-ens%s-%s.rds", dset, version, assembly)
    cache = file.path(module_file(), "cache", fname)
    message("Generating cache file ", sQuote(fname))

    ids = c('external_gene_name', 'entrezgene_id', 'ensembl_gene_id',
            'band', 'chromosome_name', 'start_position', 'end_position',
            'ensembl_transcript_id', 'transcript_start', 'transcript_end',
            'transcription_start_site', 'strand', 'gene_biotype')
    mapping = tibble::as_tibble(biomaRt::getBM(attributes=ids, mart=ensembl))
    for (col in colnames(mapping)) {
        is_empty = nchar(as.character(mapping[,col])) == 0
        mapping[[col]][is_empty] = NA
    }

    attr(mapping, "ensembl_version") = version
    attr(mapping, "dataset_version") = dataset_version

    dir.create(dirname(cache), showWarnings=FALSE)
    saveRDS(mapping, file=cache)
    printv(mapping)
    mapping
}

if (is.null(module_name())) {
    gene_table("hsapiens_gene_ensembl")
    gene_table("hsapiens_gene_ensembl", assembly="GRCh37")
    gene_table("mmusculus_gene_ensembl")
    gene_table("mmusculus_gene_ensembl", assembly="GRCm38")
}
