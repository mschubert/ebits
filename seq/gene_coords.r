io = import('io')

#' Get gene coordinates as GRanges object
#'
#' @param idtype      biomart ID type ('hgnc_symbol', 'entrezgene', etc.)
#' @param assembly    Genome assembly
#' @param granges     Return a GRanges object instead of a data.frame
#' @param chr_prefix  Chromosome prefix (eg. 'chr'; default: "")
#' @param chromosomes Subset genes to specific chromosomes; in+excludes prefix
#' @return            A data.frame or GRanges object with gene coordinates
gene_coords = function(idtype="hgnc_symbol", assembly="fixthis", granges=FALSE,
                       chr_prefix="", chromosomes=NULL) {
    #TODO:
    # keep track of assembly
    # add seqlengths to GRanges object

    fname = file.path(module_file(), paste0("coords-", idtype, ".RData"))
    if (!file.exists(fname)) {
        mart = biomaRt::useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")

        attr = c(idtype, "chromosome_name", "start_position", "end_position",
                 "strand", "gene_biotype")
        coords = biomaRt::getBM(attributes = attr,
                                filters = c("chromosome_name"),
                                values = c(1:22, 'X', 'Y', 'MT'),
                                mart = mart)

        coords = dplyr::tbl_df(coords[coords[[idtype]] != "",])

        io$save(coords, file=fname)
    } else
        coords = io$load(fname)

    if (!is.null(chromosomes)) {
        chromosomes = c(chromosomes, paste0(chr_prefix, chromosomes))
        coords = dplyr::filter(coords, chromosome_name %in% chromosomes)
    }

    strand_lookup = setNames(c("+", "-"), c(1, -1))
    coords = dplyr::mutate(coords,
        chromosome_name = paste0(chr_prefix, chromosome_name),
        strand = strand_lookup[as.character(strand)])

    if (granges) {
        coords = GenomicRanges::makeGRangesFromDataFrame(coords,
            start.field = "start_position",
            end.field = "end_position",
            keep.extra.columns = TRUE)
    }

    coords
}

