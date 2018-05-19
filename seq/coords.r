import_package('GenomicRanges', attach=TRUE)
`%>%` = magrittr::`%>%`
.gene_table = import('./gene_table')$gene_table
.genome = import('./genome')$genome

#TODO:
# keep track of assembly
# add seqlengths to GRanges object

#' @rdname gene
.process = function(coords, chromosomes, type=NULL, granges=FALSE,
                    start_field="start_position", end_field="end_position") {
    coords %>%
        dplyr::distinct() %>%
        dplyr::arrange(chromosome_name, start_position)

    if (!is.null(type))
        coords = dplyr::filter(coords, gene_biotype %in% type)

    if (is.null(chromosomes))
        chromosomes = c(1:100, 'X', 'Y', 'MT')
    coords = dplyr::filter(coords, chromosome_name %in% chromosomes)

    coords = mutate(coords, canonical_tss =
        ifelse(strand == 1, start_position, end_position))

    if (granges) {
        strand_lookup = setNames(c("+", "-"), c(1, -1))
        coords = coords %>%
            dplyr::mutate(strand = strand_lookup[as.character(strand)]) %>%
            GenomicRanges::makeGRangesFromDataFrame(
                start.field = start_field,
                end.field = end_field,
                keep.extra.columns = TRUE)
    }

    unique(coords)
}

#' Get gene or trascript coordinates as GRanges object
#'
#' @param idtype      biomart ID type ('hgnc_symbol', 'entrezgene', etc.);
#'  'external_gene_name' refers to 'hgnc_symbol' in human and 'mgi_symbol' in mouse
#' @param dset        Ensembl data set, e.g. '{hsapiens,mmusculus}_gene_ensembl'
#' @param assembly    Genome assembly (still TODO)
#' @param granges     Return a GRanges object instead of a data.frame
#' @param chromosomes Subset genes to specific chromosomes; in+excludes prefix
#' @param type        Only return genes of a certain type, e.g. 'protein_coding'
#' @return            A data.frame or GRanges object with gene coordinates
gene = function(idtype="external_gene_name", dset="hsapiens_gene_ensembl",
                assembly="fixthis", granges=FALSE, chromosomes=NULL, type=NULL) {
    idtype[idtype %in% c("hgnc_symbol", "mgi_symbol")] = "external_gene_name"

    keep = c(idtype, "band", "chromosome_name", "start_position",
             "end_position", "strand", "gene_biotype")
    .process(.gene_table(dset=dset)[,keep], chromosomes=chromosomes,
             type=type, granges=granges)
}

#' @rdname gene
transcript = function(idtype="external_gene_name", dset="hsapiens_gene_ensembl",
                      assembly="fixthis", granges=FALSE, chromosomes=NULL, type=NULL) {
    idtype[idtype %in% c("hgnc_symbol", "mgi_symbol")] = "external_gene_name"

    .process(.gene_table(dset=dset), chromosomes=chromosomes,
             type=type, granges=granges)
}

#' @rdname gene
probeset = function(idtype="external_gene_name", dset="hsapiens_gene_ensembl",
                    assembly="fixthis", granges=FALSE, chromosomes=NULL) {
    stop("not implemented")
}

chrs = function(assembly="GRCh38", granges=FALSE, chr_excl=c("Y","MT")) {
    ldf = as.data.frame(GenomeInfoDb::keepStandardChromosomes(
        GenomeInfoDb::seqinfo(.genome(assembly))))
    ldf$chrs = ldf$seqnames = rownames(ldf)
    ldf$start = 1
    ldf$end = ldf$seqlengths
    ldf$isCircular = NULL
    ldf$seqlengths = NULL
    rownames(ldf) = NULL
    ldf = ldf[!ldf$seqnames %in% chr_excl,]
    if (granges)
        ldf = GenomicRanges::makeGRangesFromDataFrame(ldf, keep.extra.columns=TRUE)
    ldf
}

#' hg19 arms from cytoband
chr_arms = function(assembly="GRCh38", granges=FALSE) {
    if (assembly != "hg19")
        warning("bands are hg19")
    env = new.env()
    data(hg19IdeogramCyto, package = "biovizBase", envir=env)
    bdf = as.data.frame(env$hg19IdeogramCyto) %>%
        dplyr::mutate(seqnames = sub("^chr", "", seqnames),
                      arm = paste(seqnames, substr(name, 0, 1), sep=".")) %>%
        dplyr::group_by(seqnames, strand, arm) %>%
        dplyr::summarize(start=min(start), end=max(end)) %>%
        dplyr::ungroup()
    if (granges)
        GenomicRanges::makeGRangesFromDataFrame(bdf, keep.extra.columns=TRUE)
    else
        bdf
}

#' hg19 cytobands
chr_bands = function(assembly="GRCh38", granges=FALSE) {
    if (assembly != "hg19")
        warning("bands are hg19")
    env = new.env()
    data(hg19IdeogramCyto, package = "biovizBase", envir=env)
    bdf = as.data.frame(env$hg19IdeogramCyto) %>%
        dplyr::mutate(seqnames = sub("^chr", "", seqnames)) %>%
        dplyr::rename(band=name)
    if (granges)
        GenomicRanges::makeGRangesFromDataFrame(bdf, keep.extra.columns=TRUE)
    else
        bdf
}
