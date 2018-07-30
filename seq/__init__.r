export_submodule('./revcomp')
export_submodule('./read_bam')
export_submodule('./read_bed')
export_submodule('./read_granges')
export_submodule('./genome')
export_submodule('./chr_lengths')
export_submodule('./genome_mask')
export_submodule('./subset_genome')
export_submodule('./intersect')
probeset_table = import('./probeset_table')$probeset_table
gene_table = import('./gene_table')$gene_table
coords = import('./coords')
aneuploidy = import('./aneuploidy')$aneuploidy
count_pattern = import('./count_pattern')$count_pattern

names = GenomeInfoDb::seqnames
lengths = GenomeInfoDb::seqlengths
levels = GenomeInfoDb::seqlevels
info = GenomeInfoDb::seqinfo
`info<-` = function(x, value) {
    idx = unique(as.character(GenomeInfoDb::seqnames(x)))
    x@seqinfo = value[idx]
    if (inherits(x, "GRanges"))
        x@seqnames = droplevels(x@seqnames)
    x
}

set_info = function(x, name, info, drop=FALSE, envir=parent.frame()) {
    # check that all levels in x are present in info, error otherwise

    # assign info and seqlevels in x
    idx = unique(as.character(GenomeInfoDb::seqnames(x)))
    x@seqinfo = info(x)[idx]

    # GenomeInfoDb has character vector for seqnames, GRanges has Factor-Rle
    if (inherits(x, "GRanges"))
        x@seqnames = droplevels(x@seqnames)

    # return new seqinfo obj
    assign(name, x, envir=envir)
}
