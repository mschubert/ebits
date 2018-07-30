.subs = import('./subset_genome')$subset_genome

#' Get genome from assembly ID
#'
#' @param assembly_id  An assembly identifier, e.g. 'hg19' or 'GRCh38'
#' @param masked       Use masked version of the genome
#' @param chrs         Only return the following chromosomes (default: all)
#' @return             Object from BSgenome
genome = function(assembly_id, masked=FALSE, chrs=NULL) {
    genome_lookup = list(
        GRCh37 = "hg19",
        GRCm38 = "mm10"
    )

    ncbi_id = NULL
    if (assembly_id %in% names(genome_lookup)) {
        ncbi_id = assembly_id
        assembly_id = genome_lookup[ncbi_id]
    }

    if (masked)
        assembly_id = paste(assembly_id, "masked", sep=".")

    gname = grep(assembly_id, BSgenome::available.genomes(), value=TRUE)[1]
    if (length(gname) == 0)
        stop("BSgenome not found for: ", assembly_id)

    g = getFromNamespace(gname, ns=gname)

    if (!is.null(ncbi_id)) {
        newlvl = GenomeInfoDb::mapSeqlevels(GenomeInfoDb::seqlevels(g), "NCBI")
        na_idx = which(is.na(newlvl))
        newlvl[na_idx] = names(newlvl)[na_idx]
        g = GenomeInfoDb::renameSeqlevels(g, newlvl)

        if (ncbi_id == "GRCh37") {
            if (is.null(chrs))
                chrs = setdiff(GenomeInfoDb::seqnames(g), "MT")
            else if ("MT" %in% chrs)
                stop("We're mapping from hg19 and MT is not the same")
        }
    }

    if (!is.null(chrs))
        g = .subs(g, as.character(chrs))

    g
}
