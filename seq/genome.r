.subs = import('./subset_genome')$subset_genome

#' Get genome from assembly ID
#'
#' @param assembly_id  An assembly identifier, e.g. 'hg19' or 'GRCh38'
#' @param masked       Use masked version of the genome
#' @param chrs         Only return the following chromosomes (default: all)
#' @return             Object from BSgenome
genome = function(assembly_id, masked=FALSE, chrs=NULL) {
    if (assembly_id == "GRCh37") {
        to_ncbi_ids = TRUE
        assembly_id = "hg19"
    } else
        to_ncbi_ids = FALSE

    if (masked)
        assembly_id = paste(assembly_id, "masked", sep=".")

    gname = grep(assembly_id, BSgenome::available.genomes(), value=TRUE)[1]
    g = getFromNamespace(gname, ns=gname)

    if (to_ncbi_ids) {
        newlvl = GenomeInfoDb::mapSeqlevels(GenomeInfoDb::seqlevels(g), "NCBI")
        g = GenomeInfoDb::renameSeqlevels(g, na.omit(newlvl))
        if (is.null(chrs))
            chrs = setdiff(GenomeInfoDb::seqnames(g), "MT")
        else if ("MT" %in% chrs)
            stop("We're mapping from hg19 and MT is not the same. - Can't do.")
    }

#FIXME: this doesn't quite work
#    if (!is.null(chrs))
#        g = .subs(g, chrs)

    g
}
