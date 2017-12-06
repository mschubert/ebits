#' Get genome from assembly ID
#'
#' @param assembly_id  An assembly identifier, e.g. 'hg19' or 'GRCh38'
#' @param masked       Use masked version of the genome
#' @return             Object from BSgenome
genome = function(assembly_id, masked=FALSE) {
    if (masked)
        assembly_id = paste(assembly_id, "masked", sep=".")

    if (assembly_id == "GRCh37")
        assembly_id = "hg19"

    gname = grep(assembly_id, BSgenome::available.genomes(), value=TRUE)[1]
    getFromNamespace(gname, ns=gname)
}
