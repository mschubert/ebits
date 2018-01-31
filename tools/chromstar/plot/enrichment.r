seq = import('seq')

#' Plots mark enrichment around a set of coordinates
#'
#' @param model  Chromstar model object
#' @param dset   Ensembl data set to use for gene annotations
#' @param ref    Center around either gene body or TSS
#' @param flank  Number  of bp in flanking region
#' @param pp     Filter peaks by 1-posterior probability (default: no filter)
#' @param ...    Additional parameters passed to chromstaR::plotEnrichment
#' @return       A ggplot2 object
enrichment = function(model, dset, ref=c("gene", "TSS"), flank=1.5e4, pp=NULL, ...) {
    ref_lookup = setNames(c(c("start", "inside", "end"), "start"),
                          c("gene", "TSS"))
    ref = match.arg(ref)
    coords = seq$coords$gene('ensembl_gene_id', dset=dset, granges=TRUE)

    if (!is.null(pp))
        model = filter_peaks(model, pp=pp)

    add = function(p) p +
        xlab("distance from gene body") +
        theme_minimal() +
        guides(color=FALSE) +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        facet_wrap(~ combination)

    p = chromstaR::plotEnrichment(model, coords, region=ref_lookup[ref],
            bp.around.annotation=flank, ...)
    lapply(p, add)
}
