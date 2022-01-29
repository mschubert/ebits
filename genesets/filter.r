import('base/operators')

#' Takes a list of gene sets and returns the list filtered by valid IDs and number
#'
#' @param genesets  A list of vectors
#' @param valid     A vector of identifiers that can be used
#' @param min       The minimum number of genes in a list to keep the list
#' @param max       The maximum number of genes in a list to keep the list
filter = function(genesets, ...) {
    UseMethod("filter")
}

filter.list = function(genesets, valid=NULL, min=2, max=Inf, warn=TRUE) {
    if (is.list(genesets[[1]]))
        return(lapply(genesets, filter, valid=valid, min=min, max=max, warn=warn))

    if (!is.null(valid)) {
        if (any(is.na(valid)))
            warning("NA found in valid set")
        if (any(valid == ""))
            warning("empty identifier found in valid set")
        genesets = lapply(genesets, function(x) intersect(x, valid))
    } else
        genesets = lapply(genesets, function(x) setdiff(na.omit(x), ""))

    num_overlap = sapply(genesets, length)
    discard = num_overlap < min | num_overlap > max

    if (any(discard) && warn) {
        warning("Discarding ", sum(discard), " (of ", length(discard), ") sets: ",
                paste(names(genesets)[discard], collapse=", "))
        genesets = genesets[!discard]
    }

    if (length(genesets) == 0)
        stop("No gene sets left after filtering")

    genesets
}

filter.data.frame = function(genesets, valid=NULL, min=2, max=Inf, warn=TRUE,
                             set="id", gene="hgnc_symbol") {
    if (!is.null(valid)) {
        if (any(is.na(valid)))
            warning("NA found in valid set")
        if (any(valid == ""))
            warning("empty identifier found in valid set")
        genesets = genesets[genesets[[gene]] %in% valid,]
    }
    if (! set %in% colnames(genesets))
        stop("variable 'set' needs to reference the gene set column")
    if (! gene %in% colnames(genesets))
        stop("variable 'gene' needs to reference the gene column")

    set_s = rlang::sym(set)
    gene_s = rlang::sym(gene)
    all_sets = unique(genesets[[set]])

    # dplyr::n() does not work here (direct all implemented as error)
    re = genesets %>%
        dplyr::distinct() %>%
        dplyr::group_by(!! set_s) %>%
        dplyr::filter(dplyr::n_distinct(!! gene_s) >= min &
                      dplyr::n_distinct(!! gene_s) <= max) %>%
        dplyr::ungroup()

    discard = setdiff(all_sets, re[[set]])

    if (length(discard) > 0 && warn)
        warning("Discarding ", length(discard), " (of ", length(all_sets), ") sets: ",
                paste(discard, collapse=", "))
    if (nrow(re) == 0)
        stop("No gene sets left after filtering")

    re
}
