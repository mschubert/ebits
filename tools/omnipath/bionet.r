import_package("tidygraph", attach=TRUE)

#' Return BioNet Steiner Tree subnetwork
#'
#' @param g  tidygraph-compatible network
#' @param assocs  data.frame with fields: n_smp, p.value, adj.p
#' @param thresh  p-value/fdr cutoff
#' @return  tidygraph object
bionet = function(g, assocs, thresh=0.05, var=c("adj.p", "padj")) {
    if (is.null(assocs$name) || any(duplicated(assocs$name)))
        stop("[tools/omnipath]: bionet needs 'assocs' with unique 'name' field")
    if (length(var) > 1) {
        var = var[1]
        message("[tools/omnipath]: using ", sQuote(var), " for scoring")
    }

    g = g %>% activate(nodes) %>% left_join(assocs)
    scores = setNames(pull(g, !! rlang::sym(var)), pull(g, name))
    scores[is.na(scores)] = 1
    scores = pmax(-log10(scores) + log10(thresh), 0)
    as_tbl_graph(BioNet::runFastHeinz(g, scores)) %>%
        activate(edges) %>%
        filter(from != to)
}
