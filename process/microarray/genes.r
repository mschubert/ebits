.idmap = import('../idmap')

#' Function to extract a gene-array value matrix from a processed expression object
#'
#' @param normData  The normalized data object
#' @param to        Identifiers to map to: hgnc_symbol, ...
#' @return          A matrix with gene identifiers
map_genes = function(normData, to="hgnc_symbol", fun=function(x) mean(x, na.rm=TRUE)) {
    .idmap$gene(exprs(normData), 'probe_id', to, fun.aggregate=fun)
}
