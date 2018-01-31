#' Function to split genome in regions with constant copy number
#'
#' @param gr       A GRanges object (eg. AneuFinder model$segments)
#' @param field    Categorical field in GRanges on which to split
#' @param binsize  Size of bins to split (instead of exact matches)
#' @return         A list of GRanges objects
split_granges = function(gr, field="state", binsize=NULL) {
    split_fun = function(x) {
        if (opt$exact)
            end(x) = start(x)-1+floor(width(x)/binsize)*binsize
        unlist(tile(x, width=binsize))
    }
    lapply(somies, as.factor(gr[[field]]))
}
