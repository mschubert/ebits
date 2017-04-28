#' duplicated() function with extended functionality
#'
#' @param ...     Arguments to be passed to R's `duplicated()`
#' @param all     Return all instances of duplicated entries
#' @param random  Randomly select with entry is marked as duplicate
duplicated = function(x, ..., all=F, random=F) {
    if (all && random)
        stop("Can not return all and randomly selected duplicates")

    if (all)
        base::duplicated(x, ...) | base::duplicated(x, ..., fromLast=TRUE)
    else if (random) { # stat.ethz.ch/pipermail/r-help/2010-June/241244.html
        if ( is.vector(x) ) {
            permutation = sample(length(x))
            x.perm      = x[permutation]
            result.perm = duplicated(x.perm, ...)
            result      = result.perm[order(permutation)]
            return(result)
        }
        else if ( is.matrix(x) ) {
            permutation = sample(nrow(x))
            x.perm      = x[permutation,]
            result.perm = duplicated(x.perm, ...)
            result      = result.perm[order(permutation)]
            return(result)
        }
        else {
            stop(paste("duplicated.random() only supports vectors",
            "matrices for now."))
        }
    }
    else
        base::duplicated(x, ...)
}
