#' Returns indices along an object, either numeric or names if available
#'
#' @param x      An object to be indexed
#' @param along  The axis along which to index for array-like objects; default: last dimension
descriptive_index = function(x, along=NULL) {
    if (!is.null(names(x)))
        names(x)
    else if ((is.character(x) || is.numeric(x) || is.logical(x)) &&
             (is.vector(x) || length(dim(x))==1))
        x
    else if (is.factor(x))
        as.character(x)
    else if (!is.null(along) && (is.matrix(x) || is.data.frame(x))) {
        dn = dimnames(x)[[along]]
        if (is.null(dn))
            1:dim(x)[along]
        else
            dn
    } else if (is.list(x)) { # list and data.frame
        if (is.null(names(x)))
            seq_along(x)
        else
            names(x)
    } else
        stop("Not sure how to get indices on that object")
}

#' Subsets an object, either with numeric indices or names if available
#'
#' @param x      An object to get a subset from
#' @param index  The index to subset with
#' @param along  The axis along which to index for array-like objects; default: last dimension
subset = function(x, index, along=NULL, drop=FALSE) {
    if (is.vector(x) && !is.list(x))
        x[index, drop=drop]
    else if (is.list(x) && !is.data.frame(x)) { #TODO: drop_list@base? (used in array/construct as well)
        if (drop && length(index) == 1)
            x[[index]]
        else
            x[index]
    } else if (is.array(x) || is.data.frame(x))
        import('../array')$subset(x, index, along, drop)
    else
        stop("Not sure how to subset that object")
}
