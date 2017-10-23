#' Removes all try-error from lists
#'
#' @param x     A list  
#' @param drop  Whether to drop unused dimensions after removing NAs
#' @param omit  Whether or not to perform action
error = function(x, drop=FALSE, omit=TRUE) {
    if (!omit)
        x
    else
        x[! sapply(x, function(x) "try-error" %in% class(x)), drop=drop]
}

#' Removes NULL (length 0) objects from lists
#'
#' @param x     A list  
#' @param drop  Whether to drop unused dimensions after removing NAs
#' @param omit  Whether or not to perform action
null = function(x, drop=FALSE, omit=TRUE) {
    if (!omit)
        x
    else
        x[sapply(x, length) != 0, drop=drop]
}

#' Removes `0` elements from an object
#'
#' @param x     A vector, matrix, or data.frame
#' @param drop  Whether to drop unused dimensions after removing NAs
#' @param omit  Whether or not to perform action
zero = function(x, drop=FALSE, omit=TRUE) {
    if (!omit)
        x
    else if (is.vector(x))
        x[x!=0, drop=drop]
    else if (is.matrix(x) || is.data.frame(x))
        x[apply(x, 1, function(r) all(r!=0)),, drop=drop]
    else
        stop("Need vector, matrix, or data.frame")
}   

#' Remove empty entries in character objects
#'
#' @param x     A vector, matrix, or data.frame
#' @param drop  Whether to drop unused dimensions after removing NAs
#' @param omit  Whether or not to perform action
empty = function(x, drop=FALSE, omit=TRUE) {
    if (!omit)
        x
    else if (is.matrix(x) || is.data.frame(x))
        x[apply(x, 1, function(r) all(nchar(r)>0)),,drop=drop]
    else if (is.character(x) || is.character(x[[1]]))
        x[sapply(x, nchar) != 0, drop=drop]
    else if (is.list(x))
        x[sapply(x, length) != 0, drop=drop]
    else
        stop("need vector, matrix or list")
}

#' Function to remove duplicates from a vector, matrix, or data.frame
#'
#' @param x     Object to drop duplicates from
#' @param ...   Arguments to pass to b$duplicated
#' @param drop  Whether to drop unused dimensions after removing NAs
#' @param omit  Whether or not to perform action
dups = function(x, ..., drop=FALSE, omit=TRUE) {
    dup = import('./duplicated')$duplicated

    if (!omit)
        x
    else if (is.vector(x))
        x[!dup(x, ...), drop=drop]
    else if (is.matrix(x) || is.data.frame(x))
        x[!dup(x, ...),,drop=drop]
    else
        stop("can only work on vector/matrix so far")
}

#' Function to remove NAs from a vector, matrix, or data.frame
#'
#' @param x     Object to remove NAs from
#' @param omit  Boolean flag indicating whether to omit or not (useful as a calling argument)
#' @param cols  Column names or indices to restrict NA counting to (default: all)
#' @param drop  Whether to drop unused dimensions after removing NAs
na = function(x, omit=TRUE, cols=TRUE, drop=FALSE) {
    if (!omit)
        x
    else if (is.vector(x))
        x[!is.na(x)]
    else
        x[apply(x[,cols,drop=FALSE], 1, function(row) !any(is.na(row))),,drop=drop]
}

#' Like na.omit, but for columns
#'
#' @param x    A matrix
#' @param ...  Arguments passed to na.omit function
na_col = function(x, ...) {
    t(na.omit(t(x), ...))
}
