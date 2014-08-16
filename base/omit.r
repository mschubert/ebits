ov = import('./override')

null.list = function(x.list){
    x.list[unlist(lapply(x.list, length) != 0)]
}   

zero = function(x) {
    x[apply(x, 1, function(r) all(r!=0)),]
}   

empty = function(x) {
    if (is.character(x) && is.matrix(x))
        x[apply(x, 1, function(r) all(nchar(r)>0)),]
    else if (is.list(x))
        x[sapply(x, length) != 0]
    else
        stop("need character matrix or list")
}

dups = function(x, ...) {
    if (is.vector(x))
        x[!ov$duplicated(x, ...)]
    else if (is.matrix(x) || is.data.frame(x))
        x[!ov$duplicated(x, ...),]
    else
        stop("can only work on vector/matrix so far")
}

na = function(x) {
    if (is.list(x))
        x[!is.na(x)]
    else
        na.omit(x)
}

na.col = function(x, ...) {
    t(na.omit(t(x), ...))
}

