null.list = function(x.list){
    x.list[unlist(lapply(x.list, length) != 0)]
}   

zero = function(x) {
    x[apply(x, 1, function(r) all(r!=0)),]
}   

empty = function(x) {
    x[apply(x, 1, function(r) all(nchar(r)>0)),]
}

dups = function(x, ...) {
    if (is.vector(x))
        x[!gn$duplicated(x, ...)]
    else if (is.matrix(x) || is.data.frame(x))
        x[!gn$duplicated(x, ...),]
    else
        stop("can only work on vector/matrix so far")
}

na.col = function(x, ...) {
    t(na.omit(t(x), ...))
}

