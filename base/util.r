import('./operators')

grep = function(pattern, x, ...) {
    # http://stackoverflow.com/questions/2969315
    if (grepl("[^\\]\\(", pattern) || grepl("^\\(", pattern))
        re = function(pattern, x, ...) stringr::str_match(x, pattern)[,-1]
    else
        re = function(pattern, x, ...) base::grep(pattern, x, value=T, ...)

    if (length(pattern) == 1)
        re(pattern, x, ...)
    else
        sapply(pattern, function(p) re(p, x, ...))
}

descriptive_index = function(x, along=NULL) {
    if (!is.null(names(x)))
        names(x)
    else if ((is.character(x) || is.numeric(x)) &&
             (is.vector(x) || length(dim(x))==1))
        x
    else if (!is.null(along) && (is.matrix(x) || is.data.frame(x))) {
        dn = dimnames(x)[[along]]
        if (is.null(dn))
            1:dim(x)[along]
        else
            dn
    } else if (is.list(x)) # list and data.frame
        seq_along(x)
    else
        stop("Not sure how to get indices on that object")
}

num_unique = function(x) {
    x = as.factor(x)
    sapply(x, function(y) sum(y==x))
}

na_filter = function(X, rowmax=1, colmax=1) {
    keepRow = rowSums(is.na(X))/dim(X)[2] <= rowmax
    keepCol = colSums(is.na(X))/dim(X)[1] <= colmax
    X[keepRow, keepCol]
}

### n-th max value
maxN = function(x, N=2){
    len = length(x)
    if(N>len){
        warning('N greater than length(x).  Setting N=length(x)')
        N = length(x)
    }
    sort(x, decreasing=T)[N]
}

minN = function(x, N=2) {
    -maxN(-x, N) 
}
