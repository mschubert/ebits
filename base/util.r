import('./operators')

grep = function(pattern, x, ...) {
    # http://stackoverflow.com/questions/2969315
    if (grepl("[^\\]\\(", pattern) || grepl("^\\(", pattern))
        re = function(pattern, x, ...) stringr::str_match(x, pattern)[,-1]
    else
        re = function(pattern, x, ...) base::grep(pattern, x, value=TRUE, ...)

    if (length(pattern) == 1)
        re(pattern, x, ...)
    else
        sapply(pattern, function(p) re(p, x, ...))
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
    if (N > len){
        warning('N greater than length(x).  Setting N=length(x)')
        N = length(x)
    }
    sort(x, decreasing=TRUE)[N]
}

minN = function(x, N=2) {
    -maxN(-x, N) 
}

top_mask = function(x, N=2) {
    if (N > length(x))
        rep(TRUE, length(x))
    else
        seq_along(x) %in% order(x, decreasing=TRUE)[1:N]
}
