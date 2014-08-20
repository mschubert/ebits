# grep only matching group '(...)'
grepo = function(pattern, x, ...) {
    # http://stackoverflow.com/questions/2969315
    require(stringr)
    if (!grepl("\\(", pattern))
        re = function(pattern, x, ...) str_match(x, paste0("(", pattern, ")"))[, 2]
    else
        re = function(pattern, x, ...) str_match(x, pattern)[, 2]

    if (length(pattern) == 1)
        re(pattern, x, ...)
    else
        sapply(pattern, function(p) re(p, x, ...))
}

# subset a data.frame with a data.frame
# compare everything as characters
dfdf = function(df, subs, exact=T) {
    oldDf = df

    if (identical(colnames(df), NULL))
        colnames(df) = c(1:ncol(df))
    if (identical(colnames(subs), NULL))
        colnames(subs) = c(1:ncol(subs))

    for (cn in colnames(subs)) {
        subs[[cn]] = as.character(subs[[cn]])
        df[[cn]] = as.character(df[[cn]])
    }

    idx = c()
    for (i in 1:nrow(subs)) {
        mask = rep(TRUE, nrow(df))
        for (name in colnames(subs))
            mask = mask & (subs[[i,name]] == df[[name]])
        if (exact && sum(mask) != 1)
            stop("exact=T needs exactly one match per row in subsetting df")
        idx = c(idx, which(mask))
    }

    oldDf[idx,]
}

num.unique = function(x) {
    x = as.factor(x)
    sapply(x, function(y) sum(y==x))
}

na.filter = function(X, rowmax=1, colmax=1) {
    keepRow = rowSums(is.na(X))/dim(X)[2] <= rowmax
    keepCol = colSums(is.na(X))/dim(X)[1] <= colmax
    X[keepRow, keepCol]
}

### n-th max value
maxN <- function(x, N=2){
    len <- length(x)
    if(N>len){
        warning('N greater than length(x).  Setting N=length(x)')
        N <- length(x)
    }
    sort(x, decreasing=T)[N]
}

minN = function(x, N=2) {
    -maxN(-x, N) 
}   

