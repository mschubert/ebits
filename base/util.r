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


# subset a data.frame with a data.frame
# compare everything as characters
dfdf = function(df, subs, exact=F, add.cols=F) {
    oldDf = df

    if (add.cols) {
        subsFull = subs
        subs = subsFull[intersect(colnames(subsFull), colnames(df))]
        subsAdd = subsFull[setdiff(colnames(subsFull), colnames(df))]
        idxAdd = c()
    }

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

        if (add.cols)
            idxAdd = c(idxAdd, rep(i, sum(mask)))
    }

    if (add.cols)
        cbind(oldDf[idx,,drop=F], subsAdd[idxAdd,,drop=F])
    else
        oldDf[idx,,drop=F]
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
