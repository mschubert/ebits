#import('./operators')

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

fuzzy.match = function(x, from, to) {
    require(stringr)

    # 1st iteration: exact matches
    index1 = match(x, from)

    # 2nd iteration: non-punctuation exact matches
    FROM = str_replace_all(toupper(from), "[[:punct:]]", "")
    x = str_replace_all(x, "[[:punct:]]", "")
    index2 = match(x, FROM)

    # 3rd iteration: closest string matches w/o punctuation
    distances = adist(FROM, x)
    mind = apply(distances, 2, min)
    nmin = sapply(1:length(mind), function(i) sum(mind[i]==distances[,i]))
    mind[nmin>1] = NA # no non-unique matches
    index3 = sapply(1:length(mind), function(i) which(distances[,i]==mind[i]) %or% NA)

    # return best match
    to[index1 %or% index2 %or% index3]
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

