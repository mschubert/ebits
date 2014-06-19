###
### general utility functions without specific use
###
source('operators.r')

na.filter = function(X, rowmax=1, colmax=1) {
    keepRow = rowSums(is.na(X))/dim(X)[2] <= rowmax
    keepCol = colSums(is.na(X))/dim(X)[1] <= colmax
    X[keepRow, keepCol]
}

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

intersect = function(...) {
    intsct = list(...)
    if (length(intsct)==1 && is.list(intsct[[1]]))
        intsct = intsct[[1]]

    result = intsct[[1]]

    if (length(intsct) > 1) {
        for (i in 2:length(intsct))
            result = base::intersect(result, intsct[[i]])
    }
    result
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

### loads .ro, returns its content instead of littering the global namespace
loadContents = function(filename) {
    lfc = function(fpath) {
        env = new.env()
        fdir = dirname(fpath)
        fid = strsplit(basename(fpath), "\\$")[[1]]
        fname = fid[1]
        subsets = fid[-1]

        load(file.path(fdir,fname), env)
        contents = as.list(env)
        if (length(contents)==1)
            contents[[1]]
        else
            contents
    }
    if (length(filename) > 1)
        lapply(filename, lfc)
    else
        lfc(filename)
}

#TODO: error if file ends with .ro/.RData
data = function(id, name=names(id) %or% id, data.dir=options('data.dir')) {
    # look for .RData, .ro in all data.dirs
    id2fname = function(id) {
        fdir = dirname(id)
        fname = basename(id)
        list.files(path=file.path(data.dir, fdir),
                   full.names=T,
                   pattern=paste0(fname, "(.ro|.RData)"))[1]
    }
    loadContents(sapply(id, id2fname))
}

read.vector = function(..., stringsAsFactors=F) {
    tab = read.table(..., stringsAsFactors=stringsAsFactors)
    if (dim(tab)[2] == 1) {
        re = tab$V1
    } else if (dim(tab)[2] == 2) {
        re = tab$V2
        names(re) = tab$V1
    }
    else {
        stop("read.vector needs either 1 or 2 columns")
    }
    re
}

num.unique = function(x) {
    x = as.factor(x)
    sapply(x, function(y) sum(y==x))
}

### loads all objects matched by a regex into a list
loadFilesByRegex = function(path, regex, names=NULL, FUN=loadContents) {
    library(gtools) # mixedsort
    files = mixedsort(list.files(path=path, pattern=regex))
    files = sapply(files, function(f) { file.path(path,f) })

    if (!is.null(names))
        stopifnot(length(files) == length(names))

    # load files and stack to array
    objList = lapply(files, FUN)
    names(objList) = names
    return(objList)
}

delete.NULLs = function(x.list){
    x.list[unlist(lapply(x.list, length) != 0)]
}

zero.omit = function(x) {
    x[apply(x, 1, function(r) all(r!=0)),]
}

empty.omit = function(x) {
    x[apply(x, 1, function(r) all(nchar(r)>0)),]
}

dups.omit = function(x, ...) {
    if (is.vector(x))
        x[!gn$duplicated(x, ...)]
    else if (is.matrix(x) || is.data.frame(x))
        x[!gn$duplicated(x, ...),]
    else
        stop("can only work on vector/matrix so far")
}

na.col.omit = function(x, ...) {
    t(na.omit(t(x), ...))
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

# subset a data.frame with a vector matching a specfic column
reIndexDf = function(df, col, idx) {
    df[unlist(sapply(idx, function(x) which(x==df[col]))),]
}

read.table = function(file, sep="\t", stringsAsFactors=F, ...) {
    index = read.table(file, sep=sep, stringsAsFactors=stringsAsFactors, ...)
    colnames(index) = index[1,]
    rownames(index) = index[,1]
    index[-1,-1]
}

read.csv = function(file, sep=",", stringsAsFactors=F) {
    read.table(file, sep=sep, stringsAsFactors=stringsAsFactors)
}

