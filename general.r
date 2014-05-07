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

se = function(x, na.rm=T) {
    if (na.rm)
        sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
    else
        sqrt(var(x)/length(x))
}

hopDistance = function(x, y) {
    sum(cumsum(as.integer(x)) - cumsum(as.integer(y)))
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
loadContents = function(filename) { #FIXME: this does not work with nested lists
    lfc = function(fpath) {
        env = new.env()

        fdir = dirname(fpath)
        fid = strsplit(basename(fpath), "\\$")[[1]]
        fname = fid[1]
        subsets = fid[-1]

        re = env[[load(file.path(fdir,fname), env)[1]]]
        for (s in subsets)
            re = re[[s]]
        re
    }
    if (length(filename) > 1)
        lapply(filename, lfc)
    else
        lfc(filename)
}

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

dups.omit = function(x, all=T) {
    x[gn$duplicated(x, all=all)]
}

na.col.omit = function(x, ...) {
    t(na.omit(t(x), ...))
}

### convert any list to a factor and return a binary matrix()
factorToModelMatrix = function(myfactor, na.rm=T, na=c('NA','','unknown')) {
    stopifnot(class(myfactor) == 'factor')
    level = levels(myfactor)
    level[level %in% na] = 'NA'

    charVec = as.character(myfactor)
    if (na.rm) {
        level = level[level!='NA']
        charVec = charVec[charVec %in% level]
    }

    groups = matrix(FALSE, nrow=length(charVec), ncol=length(level),
                    dimnames=list(names(myfactor), level))
    for (ilevel in level)
        groups[charVec==ilevel, ilevel] = TRUE
    groups
}
                    
modelMatrixToFactor = function(X, along, ambiguous="ambiguous") {
    apply(X, along, function(rn) { rownames(X)[rn!=0] }) # TODO: handle ambiguous
# ..
}

modelMatrixToList = function(X, indexOnly=F) {
    levelnames = colnames(X)
    samplenames = rownames(X)
    if (indexOnly || is.null(levelnames))
        levelnames = 1:ncol(X)
    if (indexOnly || is.null(samplenames))
        samplenames = 1:nrow(X)

    mylist = list()
    for (n in levelnames) {
        mylist[[n]] = samplenames[X[,n]==TRUE]
    }
    return(mylist)
}

