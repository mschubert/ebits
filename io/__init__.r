library(modules)
import('base/operators')

read.vector = function(..., stringsAsFactors=F) {
    tab = read.table(..., stringsAsFactors=stringsAsFactors)
    if (dim(tab)[2] == 1)
        tab$V1
    else if (dim(tab)[2] == 2)
        setNames(tab$V2, tab$V1)   
    else
        stop("read.vector needs either 1 or 2 columns")
}

read.table = function(file, sep="\t", stringsAsFactors=F, ...) {
    index = utils::read.table(file, sep=sep, stringsAsFactors=stringsAsFactors, ...)
    colnames(index) = index[1,]
    rownames(index) = index[,1]
    index[-1,-1]
}   
    
read.csv = function(file, sep=",", stringsAsFactors=F) {
    read.table(file, sep=sep, stringsAsFactors=stringsAsFactors)
}

load = function(filename) {
    lfc = function(fpath) {
        env = new.env()
        fdir = dirname(fpath)
        fid = strsplit(basename(fpath), "\\$")[[1]]
        fname = fid[1]
        subsets = fid[-1] 
 
        base::load(file.path(fdir,fname), env)
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
loadContents = function(...) {
    warning("deprecated, use io$load()")
    load(...)
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

loadFilesByRegex = function(path, regex, names=NULL, FUN=loadContents) {
    library(gtools) # mixedsort
    files = mixedsort(list.files(path=path, pattern=regex))
    files = sapply(files, function(f) file.path(path,f))

    if (!is.null(names))
        stopifnot(length(files) == length(names))
    
    # load files and stack to array
    objList = lapply(files, FUN)
    names(objList) = names
    return(objList)
}   

