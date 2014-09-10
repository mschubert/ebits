# I/O helper functions on R binary files

b = import('base')

#' Load function that returns the object(s) instead of attaching it to the global namespace
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

#' Function to load all files that match a regular expression
load.regex = function(regex, path=".", FUN=load) {
    library(gtools) # mixedsort
    files = mixedsort(list.files(path=path, pattern=regex))
    files = sapply(files, function(f) file.path(path,f))
    setNames(lapply(files, FUN), b$grepo(regex, files))
}

#' Function to load R files specified in \code{options('data.dir')}
data = function(id, name=names(id) %or% id, data.dir=options('data.dir') %or% ".") {
    # look for .RData, .ro in all data.dirs
    id2fname = function(id) {
        fdir = dirname(id)
        fname = basename(id)
        list.files(path=file.path(data.dir, fdir),
                   full.names=T,
                   pattern=paste0(fname, "(.ro|.RData)"))[1]
    }
    load(sapply(id, id2fname))
}
