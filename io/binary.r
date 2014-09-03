library(modules)
b = import('base')

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

load.regex = function(regex, path=".", FUN=load) {
    library(gtools) # mixedsort
    files = mixedsort(list.files(path=path, pattern=regex))
    files = sapply(files, function(f) file.path(path,f))
    setNames(lapply(files, FUN), b$grepo(regex, files))
}

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
