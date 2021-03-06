# I/O helper functions on R binary files
.b = import('../base')
.l1k = import('./l1ktools_io')

#' Loads .gctx files
#'
#' Workaround until I implement this properly
load_gctx = .l1k$read.gctx
read_gct = .l1k$read.gct

#' Load function that returns the object(s)
#'
#' @param filename  Name or path of file
#' @param ...       Additional arguments to the loading function
#' @param drop      If one element, return it instead of list
#' @return          Contents of the file
load = function(filename, ..., drop=TRUE) {
    get_contents = function(fpath) {
        env = new.env()
        fid = strsplit(basename(fpath), "\\$")[[1]]
        fname = fid[1]
        subsets = fid[-1] 
 
        base::load(file.path(dirname(fpath), fname), env)
        contents = as.list(env)
        if (length(contents)==1 && drop)
            contents[[1]] 
        else
            contents
    }   
    if (length(filename) > 1) {
        nn = sub("\\.RData", "", basename(filename))
        setNames(lapply(filename, get_contents), nn)
    } else
        get_contents(filename)
}

save = function(..., file) {
    if (grepl("\\.gct(x)?$", file)) {
        obj = list(...)
        stopifnot(length(obj) == 1 && class(obj[[1]]) == 'matrix')
        obj = new("GCT", obj[[1]])
        if (grepl("x$", file))
            .l1k$write.gctx(obj, file)
        else
            .l1k$write.gct(obj, file)
    } else
        base::save(..., file=file, envir=parent.frame())
}

#' Function to load all files that match a regular expression
load_regex = function(regex, path=".", FUN=load, ...) {
    fnames = gtools::mixedsort(list.files(path=path, pattern=regex, ...))
    files = sapply(fnames, function(f) file.path(path,f))
    setNames(lapply(files, FUN), .b$grep(regex, fnames))
}
