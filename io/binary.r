# I/O helper functions on R binary files
.b = import('../base')

#' Load function that returns the object(s) instead of attaching it to the global namespace
#'
#' @param filename  Name or path of file
#' @return          Contents of the file
load = function(filename) {
    get_contents = function(fpath) {
        env = new.env()
        fid = strsplit(basename(fpath), "\\$")[[1]]
        fname = fid[1]
        subsets = fid[-1] 
 
        base::load(file.path(dirname(fpath), fname), env)
        contents = as.list(env)
        if (length(contents)==1)
            contents[[1]] 
        else
            contents
    }   
    if (length(filename) > 1)
        lapply(filename, get_contents)
    else
        get_contents(filename)
}

#' Function to load all files that match a regular expression
load_regex = function(regex, path=".", FUN=load, ...) {
    fnames = gtools::mixedsort(list.files(path=path, pattern=regex, ...))
    files = sapply(fnames, function(f) file.path(path,f))
    setNames(lapply(files, FUN), .b$grep(regex, fnames))
}
