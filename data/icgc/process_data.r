# scripts to construct .RData/hdf5 objects from raw data
library(dplyr)
b = import('base')
ar = import('array')
io = import('io')
idmap = import('idmap')

.icgc_raw_dir = getOption('icgc_raw_dir') %or% stop("need option 'icgc_raw_dir'")
.icgc_data_dir = getOption('icgc_data_dir') %or% stop("need option 'icgc_data_dir'")

mat = function(fname, regex, formula, map.hgnc=FALSE, force=FALSE, fun.aggregate=sum) {
    if (!force && file.exists(file.path(.icgc_data_dir, fname)))
        return()

    efiles = list.files(.icgc_raw_dir, regex, recursive=T, full.names=T)

    file2matrix = function(fname) {
        message(fname)
        mat = ar$construct(read.table(fname, header=T, sep="\t"),
                           formula = formula,
                           fun.aggregate=fun.aggregate)
        mat = mat[!rownames(mat) %in% c('?',''),] # aggr fun might handle his?
        if (map.hgnc)
            mat = idmap$gene(mat, to='hgnc_symbol', fun.aggregate=fun.aggregate)
        mat
    }
    obj = lapply(efiles, file2matrix)

    if (length(obj) == 0)
        stop("all file reads failed, something is wrong")

    ids = unlist(sapply(obj, function(x) colnames(x)))
    dups = duplicated(ids)
    if (any(dups))
        warning(paste("Duplicate entries will be overstacked:", ids[dups]))

    mat = t(ar$stack(obj, along=2)) # this can be done better
    if (grepl("\\.h5$", fname))
        h5store::h5save(mat, file=file.path(.icgc_data_dir, fname))
    else
        save(mat, file=file.path(.icgc_data_dir, fname))
}

df = function(fname, regex, transform=function(x) x, force=FALSE) {
    if (!force && file.exists(file.path(.icgc_data_dir, fname)))
        return()

    files = list.files(.icgc_raw_dir, regex, recursive=T, full.names=T)
    mat = do.call(rbind, lapply(files, function(f) cbind(
        study = b$grep("/([^/]+)/[^/]+$", f),
        read.table(f, header=T, sep="\t") #FIXME: io$read should work here
    ))) %>% transform

    if (grepl("\\.h5$", fname))
        h5store::h5save(mat, file=file.path(.icgc_data_dir, fname))
    else
        save(mat, file=file.path(.icgc_data_dir, fname))
}
