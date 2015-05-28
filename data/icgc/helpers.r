library(dplyr)
.b = import('base')
.ar = import('array')
.io = import('io')
.p = import('../path')

icgc_data_dir = .p$path('icgc')
icgc_raw_dir = .p$file('icgc', 'version_17')

.clinical = NULL
.clinicalsample = NULL

#' Helper function to get ICGC RData contents
#'
#' @param var   The variable name to cache to, either `.clinical` or `.clinicalsample`
#' @param file  The file name, either 'clinical.RData' or 'clinicalsample.RData'
#' @return      A data.frame containing the file contents
getRData = function(var, file) {
    re = base::get(var, envir=parent.env(environment()))
    if (is.null(re)) {
        re = .io$load(.io$file_path(icgc_data_dir, file))
        assign(var, re, envir=parent.env(environment()))
    }
    re
}

#' Helper function to get ICGC HDF5 objects
#'
#' @param fname  The file name
#' @param ...    Arguments passed to varmap
#' @return       A matrix with mapped IDs
getHDF5 = function(fname, ...) {
    args = list(...)
    index = args$index
    args$index = NULL

    f = do.call(varmap, args)
    re = t(h5store::h5load(.io$file_path(icgc_data_dir,
        fname, ext=".h5"), index=names(f)))
    colnames(re) = unname(f)
    re
}

#' Function to map the given variables to an HDF5 index and name mappings
#'
#' @param valid     A vector of valid identifiers
#' @param samples   ICGC sample ids
#' @param specimen  ICGC specimen ids
#' @param donors    ICGC donor ids
#' @return          List with file index and mapped column names
varmap = function(valid, samples=NULL, specimen=NULL, donors=NULL, map.ids=TRUE) {
    nNull = is.null(samples) + is.null(specimen) + is.null(donors)
    if (nNull < 2)
        stop("can take ONE of index, samples, specimen, donors")

    if (!is.null(specimen)) {
        to = "icgc_specimen_id"
        filter_to = specimen
    } else if (!is.null(donors)) {
        to = "icgc_donor_id"
        filter_to = donors
    } else if (!is.null(samples) || is.logical(map.ids)) {
        to = "icgc_sample_id"
        filter_to = samples
    } else {
        to = map.ids
        filter_to = NULL
    }

    .b$match(x = valid,
             from = "icgc_sample_id",
             to = to,
             filter_to = filter_to,
             data = getRData('.clinicalsample', 'clinicalsample.RData'))
}

mat = function(fname, regex, formula, map.hgnc=FALSE, force=FALSE, fun.aggregate=sum) {
    if (!force && file.exists(file.path(icgc_data_dir, fname)))
        return()

    idmap = import('process/idmap')
    efiles = list.files(icgc_raw_dir, regex, recursive=T, full.names=T)

    file2matrix = function(fname) {
        message(fname)
        mat = .ar$construct(read.table(fname, header=T, sep="\t"),
                           formula = formula,
                           fun.aggregate=fun.aggregate)
        mat = mat[!rownames(mat) %in% c('?',''),] # aggr fun might handle his?
        if (map.hgnc)
            mat = idmap$gene(mat, to='hgnc_symbol', summarize=fun.aggregate)
        mat
    }
    obj = lapply(efiles, file2matrix)

    if (length(obj) == 0)
        stop("all file reads failed, something is wrong")

    mat = t(.ar$stack(obj, along=2, fill=0))
    if (grepl("\\.h5$", fname))
        h5store::h5save(mat, file=file.path(icgc_data_dir, fname))
    else
        save(mat, file=file.path(icgc_data_dir, fname))
}

df = function(fname, regex, transform=function(x) x, force=FALSE) {
    if (!force && file.exists(file.path(icgc_data_dir, fname)))
        return()

    files = list.files(icgc_raw_dir, regex, recursive=TRUE, full.names=TRUE)
    mat = do.call(rbind, lapply(files, function(f) cbind(
        study = b$grep("/([^/]+)/[^/]+$", f),
        read.table(f, header=TRUE, sep="\t") #FIXME: io$read should work here
    ))) %>% transform()

    if (grepl("\\.h5$", fname))
        h5store::h5save(mat, file=file.path(icgc_data_dir, fname))
    else
        save(mat, file=file.path(icgc_data_dir, fname))
}
