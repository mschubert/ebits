#' Return an expression object from series ID(s)
#'
#' @param ids  ArrayExpress or NCBI GEO identifier
#' @return     The expression object
get_series = function(ids) {
    gs = function(id) {
        if (grepl("^E-", id)) {
            library(ArrayExpress) # error otherwise
            ArrayExpress::ArrayExpress(id) #TODO: try-catch
        } else if (grepl("GSE", id))
            stop("not implemented")
        else
            stop("need ArrayExpress or GEO identifiers")
    }
    if (length(ids) == 1)
        gs(ids)
    else
        sapply(ids, gs)
}

#' Return an expression object from a directory or a vector of file names
#'
#' @param files  Directory path or vector of file names to be included
#' @return       The expression object
get_files = function(directory=".", files=NA, pattern="\\.(CEL|cel)(\\.gz)?$") {
    regex = "\\.(CEL|cel)(\\.gz)?$"
    lsCEL = list.files(path=directory, pattern=regex)
    if (identical(files, NA))
        files = lsCEL
    fnames = sub(regex, "", files)
    if (!grepl(regex, files[1]))
        files = sapply(files, function(f) grep(f, lsCEL, value=T))
    files = sapply(files, function(f) file.path(directory, f))

    rawData = oligo::read.celfiles(filenames=files)
    oligo::sampleNames(rawData) = fnames
    rawData
}
