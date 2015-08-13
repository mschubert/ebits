#' Reads a text file in yaml format
#'
#' @param fname  The file name to read
#' @param n      Maximum number of documents to parse (default: Inf)
#' @param drop   Drop the list if there is only one element in it
#' @return       A nested list representing the yaml structure
read_yaml = function(fname, n=Inf, drop=TRUE) {
    flines = readLines(fname)
    splits = cumsum(flines == "---")

    mask = splits <= n
    flines = flines[mask]
    splits = splits[mask]

    docs = unname(base::split(flines, splits))
    parsed = lapply(docs, function(d) yaml::yaml.load(paste(d, collapse="\n")))
    if (length(parsed) == 1 && drop == TRUE)
        parsed[[1]]
    else
        parsed
}
