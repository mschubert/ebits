#' Reads the gmt file format
#'
#' @param fname    The file name
#' @param weights  Read weights in file
#' @return         A list of lists of entries
read_gmt = function(fname, weights=FALSE) {
    line2list = function(l) {
        re = strsplit(l, "\t")[[1]]
        targets = sapply(re[3:length(re)], function(f) strsplit(f, ","))
        genes = unname(sapply(targets, function(f) f[1]))
        if (weights) {
            if (length(targets[[1]]) > 1)
                val = sapply(targets, function(f) as.numeric(f[2]))
            else
                val = rep(1, length(targets))
            genes = setNames(val, genes)
        }
        setNames(list(toupper(genes)), re[1])
    }
    ll = readLines(fname)
    do.call(c, lapply(ll, line2list))
}
