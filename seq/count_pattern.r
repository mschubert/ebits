#' Count occurrence of a pattern along a genome, or in a set of ranges
#'
#' @param pat  Character string of a pattern
#' @param gen  Genome object
#' @param rng  Ranges to search within (optional)
#' @param rc   Include reverse complement in search (default: FALSE)
#' @return     GRagens object of all matches
count_pattern = function(pat, gen, rng=NULL, rc=FALSE) {
    count_one = function(x) {
        pats = Biostrings::vmatchPattern(x, gen)
        if (is.null(rng))
            length(pats)
        else
            count_overlaps(rng, pats)
    }
    x = Biostrings::DNAString(pat)
    counts = count_one(x)
    if (rc)
        counts = counts + count_one(Biostrings::reverse(x))
    counts
}


