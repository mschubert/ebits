#' Perform FET for all elements in needle, haystack
#'
#' @param set1
#' @param set2
#' @param valid
fishers_exact_test = function(set1, set2, valid) {
    stop("this is not working yet")

    tp = length(intersect(set1, set2))
    fp = length(setdiff(set1, set2))
    tn = length(intersect(set1, setdiff(set2, valid)))
    fn = length(setdiff(set2, set1))
    stats::fisher.test(matrix(c(tp, fp, fn, tn), ncol=2))
}

if (is.null(module_names())) {
    re = fishers_exact_test(1:10, 2:11, 1:100)
}
