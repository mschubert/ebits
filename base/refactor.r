import_('./operators')

#' Creates a factor where levels are ordered by an external value
#' 
#' @param x         A vector to be converted in a factor
#' @param level_by  A vector of the same length we call `sort()` on
#' @param ...       Arguments passed to sort
refactor = function(x, level_by, summarize=NULL, ...) {
    if (is.null(summarize))
        order = unique(names(sort(setNames(level_by, x), ...)))
    else {
        df = data.frame(x=x, l=level_by) %>%
            dplyr::group_by(l) %>%
            dplyr::summarize(y = summarize(x))
        order = unique(names(sort(setNames(df$y, df$x), ...)))
    }
    factor(x, levels=order)
}
