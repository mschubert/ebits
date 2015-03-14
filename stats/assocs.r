# linear associations (anova-like)

#' @param formula  A formula that describes the data relationship
#' @param data     The data to process, or parent.frame() by default
#' @param min_pts  Minimum number of data points to calculate model from
#' @param ...      Used as variable names and indices along columns;
#                  this should be needed only if called from df$call
lm = function(formula, data=parent.frame(), min_pts=3, ...) {
    subs = list(...)
    for (i in seq_along(subs))
        data[[names(subs)[i]]] = data[[names(subs)[i]]][,subs[i]]

    if (nrow(data) < min_pts)
        NULL
    else
        cbind(broom::tidy(lm(formula, data=data)), size=nrow(data))
}

#.cox = function(formula) {
#}
#
## this will be somewhat complicated
#.sem = function(formula) {
#}
