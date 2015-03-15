#' @param formula  A formula that describes the data relationship
#' @param data     The data to process, or parent.frame() by default
#' @param min_pts  Minimum number of data points to calculate model from
#' @param return_intercept  Whether or not to return the intercept
#' @param ...      Used as variable names and indices along columns;
#                  this should be needed only if called from df$call
#' @return         A data.frame with the associations
lm = function(formula, data=parent.frame(), min_pts=3, return_intercept=FALSE, ...) {
    subs = list(...)
    for (i in seq_along(subs))
        data[[names(subs)[i]]] = data[[names(subs)[i]]][,subs[[i]]]
    data = na.omit(as.data.frame(data))

    if (nrow(data) >= min_pts) {
        result = cbind(broom::tidy(stats::lm(formula, data=data)), size=nrow(data))
        if (return_intercept)
            result
        else
            dplyr::filter(result, term != "(Intercept)")
    }
}

#cox = function(formula) {
#}
#
## this will be somewhat complicated
#sem = function(formula) {
#}
