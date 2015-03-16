.b = import('../base')
.ar = import('../array')
.df = import('../data_frame')

#' @param formula  A formula that describes the data relationship
#' @param data     The data to process, or parent.frame() by default
#' @param min_pts  Minimum number of data points to calculate model from
#' @param return_intercept  Whether or not to return the intercept
#' @param ...      Used as variable names and indices along columns;
#                  this should be needed only if called from df$call
#' @return         A data.frame with the associations
lm = function(formula, data=parent.frame(), min_pts=3, subsets=NULL, return_intercept=FALSE, ...) {
    # subset as specified in `...`
    subs = list(...)
    data = data[all.vars(formula)]
    for (i in seq_along(subs))
        data[[names(subs)[i]]] = .b$subset(data[[names(subs)[i]]], index=subs[[i]])
    data = na.omit(as.data.frame(data))

    # define what to do with one subset
    one_lm = function(data) {
        if (nrow(data) >= min_pts) {
            result = cbind(broom::tidy(stats::lm(formula, data=data)), size=nrow(data))
            if (return_intercept)
                result
            else
                dplyr::filter(result, term != "(Intercept)")
        }
    }

    # subset as specified in `subsets`
    if (is.null(subsets))
        one_lm(data)
    else
        lapply(.ar$split(data, along=1, subsets=subsets), one_lm) %>%
            .df$rbind(add_col="subset")
}

#TODO: subsets should support df column name

#cox = function(formula) {
#}
#
## this will be somewhat complicated
#sem = function(formula) {
#}
