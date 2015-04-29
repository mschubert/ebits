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
    if (is.environment(data))
        data = mget(all.vars(formula), envir=data)
    else
        data = data[all.vars(formula)]

    subs = list(...)
    for (i in seq_along(subs))
        data[[names(subs)[i]]] = .b$subset(data[[names(subs)[i]]], index=subs[[i]])

    # define what to do with one subset
    one_lm = function(data) {
        pts = nrow(na.omit(do.call(cbind, data))) #FIXME: this will be wrong with matrices
        if (pts >= min_pts)
            stats::lm(formula, data=data) %>% .clean_model_result()
    }

    # subset as specified in `subsets`
    if (is.null(subsets))
        one_lm(data)
    else
        lapply(data, function(d) .ar$split(d, along=1, subsets=subsets)) %>%
            .b$list$transpose() %>%
            lapply(one_lm) %>%
            .df$rbind(add_col="subset")
}

.clean_model_result = function(model, return_intercept=FALSE, add_size=TRUE) {
    #TODO: clean multi-response model names
    model = model %>%
        broom::tidy() %>%
        cbind(size = 10) #FIXME: size
    if (return_intercept)
        model
    else
        dplyr::filter(model, term != "(Intercept)")
}

#cox = function(formula) {
#}
#
## this will be somewhat complicated
#sem = function(formula) {
#}
