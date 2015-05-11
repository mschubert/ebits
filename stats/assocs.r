.b = import('../base')
.ar = import('../array')
.df = import('../data_frame')

#' @param formula  A formula that describes the data relationship
#' @param data     The data to process, or parent.frame() by default
#' @param min_pts  Minimum number of data points to calculate model from (TODO: each level for multi-reg)
#' @param return_intercept  Whether or not to return the intercept
#' @param ...      Used as variable names and indices along columns;
#                  this should be needed only if called from df$call
#' @return         A data.frame with the associations
lm = function(formula, data=parent.frame(), min_pts=3, subsets=NULL, return_intercept=FALSE, ...) {
    # subset as specified in `...`
    data_env = as.environment(data)
    fvars = all.vars(formula)
    data = c(mget(fvars[fvars %in% ls(data_env)], envir=data_env),
             mget(fvars[!fvars %in% ls(data_env)], envir=parent.frame()))

    subs = list(...)
    for (i in seq_along(subs))
        data[[names(subs)[i]]] = .b$subset(data[[names(subs)[i]]], index=subs[[i]])

    # define what to do with one subset
    one_lm = function(data) {
        pts = nrow(na.omit(do.call(cbind, data)))
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
    model = model %>%
        broom::tidy() %>%
        cbind(size = NA) #FIXME: size
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
