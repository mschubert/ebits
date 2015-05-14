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
lm = function(formula, data=parent.frame(), min_pts=3, group=NULL, subsets=NULL, atomic=NULL, hpc_args=NULL) {
    idf = .df$from_formula(formula, data=data, group=group, subsets=subsets, atomic=atomic)
#FIXME: work with combination of parent.frame()+explicit data
    # ... : row arguments of the data.frame
    one_item = function(formula, data, subsets=NULL, ...) {
        args = list(...)

        # subset data according to subsets
        if (!is.null(subsets)) {
            data = lapply(data, function(x) x[subsets == args$subset,,drop=FALSE])
            args$subset = NULL
        }

        # subset data according to data.frame indices
        for (name in names(data))
            data[[name]] = data[[name]][, args[[name]], drop=TRUE]
        stopifnot(sapply(data, is.vector))

        # calculate the model
        pts = nrow(na.omit(do.call(cbind, data)))
        stats::lm(formula, data) %>%
            broom::tidy() %>%
            cbind(size = pts)
    }

    .df$call(idf, one_item, hpc_args=hpc_args)
}

#TODO: split out the indexing and function part
sem = function(formula, data=parent.frame(), min_pts=3, group=NULL, subsets=NULL, atomic=NULL, hpc_args=NULL) {
    idf = .df$from_formula(formula, group=group, subsets=subsets, atomic=atomic)

    # ... : row arguments of the data.frame
    one_item = function(formula, data, subsets=NULL, ...) {
        args = list(...)

        # subset data according to subsets
        if (!is.null(subsets)) {
            data = lapply(data, function(x) x[subsets == args$subset,,drop=FALSE])
            args$subset = NULL
        }

        # subset data according to data.frame indices
        for (name in names(data))
            data[[name]] = data[[name]][, args[[name]], drop=TRUE]
        stopifnot(sapply(data, is.vector))

        # calculate the model
#TODO: fill in simple multiple regression using SEM
    }

    .df$call(idf, one_item, hpc_args=hpc_args)
}

#cox = function(formula) {
#}
#
## this will be somewhat complicated
#sem = function(formula) {
#}
