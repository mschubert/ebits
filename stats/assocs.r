.b = import('../base')
.ar = import('../array')
.df = import('../data_frame')

#' @param formula  A formula that describes the data relationship
#' @param data     The data to process, or parent.frame() by default
#' @param min_pts  Minimum number of data points to calculate model from (TODO: each level for multi-reg)
#' @param group    Variables to iterate together
#' @param subsets  Vector whose unique set is used to split the data along rows
#' @param atomic   Variables that should not be iterated, e.g. a coefficients matrix
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
        if (pts < min_pts)
            NULL
        else
            stats::lm(formula, data) %>%
                broom::tidy() %>%
                cbind(size = pts)
    }

    .df$call(idf, one_item, hpc_args=hpc_args)
}

#' @param formula  A formula of kind `time + status ~ independent variables`
#' @param data     The data to process, or parent.frame() by default
#' @param min_pts  Minimum number of data points to calculate model from (TODO: each level for multi-reg)
#' @param group    Variables to iterate together
#' @param subsets  Vector whose unique set is used to split the data along rows
#' @param atomic   Variables that should not be iterated, e.g. a coefficients matrix
#' @return         A data.frame with the associations
coxph = function(formula, data=parent.frame(), min_pts=3, group=NULL, subsets=NULL, atomic=NULL, hpc_args=NULL) {
#TODO: split out the indexing and function part
    idf = .df$from_formula(formula, data=data, group=group, subsets=subsets, atomic=atomic)

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
        fstr = strsplit(sub("\\+", ",", deparse(formula)), "~")[[1]]
        formula = formula(paste("survival::Surv(", fstr[1], ") ~", fstr[-1]))
        survival::coxph(formula, data) %>%
            broom::tidy() %>%
            cbind(size = pts)
    }

    .df$call(idf, one_item, hpc_args=hpc_args)
}
