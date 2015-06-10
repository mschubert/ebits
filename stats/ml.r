.b = import('../base')
.df = import('../data_frame')

#' @param formula     A formula that describes the data relationship
#' @param train_args  mlr args, e.g. list("regr.glmnet", dfmax=5)
#' @param data        The data to process, or parent.frame() by default
#' @param models      Return model objects instead of fit statistics
#' @param aggr        Return aggregate statistics instead of fit statistics
#' @param xval        Number of cross-validation steps to perform
#' @param min_pts     Minimum number of data points to calculate model from
#' @param subsets     Vector whose unique set is used to split the data along rows
#' @return            A data.frame containing the trained models
ml = function(formula, train_args, data=parent.frame(), models=FALSE, aggr=FALSE, xval=5, min_pts=10, subsets=NULL, hpc_args=NULL) {
    library(mlr) # needed for options

    # set all formula vars to atomic
    indep_vars = all.vars(formula[[3]])
    dep_vars = all.vars(formula[[2]])
    stopifnot(length(dep_vars) == 1) # only allow one readout variable

    one_item = function(formula, data, subsets=NULL, ...) {
        args = list(...)

        # subset data according to subsets
        if (!is.null(subsets)) {
            data = lapply(data, function(x) x[subsets == args$subset,,drop=FALSE])
            args$subset = NULL
        }

        # bind data and response to data.frame, use var name if one column
        for (i in seq_along(data))
            if (ncol(data[[i]]) == 1 && is.null(colnames(data[[i]])))
                colnames(data[[i]]) = names(data)[i]
        data = as.data.frame(na.omit(do.call(cbind, data)))
        colnames(data) = make.names(colnames(data)) # mlr needs this

        # make sure we have enough data
        if (nrow(data) < min_pts)
            return(list(NA))

        # train model, max 4 variables
        learner = do.call(mlr::makeLearner, as.list(train_args))
        task = mlr::makeRegrTask(data=data, target=dep_vars)
        result = mlr::crossval(learner, task, iters=xval, models=models)

        if (aggr)
            result$aggr
        else if (models)
            result$models
        else
            result$pred
    }

    # construct df + call
    idf = .df$from_formula(formula, data=data, subsets=subsets, atomic=indep_vars)
    .df$call(idf, one_item, hpc_args=hpc_args, tidy=!models)
}
