#' Function for easy access to `mlr` machine learning models
#'
#' @param formula     A formula that describes the data relationship
#' @param train_args  mlr args, e.g. list("regr.glmnet", dfmax=5)
#' @param data        The data to process, or parent.frame() by default
#' @param models      Return model objects instead of fit statistics
#' @param aggr        Return aggregate statistics instead of fit statistics
#'                    This can be a list of mlr "Measure" objects (e.g. mlr::mse)
#' @param xval        Number of cross-validation steps to perform
#' @param min_pts     Minimum number of data points to calculate model from
#' @param shuffle_labels  Whether to shuffle the response variable - for null models
#' @param subsets     Vector whose unique set is used to split the data along rows
#' @return            A data.frame containing the trained models
ml = function(formula, train_args, data=environment(formula),
              models=FALSE, aggr=FALSE, xval=5, min_pts=10, shuffle_labels=FALSE) {
    library(mlr) # needed for options

    # set all formula vars to atomic
    indep_vars = all.vars(formula[[3]])
    dep_vars = all.vars(formula[[2]])
    stopifnot(length(dep_vars) == 1 && length(dim(dep_vars)) < 2) # only vector response
    if (identical(shuffle_labels, TRUE))
        data[[dep_vars]] = sample(data[[dep_vars]])

    # bind data and response to data.frame, use var name if one column
    for (i in seq_along(data))
        if (is.matrix(data[[i]]) && ncol(data[[i]]) == 1 && is.null(colnames(data[[i]])))
            colnames(data[[i]]) = names(data)[i]
    data = as.data.frame(na.omit(do.call(cbind, data)))
    colnames(data) = make.names(colnames(data), unique=TRUE) # mlr needs this

    # make sure we have enough data
    if (nrow(data) < min_pts)
        return(NULL)

    # train model, max 4 variables
    learner = do.call(mlr::makeLearner, as.list(train_args))
    task = mlr::makeRegrTask(data=data, target=dep_vars)

    if (is.list(aggr)) {
        result = mlr::crossval(learner, task, iters=xval, models=models, measures=aggr)
        aggr = TRUE
    } else
        result = mlr::crossval(learner, task, iters=xval, models=models)

    if (aggr)
        result$aggr
    else if (models)
        result$models
    else
        result$pred$data
}

if (is.null(module_name())) {
}
