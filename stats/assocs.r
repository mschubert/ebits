import('../base/operators')

lm = function(formula, data=environment(formula), min_pts=3, return_intercept=FALSE) {
    pts = nrow(na.omit(do.call(cbind, data)))
    if (pts < min_pts)
        NULL
    else {
        re = stats::lm(formula, data) %>%
            broom::tidy() %>%
            cbind(size = pts)
        if (return_intercept)
            re
        else
            dplyr::filter(re, term != "(Intercept)")
    }
}

coxph = function(formula, data=environment(formula), min_pts=3) {
    pts = nrow(na.omit(do.call(cbind, data)))
    if (pts < min_pts)
        NULL
    else {
        fstr = strsplit(sub("\\+", ",", deparse(formula)), "~")[[1]]
        formula = formula(paste("survival::Surv(", fstr[1], ") ~", fstr[-1]))
        survival::coxph(formula, data) %>%
            broom::tidy() %>%
            cbind(size = pts)
    }
}

#' @param formula     A formula that describes the data relationship
#' @param train_args  mlr args, e.g. list("regr.glmnet", dfmax=5)
#' @param data        The data to process, or parent.frame() by default
#' @param models      Return model objects instead of fit statistics
#' @param aggr        Return aggregate statistics instead of fit statistics
#' @param xval        Number of cross-validation steps to perform
#' @param min_pts     Minimum number of data points to calculate model from
#' @param subsets     Vector whose unique set is used to split the data along rows
#' @return            A data.frame containing the trained models
ml = function(formula, train_args, data=environment(formula),
              models=FALSE, aggr=FALSE, xval=5, min_pts=10) {
    library(mlr) # needed for options

    # set all formula vars to atomic
    indep_vars = all.vars(formula[[3]])
    dep_vars = all.vars(formula[[2]])
    stopifnot(length(dep_vars) == 1) # only allow one readout variable

    # bind data and response to data.frame, use var name if one column
    for (i in seq_along(data))
        if (is.matrix(data[[i]]) && ncol(data[[i]]) == 1 && is.null(colnames(data[[i]])))
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
        result$pred$data
}

if (is.null(module_name())) {
    # lm models
    re1 = lm(Sepal.Width ~ Sepal.Length, data=iris)
    testthat::expect_equal(re1['1','term'], 'Sepal.Length')
    testthat::expect_equal(re1['1','size'], 150)

    re2 = lm(Sepal.Width ~ Sepal.Length, data=iris, return_intercept=TRUE)
    ref2 = broom::tidy(stats::lm(Sepal.Width ~ Sepal.Length, data=iris))
    testthat::expect_equal(dplyr::select(re2, -size), ref2)
    
    re3 = lm(Sepal.Width ~ Sepal.Length, data=iris, min_pts=200)
    testthat::expect_null(re3)

    # coxph models
    # ...
}
