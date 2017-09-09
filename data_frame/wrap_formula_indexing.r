#' Takes a function and returns a function that index-wraps the provided
#'
#' @param FUN  A model function to be index-wrapped
#' @return     A function that calls the supplied
wrap_formula_indexing = function(FUN) {
    new_FUN = function() {
        #' @param ...   Arguments as defined in the data.frame row
        one_item = function(..., data, subsets=NULL) {
            args = list(...)

            # subset all iterated data that is not masked by 'atomic' flags
            is_iterated = intersect(names(data), names(args))
            for (name in is_iterated)
                data[[name]] = idx$subset(data[[name]], args[[name]],
                                          atomic=atomic_class, drop=TRUE)

            # subset data according to subsets (irrespective of atomics)
            if (!is.null(subsets)) {
                for (name in names(data))
                    data[[name]] = idx$subset(data[[name]],
                                              subsets==args$subset,
                                              along=1)
            }

            # calculate the model
            call_args = as.list(match.call())
            call_args = call_args[intersect(names(call_args), names(FUN_formals))]
            call_args$data = data
            do.call(FUN, call_args)
        }

        df = import_('../data_frame')
        idx = import_('../base/indexing')
        func = import_('../base/functional')
        ci = import('../data_frame/create_formula_index')

        # replace with: call_args = as.list(func$eval_call())[-1] ?
        call_args = as.list(func$match_call_defaults())[-1]
        call_args = call_args[!names(call_args) %in% c("rep","hpc_args")]
        content = do.call(ci$create_formula_index, call_args)

        df$call(index=content$index, fun=one_item, const=content$const,
                rep=rep, result_only=result_only, hpc_args=hpc_args)
    }

    FUN_formals = formals(FUN)
    if (!"data" %in% names(FUN_formals))
        stop("function needs 'data' argument in order to be wrapped")
    add_formals = list(group=NULL, subsets=NULL, atomic=NULL,
                       rep=FALSE, hpc_args=NULL, result_only=FALSE)
    formals(new_FUN) = c(FUN_formals, add_formals)
    assign("FUN", FUN, envir=environment(new_FUN))
    pryr::unenclose(new_FUN)
}

if (is.null(module_name())) {
    library(testthat)

    fx = function(f, data=parent.frame(), atomic_class='vector') names(data)
    wf = wrap_formula_indexing(fx)
    
    re1 = wf(Sepal.Length ~ Sepal.Width, data=iris)
    expect_true(all(unlist(re1) %in% colnames(iris)))

#    re2 = wf(Sepal.Length ~ Sepal.Width, data=iris, rep=5)
#    expect_equal(nrow(re2), nrow(re1)*5)
#    expect_equal(re2$rep, rep(1:5, each=2))

    fx = function(f, data=parent.frame(), atomic_class='vector') 1
    wf = wrap_formula_indexing(fx)

    width = cbind(sepal=iris$Sepal.Width, petal=iris$Petal.Width)
    length = cbind(sepal=iris$Sepal.Length, petal=iris$Petal.Length)
    re3 = wf(width ~ length)
    expect_equal(nrow(re3), 4)
    expect_equal(setdiff(colnames(width), re3$width), character(0))
    expect_equal(setdiff(colnames(length), re3$length), character(0))

    # manually importing lm
    mylm = import('../stats/export_indexed/lm')$lm
    wlm = wrap_formula_indexing(mylm)
    y = as.matrix(c(1:4))
    x = matrix(1:20 + rnorm(20), nrow=4)
    re4 = wlm(y ~ x)
    expect_equal(re4$y, rep(1, ncol(x)))
    expect_equal(re4$x, 1:ncol(x))
    expect_equal(re4$term, rep("x", ncol(x)))
    expect_true(all(c("estimate", "statistic", "p.value") %in% colnames(re4)))

    # integrative test using linear model
#    lm_fun = function(x, index) {
#        st = import('../stats')
#        mod = st$lm(x ~ 0 + Species, data=index)
#    }
#    re = lm_fun(x=iris[1:3], index=iris['Species'])
#    expect_equal(unique(re$term), paste0("Species", unique(iris$Species)))
#    expect_equal(unique(re$x), colnames(iris)[1:3])
#    expect_equal(unique(re$size), 50)
}
