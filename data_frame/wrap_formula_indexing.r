#' Takes a function and returns a function that index-wraps the provided
#'
#' @param FUN  A model function to be index-wrapped
#' @return     A function that calls the supplied
wrap_formula_indexing = function(FUN) {
    new_FUN = function() {
        #' @param ...   Arguments as defined in the data.frame row
        one_item = function(data, subsets=NULL, ...) {
            args = list(...)

            # subset all iterated data that is not masked by 'atomic' flags
            is_iterated = intersect(names(data), names(args))
            for (name in is_iterated)
                data[[name]] = idx$subset(data[[name]], args[[name]], atomic=atomic_class, drop=TRUE)

            # subset data according to subsets (irrespective of atomic specifications)
            if (!is.null(subsets)) {
                for (name in names(data))
                    data[[name]] = idx$subset(data[[name]], subsets==args$subset, along=1)
            }

            # calculate the model
            call_args = as.list(match.call())
            call_args = call_args[intersect(names(call_args), names(FUN_formals))]
            call_args$data = data
            do.call(FUN, call_args)
        }

        func = import_('../base/functional')
        idx = import_('../base/indexing')
        df = import_('../data_frame')

#FIXME: commented line below does not work
        fca = function(ca) {
            if (class(ca) %in% c("name", "call"))
                eval(ca, envir=ca_env)
            else
                ca
        }
        ca_env = parent.frame()
        call_args = as.list(func$match_call_defaults())[-1]
        call_args = lapply(call_args, fca)

#        call_args = as.list(func$eval_call())[-1]
        call_args = call_args[!names(call_args) %in% c("rep","hpc_args")]

        idf = do.call(df$create_formula_index, call_args)
        df$call(idf, one_item, rep=rep, result_only=result_only, hpc_args=hpc_args)
    }

    FUN_formals = formals(FUN)
    if (!"data" %in% names(FUN_formals))
        stop("function needs 'data' argument in order to be wrapped")
    add_formals = list(group=NULL, subsets=NULL, atomic=NULL, rep=FALSE, hpc_args=NULL, result_only=FALSE)
    formals(new_FUN) = c(FUN_formals, add_formals)
    new_FUN
}

if (is.null(module_name())) {
    library(testthat)

    fx = function(f, data=parent.frame(), atomic_class='vector') names(data)
    wf = wrap_formula_indexing(fx)
    
    re1 = wf(Sepal.Length ~ Sepal.Width, data=iris)
    expect_true(all(unlist(re1) %in% colnames(iris)))

    re2 = wf(Sepal.Length ~ Sepal.Width, data=iris, rep=5)
    expect_equal(nrow(re2), 5)

    fx = function(f, data=parent.frame(), atomic_class='vector') 1
    wf = wrap_formula_indexing(fx)

    width = cbind(sepal=iris$Sepal.Width, petal=iris$Petal.Width)
    length = cbind(sepal=iris$Sepal.Length, petal=iris$Petal.Length)
    re3 = wf(width ~ length)
    expect_equal(nrow(re3), 4)
    expect_equal(setdiff(colnames(width), re3$width), character(0))
    expect_equal(setdiff(colnames(length), re3$length), character(0))

    # integrative test using linear model
    lm_fun = function(x, index) {
        st = import('../stats')
        mod = st$lm(x ~ 0 + Species, data=index)
    }
    re = lm_fun(x=iris[1:3], index=iris['Species'])
    expect_equal(unique(re$term), paste0("Species", unique(iris$Species)))
    expect_equal(unique(re$x), colnames(iris)[1:3])
    expect_equal(unique(re$size), 50)
}
