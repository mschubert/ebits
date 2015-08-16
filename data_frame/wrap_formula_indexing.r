#' Takes a function and returns a function that index-wraps the provided
#'
#' @param FUN  A model function to be index-wrapped
#' @return     A function that calls the supplied
wrap_formula_indexing = function(FUN) {
    new_FUN = function() {
        #' @param ...   Arguments as defined in the data.frame row
        one_item = function(data, subsets=NULL, ...) {
            args = list(...)

            # subset data according to data.frame indices, specified subsets
            is_iterated = intersect(names(data), names(args))
            for (name in is_iterated) {
                tmp = idx$subset(data[[name]], args[[name]], atomic=atomic_class, drop=TRUE)
                if (is.null(subsets))
                    data[[name]] = tmp
                else
                    data[[name]] = idx$subset(tmp, subsets==args$subset, along=1)
            }

            # calculate the model
            call_args = as.list(match.call())
            call_args = call_args[intersect(names(call_args), names(FUN_formals))]
            call_args$data = data
            do.call(FUN, call_args)
        }

        func = import('../base/functional')
        idx = import('../base/indexing')
        df = import('../data_frame')
        call_args = as.list(func$match_call_defaults())[-1]
        call_args = lapply(call_args, function(a) {
            if (class(a) %in% c("name", "call"))
                eval(a)
            else
                a
        })
        call_args = call_args[!names(call_args) %in% c("rep","hpc_args")]
        idf = do.call(df$create_formula_index, call_args)
        df$call(idf, one_item, rep=rep, hpc_args=hpc_args)
    }

    FUN_formals = formals(FUN)
    if (!"data" %in% names(FUN_formals))
        stop("function needs 'data' argument in order to be wrapped")
    add_formals = list(group=NULL, subsets=NULL, atomic=NULL, rep=FALSE, hpc_args=NULL)
    formals(new_FUN) = c(FUN_formals, add_formals)
    new_FUN
}

if (is.null(module_name())) {
    fx = function(f, data=environment(f), atomic_class='vector') names(data)
    wf = wrap_formula_indexing(fx)
    
    re1 = wf(Sepal.Length ~ Sepal.Width, data=iris)
    testthat::expect_true(all(unlist(re1) %in% colnames(iris)))

    re2 = wf(Sepal.Length ~ Sepal.Width, data=iris, rep=5)
    testthat::expect_equal(nrow(re2), 5)

    fx = function(f, data=environment(f), atomic_class='vector') 1
    wf = wrap_formula_indexing(fx)

    width = cbind(sepal=iris$Sepal.Width, petal=iris$Petal.Width)
    length = cbind(sepal=iris$Sepal.Length, petal=iris$Petal.Length)
    re3 = wf(width ~ length)
    testthat::expect_equal(nrow(re3), 4)
    testthat::expect_equal(setdiff(colnames(width), re3$width), character(0))
    testthat::expect_equal(setdiff(colnames(length), re3$length), character(0))
}
