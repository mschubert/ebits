.func = import('../base/functional')

#' Takes a function and returns a function that index-wraps the provided
#'
#' @param FUN  A model function to be index-wrapped
#' @return     A function that calls the supplied
wrap_formula_indexing = function(FUN) {
    new_FUN = function() {
        #' @param ...   Arguments as defined in the data.frame row
        one_item = function(data, subsets=NULL, ...) {
            args = list(...)

            # subset data according to subsets
            if (!is.null(subsets)) {
                data = lapply(data, function(x) x[subsets == args$subset,,drop=FALSE])
                args$subset = NULL
            }

            # subset data according to data.frame indices
            is_iterated = intersect(names(data), names(args))
            for (name in is_iterated)
                data[[name]] = data[[name]][, args[[name]], drop=TRUE]
            stopifnot(sapply(data[is_iterated], is.vector))

            # calculate the model
            call_args = as.list(match.call())
            call_args = call_args[intersect(names(call_args), names(FUN_formals))]
            call_args$data = data
            do.call(FUN, call_args)
        }

        df = import('../data_frame')
        call_args = as.list(.func$match_call_defaults())[-1]
        call_args = call_args[names(call_args) != "hpc_args"]
        idf = do.call(df$create_formula_index, call_args)
        df$call(idf, one_item, hpc_args=hpc_args)
    }

    FUN_formals = formals(FUN)
    if (!"data" %in% names(FUN_formals))
        stop("function needs 'data' argument in order to be wrapped")
    add_formals = list(group=NULL, subsets=NULL, atomic=NULL, hpc_args=NULL)
    formals(new_FUN) = c(FUN_formals, add_formals)
    new_FUN
}
