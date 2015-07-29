#' Call a function passing each row as arguments
#'
#' @param df           A call index of class `IndexedCall` or descendent thereof
#' @param fun          The function to call
#' @param ...          Further arguments to pass to `fun`
#' @param result_only  Return only the result column instead of the data.frame
#' @param tidy         Try to convert the result into a data.frame
#' @param hpc_args     If not NULL, arguments to be passed to `hpc$Q`
#' @return             A data frame with the function call results
call = function(df, fun, ..., result_only=FALSE, tidy=TRUE, hpc_args=NULL) {
    if (!inherits(df, "IndexedCall"))
        stop("df needs to be created with df$create_[formula_]index")

    if (is.null(hpc_args))
        .serial(df=df, fun=fun, ..., result_only=result_only, tidy=tidy)
    else
        do.call(.hpc, c(list(df=df, ` fun`=fun, ...,
                result_only=result_only, tidy=tidy), hpc_args))
}

.serial = function(df, fun, ..., result_only=FALSE, tidy=TRUE, hpc_args=NULL) {
    irow2result = function(i) {
        index_row = as.list(df@index[i,,drop=FALSE])
        result = do.call(fun, c(index_row, args))

        if (is.null(result))
            return(NULL)

        if (tidy)
            result = as.data.frame(result)

        if (result_only)
            c(result)
        else
            cbind(index_row, result)
    }

    args = c(list(...), df@args, subsets=list(df@subsets))

    result = lapply(seq_len(nrow(df@index)), irow2result)

    if (tidy)
        do.call(rbind, result)
    else
        result
}

.hpc = function(df, ` fun`, ..., result_only=FALSE, tidy=TRUE) {
    hpc = import('../hpc')
    args = c(list(...), df@args, subsets=list(df@subsets))

    result = do.call(hpc$Q, c(more.args=list(args), df@index, list(` fun`=` fun`)))

    if (!result_only) {
        rownames(df@index) = as.character(1:nrow(df@index))
        result = lapply(names(result), function(i)
            c(as.list(df@index[i,,drop=FALSE]), as.list(result[[i]]))
        )
    }
    if (tidy) {
        result = lapply(result, as.data.frame)
        result = do.call(rbind, result)
    }

    result
}
