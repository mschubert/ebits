#' Call a function passing each row as arguments
#'
#' @param df           A data.frame whose columns represent function args
#' @param fun          The function to call
#' @param ...          Further arguments to pass to `fun`
#' @param result_only  Return only the result column instead of the data.frame
#' @param tidy         Try to convert the result into a data.frame
#' @param hpc_args     If not NULL, arguments to be passed to `hpc$Q`
#' @return             A data frame with the function call results
call = function(df, fun, ..., result_only=FALSE, tidy=TRUE, hpc_args=NULL) {
    if (is.null(hpc_args))
        .serial(df=df, fun=fun, ..., result_only=result_only, tidy=tidy)
    else
        .hpc(df=df, ` fun`=fun, ..., result_only=result_only, tidy=tidy) #TODO: hpc_args+fun args
}

.serial = function(df, fun, ..., result_only=FALSE, tidy=TRUE, hpc_args=NULL) {
    irow2result = function(i) {
        index_row = as.list(df[i,,drop=FALSE])
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

    args = list(...)
    if("attr_args" %in% class(df)) {
        args = c(args, attr(df, "args"))
        attr(df, "args") = NULL # do not copy for each row
    }

    result = lapply(seq_len(nrow(df)), irow2result)

    if (tidy)
        do.call(rbind, result)
    else
        result
}

.hpc = function(df, ` fun`, ..., result_only=FALSE, tidy=TRUE) {
    hpc = import('../hpc')
    args = list(...)

    if ("attr_args" %in% class(df)) {
        args$more.args = append(args$more.args, attr(df, "args"))
        attr(df, "args") = NULL
    }

    result = do.call(hpc$Q, c(args, df, list(` fun`=` fun`)))

    if (!result_only) {
        rownames(df) = as.character(1:nrow(df))
        result = lapply(names(result), function(i)
            cbind(as.list(df[i,,drop=FALSE]), result[[i]])
        )
    }
    if (tidy)
        result = do.call(rbind, result)

    result
}
