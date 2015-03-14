#' Call a function passing each row as arguments
#'
#' @param df           A data.frame whose columns represent function args
#' @param fun          The function to call
#' @param ...          Further arguments to pass to `fun`
#' @param result_only  Return only the result column instead of the data.frame
#' @param tidy         Try to convert the result into a data.frame
#' @return             A data frame with the function call results
call = function(df, fun, ..., result_only=FALSE, tidy=TRUE) {
    irow2result = function(i) {
        index_row = as.list(df[i,,drop=FALSE])
        result = do.call(fun, c(index_row, args))

        if (tidy)
            result = as.data.frame(result)

        if (result_only)
            c(result)
        else
            cbind(index_row, result)
    }

    args = list(...)
    if("attr_args" %in% class(df))
        args = c(args, attr(df, "args"))
    attr(df, "args") = NULL # do not copy for each row

    result = lapply(seq_len(nrow(df)), irow2result)

    if (tidy)
        do.call(rbind, result)
    else
        result
}

#' Use hpc$Q to process data.frame on a cluster
#'
#' @param df   A data.frame that should be processed
#' @param ...  Arguments to be passed to hpc$Q
call_hpc = function(df, ` fun`, ...) {
    hpc = import('../hpc')
    args = list(...)

    if ("attr_args" %in% class(df))
        args$more.args = append(args$more.args, attr(df, "args"))

    do.call(hpc$Q, c(args, df, list(` fun`=` fun`)))
}
