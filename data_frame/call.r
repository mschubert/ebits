#' Call a function passing each row as arguments
#'
#' @param df           A call index of class `IndexedCall` or descendent thereof
#' @param fun          The function to call
#' @param ...          Further arguments to pass to `fun`
#' @param result_only  Return only the result column instead of the data.frame
#' @param tidy         Try to convert the result into a data.frame
#' @param rep          How many times to repeat the calls (useful for sampling funcs)
#' @param hpc_args     If not NULL, arguments to be passed to `hpc$Q`
#' @return             A data frame with the function call results
call = function(df, fun, ..., result_only=FALSE, tidy=TRUE, rep=FALSE, hpc_args=NULL) {
    if (!inherits(df, "IndexedCall"))
        stop("df needs to be created with df$create_[formula_]index")

    if (is.null(hpc_args))
        .serial(df=df, fun=fun, ..., result_only=result_only, tidy=tidy, rep=rep)
    else
        do.call(.hpc, c(list(df=df, ` fun`=fun, ..., rep=rep,
                result_only=result_only, tidy=tidy), hpc_args))
}

.serial = function(df, fun, ..., result_only=FALSE, tidy=TRUE, rep=FALSE, hpc_args=NULL) {
    irow2result = function(i) {
        do.call(fun, ..., c(as.list(df@index[i,,drop=FALSE]), args))
    }

    args = c(df@args, subsets=list(df@subsets))

    if (identical(rep, FALSE) || is.null(rep)) {
        index = df@index
        add_rep = NULL
    } else {
        index = do.call(rbind, replicate(rep, df@index, simplify=FALSE))
        add_rep = c(sapply(1:nrow(df@index), function(i) rep(i, rep)))
    }

    result = lapply(seq_len(nrow(df@index)), irow2result)
    index$rep = add_rep

    if (!result_only) {
        rownames(index) = as.character(1:nrow(index))
        result = lapply(names(result), function(i) {
            if (is.null(names(result[[1]])))
                c(as.list(index[i,,drop=FALSE]), result=as.list(result[[i]]))
            else
                c(as.list(index[i,,drop=FALSE]), as.list(result[[i]]))
        })
    }
    if (tidy) {
        result = lapply(result, as.data.frame)
        result = plyr::rbind.fill(result)
    }

    result
}

.hpc = function(df, ` fun`, ..., result_only=FALSE, rep=FALSE, tidy=TRUE) {
    hpc = import('../hpc')
    args = list(c(df@args, subsets=list(df@subsets)))

    if (identical(rep, FALSE) || is.null(rep)) {
        index = df@index
        add_rep = NULL
    } else {
        index = do.call(rbind, replicate(rep, df@index, simplify=FALSE))
        add_rep = c(sapply(1:nrow(df@index), function(i) rep(i, rep)))
    }

    result = do.call(hpc$Q, c(list(` fun`=` fun`, ...), index, more.args=args))
    index$rep = add_rep

    if (!result_only) {
        rownames(index) = as.character(1:nrow(index))
        result = lapply(names(result), function(i) {
            if (is.null(names(result[[1]])))
                c(as.list(index[i,,drop=FALSE]), result=as.list(result[[i]]))
            else
                c(as.list(index[i,,drop=FALSE]), as.list(result[[i]]))
        })
    }
    if (tidy) {
        result = lapply(result, as.data.frame)
        result = plyr::rbind.fill(result)
    }

    result
}
