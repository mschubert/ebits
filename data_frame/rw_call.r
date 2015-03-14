rw_call = function(df, fun, args=list(), keep_params=TRUE, tidy=TRUE) {
    irow2result = function(i) {
        index_row = df[i,,drop=TRUE] # named list
        re = do.call(fun, c(index_row, args))

        if (tidy)
            re = as.data.frame(re)

        if (keep_params)
            cbind(index_row, re)
        else
            re
    }

    result = lapply(seq_len(nrow(df)), irow2result)

    if (tidy)
        do.call(rbind, result)
    else
        result
}
