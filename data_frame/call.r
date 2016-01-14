.dp = import_package_('dplyr')

#' Call a function passing each row as arguments
#'
#' @param df           A call index of class `IndexedCall` or descendent thereof
#' @param fun          The function to call
#' @param ...          Further arguments to pass to `fun`
#' @param result_only  Return only the result column instead of the data.frame
#' @param rep          How many times to repeat the calls (useful for sampling funcs)
#' @param hpc_args     If not NULL, arguments to be passed to `hpc$Q`
#' @return             A data frame with the function call results
call = function(df, fun, ..., result_only=FALSE, rep=FALSE, hpc_args=NULL) {
    if (!inherits(df, "IndexedCall"))
        stop("df needs to be created with df$create_[formula_]index")

    if (identical(rep, FALSE) || is.null(rep)) {
        index = df$index
        add_rep = NULL
    } else {
        index = do.call(rbind, replicate(rep, df$index, simplify=FALSE))
        add_rep = c(sapply(1:rep, function(i) rep(i, nrow(df$index))))

        if (nrow(index) == 0) { # because rbind((1,0)*x) = (0,0), not (x,0)
            index = data.frame(.=add_rep)
            index$. = NULL
        }
    }

    # pass 'subsets' as argument if they are specified
    if ("subsets" %in% ls(df))
        df$args = c(df$args, subsets=list(df$subsets))

    # perform function calls either sequentially or with hpc module
    if (is.null(hpc_args))
        result = lapply(seq_len(nrow(index)), function(i) {
            do.call(fun, ..., c(as.list(index[i,,drop=FALSE]), df$args))
        })
    else
        result = do.call(import_('../hpc')$Q, c(list(fun=fun, ...), index,
            hpc_args, const=list(df$args)))

    if (!result_only) {
        index$rep = add_rep
        index$..id = rownames(index)

        # converting to list: https://github.com/hadley/dplyr/issues/1450
        # bind rows not with mixed length list (to be raised)
        # could simplify to c(as.list(r), ..id=n) if this worked
        concat = function(r,n) {
            if (is.null(r))
                list(..id=n)
            else {
                r = as.list(r)
                c(r, ..id=list(rep(n, length(r[[1]]))))
            }
        }
        result = mapply(concat, result, as.character(seq_along(result)),
                        USE.NAMES=TRUE, SIMPLIFY=FALSE) %>%
            .dp$bind_rows() %>%
            .dp$left_join(index, ., by="..id") %>%
            .dp$select(-..id)
        result$..id = NULL
        colnames(result)[colnames(result) == ""] = "result"
    }

    result
}
