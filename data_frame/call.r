.dp = import_package_('dplyr')
.ci = import_('./create_index')

#' Call a function passing each row as arguments
#'
#' @param df           A data.frame or index of class `IndexedCall` or descendent thereof
#' @param fun          The function to call
#' @param ...          Further arguments to pass to `fun`
#' @param result_only  Return only the result column instead of the data.frame
#' @param rep          How many times to repeat the calls (useful for sampling funcs)
#' @param hpc_args     If not NULL, arguments to be passed to `hpc$Q`
#' @return             A data frame with the function call results
call = function(df, fun, ..., result_only=FALSE, rep=FALSE, hpc_args=NULL) {
    if (!inherits(df, "IndexedCall"))
        df = do.call(.ci$create_index, df)

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

    # add (...) to df$args
    const_args = c(list(...), df$args)

    # pass 'subsets' as argument if they are specified
    if ("subsets" %in% ls(df))
        const_args = c(const_args, subsets=list(df$subsets))

    # perform function calls either sequentially or with hpc module
    #TODO: replace local call by dplyr::do(rowwise(df))
    if (is.null(hpc_args))
        result = lapply(seq_len(nrow(index)), function(i) {
            do.call(fun, c(as.list(index[i,,drop=FALSE]), const_args))
        })
    else
        result = do.call(import_('../hpc')$Q, c(list(fun=fun), index,
            hpc_args, const=list(const_args)))

    # don't use tidyr::unnest() here, too slow
    if (!result_only) {
        index$rep = add_rep
        index$.id = 1:nrow(index)

        error_empty = function(r) class(r)[1] %in% c("NULL", "try-error")
        result = result[! sapply(result, error_empty)]
        result = b$lnapply(result, function(x) {
            if (is.list(x))
                x
            else
                list(result=x)
        })

        result = data.table::rbindlist(result, fill=TRUE, idcol=".id")
        result = merge(index, result, by=".id", all.x=TRUE)
        result$.id = NULL
    }

    result
}

if (is.null(module_name())) {
    library(testthat)
	b = import('../base')

	x = 1:2
	y = 3:4
	z = 10

	f1 = function(x,y) x+y
	r1 = b$expand_grid(x=x,y=y) %>% call(f1)

	f2 = function(x,y,z) x+y+z
	r2 = b$expand_grid(x=x,y=y) %>% call(f2, z=z)

	expect_equal(r1[c('x','y')], r2[c('x','y')])
	expect_equal(r1$result, rowSums(expand.grid(x,y)))
	expect_equal(r2$result, z + rowSums(expand.grid(x,y)))
}
