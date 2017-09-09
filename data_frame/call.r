.b = import('../base')
.dp = import_package_('dplyr')
.func = import('../base/functional')

#' Call a function passing each row as arguments
#'
#' @param df           A data.frame or index of class `IndexedCall` or descendent thereof
#' @param fun          The function to call
#' @param ...          Further arguments to pass to `fun`
#' @param result_only  Return only the result column instead of the data.frame
#' @param rep          How many times to repeat the calls (useful for sampling funcs)
#' @param hpc_args     If not NULL, arguments to be passed to `hpc$Q`
#' @return             A data frame with the function call results
call = function(index, fun, const=list(), result_only=FALSE, rep=FALSE, hpc_args=NULL) {
    if (identical(rep, FALSE) || is.null(rep)) {
        add_rep = NULL
    } else {
        index = do.call(rbind, replicate(rep, index, simplify=FALSE))
        add_rep = c(sapply(1:rep, function(i) rep(i, nrow(index))))

        if (nrow(index) == 0) { # because rbind((1,0)*x) = (0,0), not (x,0)
            index = data.frame(.=add_rep)
            index$. = NULL
        }
    }

    # perform function calls either sequentially or with hpc module
    if (is.null(hpc_args))
        result = lapply(seq_len(nrow(index)), function(i) {
            do.call(fun, c(as.list(index[i,,drop=FALSE]), const))
        })
    else
        result = do.call(clustermq::Q,
             c(list(fun=fun, const=const), index, hpc_args))

    # don't use tidyr::unnest() here, too slow
    if (!result_only) {
        index$rep = add_rep
        index$.id = as.character(1:nrow(index))
        names(result) = as.character(1:length(result))

        error_empty = function(r) class(r)[1] %in% c("NULL", "try-error")
        result = result[! sapply(result, error_empty)]
        result = .b$lnapply(result, function(x) {
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

if (!is.null(module_name())) {
    library(testthat)

    x = 1:2
    y = 3:4
    z = 10

    f1 = function(x,y) x+y
    r1 = call(expand.grid(x=x,y=y), f1)

    f2 = function(x,y,z) x+y+z
    r2 = call(expand.grid(x=x,y=y), f2, const=list(z=10))

    expect_equal(r1[c('x','y')], r2[c('x','y')])
    expect_equal(r1$result, rowSums(expand.grid(x,y)))
    expect_equal(r2$result, z + rowSums(expand.grid(x,y)))

    # test correct ordering of empty results
    ferr = function(x) if (x %% 2 == 0) x
    r3 = call(data.frame(x=1:5), ferr)
    expect_equal(r3$result, c(NA, 2, NA, 4, NA))
}
