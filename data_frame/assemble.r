import('../base/operators')
.ar = import('../array/stack')

#' Creates a data.frame from named vectors
#'
#' @param ...  Multiple named vectors. If arguments are named, those
#'             will be the names used in the result columns. If not,
#'             the variable names will be used for columns.
#' @return     A data_frame with stacked rows
assemble = function(...) {
    l. = list(...)
    mynames = names(l.) %or% unlist(match.call(expand.dots=FALSE)$...)
    myclasses = sapply(l., class)
    names(l.) = mynames
    re = as.data.frame(.ar$stack(l., along=2))
#TODO: return right classes
}

if (is.null(module_name())) {
}
