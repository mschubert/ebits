.b = import_('../base', attach_operators=FALSE)
.ic = import_('./IndexedCall')

#' expand.grid() function not converting to factors
expand_grid = function(..., KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE) {
    base::expand.grid(..., KEEP.OUT.ATTRS=KEEP.OUT.ATTRS, stringsAsFactors=stringsAsFactors)
}

#' Creates an IndexedCall object
#'
#' @param ...          Variables to be iterated
#' @param args         Variables to be supplied in every call
#' @param expand_grid  Do all combination of the iterated variables
create_index = function(..., args=list(), expand_grid=FALSE) {
    if (expand_grid)
        index = .b$expand_grid(...)
    else
        index = dplyr::as_data_frame(list(...))

    # set class so that our object can handle it
    attr(index, "class") = "data.frame"

    .ic$IndexedCall$new(index=index, args=args)
}
