#' expand.grid() function not converting to factors
expand_grid = function(..., KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE) {
    base::expand.grid(..., KEEP.OUT.ATTRS=KEEP.OUT.ATTRS, stringsAsFactors=stringsAsFactors)
}
