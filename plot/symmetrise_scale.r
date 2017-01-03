#' Symmetrises a scale
#'
#' also works in a faceted grid
#' from: http://stackoverflow.com/q/37635016
#'
#' @param p     A ggplot2 object
#' @param axis  Which axis to symmetrise, either "x" or "y"
symmetrise_scale = function(p, axis = "x"){
  gb = ggplot_build(p)
  type = switch(axis, "x" = "x.range", "y" = "y.range")
  lims = sapply(gb$layout$panel_ranges, "[[", type)
  fname = as.character(p$facet$params$facets)
  facets = gb$layout$panel_layout[ ,fname, drop=FALSE]
  lims2 = as.vector(t(tcrossprod(apply(abs(lims), 2, max), c(-1,1))))
  dummy = setNames(data.frame(rep(facets, each=2), lims2), c(fname, axis))
  switch(axis, 
         "x" = p + geom_blank(data=dummy, aes(x=x, y=Inf)), 
         "y" = p + geom_blank(data=dummy, aes(x=Inf, y=y)))
}
