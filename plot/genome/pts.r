.theme = import('../theme')

#' Plots segments on underlying point data
#'
#' @param pts  point data, derived from GRanges
#' @param aes  ggplot2 mapping (required: nothing)
#' @return  ggplot2 object
pts = function(pts, aes, ...) {
    if ("GRanges" %in% class(pts))
        pts = as.data.frame(pts)
    if (!"midpoint" %in% colnames(pts))
        pts$midpoint = (pts$start + pts$end) / 2

    aes_default = aes(x=midpoint)
    aes_pt = utils::modifyList(aes_default, aes)

    args = list(...)
    defaults = list(shape=1, alpha=0.5)
    defaults = defaults[!names(defaults) %in% names(aes_pt)]
    args = utils::modifyList(defaults, args)

    list(do.call(geom_point, c(list(data=pts, mapping=aes_pt), args)),
         facet_grid(. ~ seqnames, scales="free_x"),
         theme(panel.spacing = unit(0, "lines")),
         .theme$no_x())
}
