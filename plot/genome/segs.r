#' Plots segments on underlying point data
#'
#' @param segs  segment data
#' @param aes  ggplot mapping (required: y)
#' @param fml  formula for mapping between pts and segs
#' @return  ggplot2 object
segs = function(segs, aes, fml, ..., breaks=NULL) {
    if ("GRanges" %in% class(segs))
        segs = as.data.frame(segs)

    default_segs = aes(x=start, xend=end)
    default_segs[['yend']] = aes[['y']]
    aes_segs = utils::modifyList(default_segs, aes)

    args = list(...)
    defaults = list(size=1.5, color="green")
    defaults = defaults[!names(defaults) %in% names(aes_segs)]
    args = utils::modifyList(defaults, args)

    if (is.null(breaks))
        breaks = 1:3
    breaks = breaks * eval(fml[[2]][[3]], envir=environment(fml))

    sn = "ploidy"
    list(do.call(geom_segment, c(list(data=segs, mapping=aes_segs), args)),
         facet_grid(. ~ seqnames, scales="free_x"),
         scale_y_continuous(sec.axis=sec_axis(fml, breaks=breaks, name=sn)))
}
