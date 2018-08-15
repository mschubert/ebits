#' Plots segments on underlying point data
#'
#' @param segs  segment data
#' @param aes  ggplot mapping (required: y)
#' @param fml  formula for mapping between pts and segs
#' @param breaks  dashed line breaks indicating secondary axis
#' @param name  name of the secondary axis
#' @return  ggplot2 object
segs = function(segs, aes, fml, ..., breaks=NULL, name=waiver()) {
    if ("GRanges" %in% class(segs))
        segs = as.data.frame(segs)

    default_segs = aes(x=start, xend=end)
    default_segs[['yend']] = aes[['y']]
    aes_segs = utils::modifyList(default_segs, aes)

    args = list(...)
    defaults = list(size=1.5, color="green")
    defaults = defaults[!names(defaults) %in% names(aes_segs)]
    args = utils::modifyList(defaults, args)

    fscale = eval(fml[[2]][[3]], envir=environment(fml))
    if (is.null(breaks) && nrow(segs) > 0)
        breaks = 1:ceiling(max(segs[[as.character(aes[['y']][[2]])]])/fscale)
    breaks = breaks * fscale

    list(geom_hline(yintercept=breaks, color="grey", linetype="dashed"),
         do.call(geom_segment, c(list(data=segs, mapping=aes_segs), args)),
         facet_grid(. ~ seqnames, scales="free_x"),
         scale_y_continuous(sec.axis=sec_axis(fml, breaks=breaks, name=name)))
}
