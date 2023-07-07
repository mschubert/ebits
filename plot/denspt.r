#' Draw a density Hex with points at its border
#'
#' @param data     A data.frame
#' @param mapping  ggplot2 mapping for variables: x, y, fill, label
#' @param n_tile   How many density tiles to draw along each axis
#' @param draw_pt  How many points to draw at lowest density
#' @param nodens   Do not draw density if fewer points than this number
#' @param draw_label  How many points to label
#' @param always_label  Character vector of labels to always draw
#' @param max_ov   Maximumg overlap for point labels (ggrepel)
#' @param h  Kernel width for density estimation in number of tiles (x=y or x,y)
#' @param palette  The color palette to use
#' @param pal_dir  The direction of the palette
#' @param pal_alpha  The transparency of the density layer
denspt = function(data, mapping, n_tile=50, draw_pt=500, nodens=500, draw_label=60, always_label=c(),
                  tsize=NULL, max_ov=25, h=15, ..., palette="RdPu", pal_dir=1, pal_alpha=1) {
    mis_map = setdiff(c("x", "y", "label"), names(mapping))
    if (length(mis_map) > 0)
        stop("Missing mapping: ", paste(mis_map, collapse=", "))

    lx = rlang::as_name(mapping$x)
    ly = rlang::as_name(mapping$y)
    ll = rlang::as_name(mapping$label)
    keep = apply(data[c(lx, ly)], 1, function(x) !any(is.na(x) | is.infinite(x)))
    data = data[keep,]
    x = data[[lx]]
    y = data[[ly]]

    h = c(diff(range(x, na.rm=TRUE))/h[1], diff(range(y, na.rm=TRUE))/rev(h)[1])
    dens = MASS::kde2d(x, y, h, n=n_tile)
    data$dens = fields::interp.surface(dens, data.frame(x=x, y=y))
    data$draw_pt = ifelse(rank(data$dens)<draw_pt, "pt", NA)
    data[[ll]] = ifelse(rank(data$dens)<draw_label | data[[ll]] %in% always_label, data[[ll]], NA)
    if (is.null(tsize))
        tsize = 1.5 + 5 * 1/sqrt(mean(nchar(data[[ll]]), na.rm=TRUE))

    dens_geom = list()
    if (nrow(data) > nodens)
        dens_geom = list(geom_hex(bins=n_tile, alpha=pal_alpha, color=NA))

    ggplot(data, mapping) +
        dens_geom +
        geom_hline(yintercept=0, color="grey", linetype="dashed") +
        geom_vline(xintercept=0, color="grey", linetype="dashed") +
        geom_point(aes(shape=draw_pt, ...)) +
        geom_smooth(color="blue", method="lm", se=FALSE) +
        ggrepel::geom_label_repel(max.overlaps=max_ov, size=tsize,
            min.segment.length=0, segment.alpha=0.3, fill="#ffffffa0", label.size=NA,
            max.iter=1e6, max.time=10, label.padding = unit(0.12, "lines"),
            box.padding = unit(0.01, "lines"), na.rm=TRUE) +
        scale_fill_distiller(palette=palette, trans="log10",
            breaks=c(1,10,50,200,1000,5000,20000), direction=pal_dir) +
        scale_shape_manual(values=c(pt=19)) +
        guides(shape = "none",
               fill = guide_legend(override.aes=list(alpha=pal_alpha))) +
        theme_minimal()
}
