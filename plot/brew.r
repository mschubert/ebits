#' Shows available colour palettes, either as plot or return value
#'
#' @param plot  Show a graphical plot instead of textual output
show = function(plot=TRUE) {
    if (! plot)
        RColorBrewer::brewer.pal.info
    else
        tryCatch(
            RColorBrewer::display.brewer.all(),
            warning = function(x) RColorBrewer::brewer.pal.info
        )
}

#' Lists the palettes without trying to plot them
list = function() show(plot=FALSE)

#' Maps sortable values to sequential ColorBrewer palette
#'
#' @param x            Vector of values
#' @param palette      Name of the palette to use
#' @param na           Color to use for NA values; default: NA
#' @param interpolate  Use only palette colors or interpolate between
seq = function(x, palette="Greens", na=NA, interpolate=TRUE) {
    map(x = x,
        palette = palette,
        na = na,
        interpolate = interpolate)
}

#' Maps qualitative values to qualitative ColorBrewer palette
#'
#' @param x            Vector of values
#' @param palette      Name of the palette to use
#' @param na           Color to use for NA values; default: NA
qual = function(x, palette="Set1", na=NA) {
    map(x = x,
        palette = palette,
        na = na,
        interpolate = FALSE)
}

#' Maps diverging values to diverging ColorBrewer palette
#'
#' @param x            Vector of values
#' @param palette      Name of the palette to use
#' @param na           Color to use for NA values; default: NA
#' @param interpolate  Use only palette colors or interpolate between
#' @param center       Center around a value, e.g. diverging from 0
div = function(x, palette="RdBu", na=NA, interpolate=TRUE, center=NA) {
    map(x = x,
        palette = palette,
        na = na,
        interpolate = interpolate,
        center = center)
}

#' Map values to qualitative hues and sequential shades
qual_seq = function() {
    stop("not implemented")
}

#' Map values to qualitative hues and diverging shades
qual_div = function() {
    stop("not implemented")
}

#' General function to map values to colors
#'
#' @param palette      ColorBrewer palette to use
#' @param na           Colour to use if value is NA
#' @param interpolate  Interpolate between known Brewer colors
#' @param center       Center around a value, e.g. diverging from 0
map = function(x, palette, na=NA, interpolate=FALSE, center=NA) {
    max_cols = RColorBrewer::brewer.pal.info[palette,'maxcolors']
    if (interpolate == TRUE)
        n_col = 100
    else
        n_col = min(length(unique(x)), max_cols)
    pal_col = min(n_col, max_cols)

    xmin = min(x, na.rm=TRUE)
    xmax = max(x, na.rm=TRUE)
    if (!is.na(center)) {
        range_max = max(xmax - center, center - xmin)
        xmin = center - range_max
        xmax = center + range_max
    }

    quantize = base::seq(from = xmin, to = xmax, length.out = n_col)

    bins = cut(x, breaks=quantize, include.lowest=TRUE,
               label=cut(quantize, breaks=quantize)[-1])

    cols = grDevices::colorRampPalette(RColorBrewer::brewer.pal(pal_col, palette))(n_col)

    color = cols[as.numeric(bins)]
    color[is.na(color)] = na
    color
}
