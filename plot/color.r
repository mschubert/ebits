#' Takes a data.frame and calculates colors according to p-value
#'
#' @param df      Data frame
#' @param pvalue  Field name to use for p-values
#' @param cmap    The color map to be used
pvalue = function(df, pvalue="pvalue", cmap="") {
    warning("not implemented")
    df
}

#' Takes a data.frame and calculates colors according to p-value
#'
#' @param df      Data frame
#' @param pvalue  Field name to use for p-values
#' @param effect  Field name to use for effect size
#' @param dir     Direction of effect considered positive [+/-1]
#' @param thresh  Threshold for significance
p_effect = function(df, pvalue="pvalue", effect="effect", dir=1, thresh=0.05) {
    df[[effect]] = sign(dir) * df[[effect]]
    df$color = rep(rgb(0, 151, 30, 120, maxColorValue=255), nrow(df)) # green
    df$color[df[[effect]] < 0] = rgb(225, 0, 0, 120, maxColorValue=255) # red
    df$color[df[[pvalue]] > 0.05] = rgb(200, 200, 200, 120, maxColorValue=255) # grey
    df[[effect]] = sign(dir) * df[[effect]]
    df
}

#' Shows available colour palettes, either as plot or return value
palettes = function() {
    tryCatch(
        RColorBrewer::display.brewer.all(),
        warning = function(x) RColorBrewer::brewer.pal.info
    )
}

#' @param df        Main data frame
#' @param value     Field name of variable to derive colours from
#' @param color     Field name of variable to save colours to
#' @param palette   ColorBrewer palette to use
#' @param na        Colour to use if value is NA
#' @param quantize  Number of colours or sequence of breaks - e.g.: `seq(-1,1,by=0.25)`
#TODO: alpha?
#TODO: handle categorical, mix between categorical+continuous [subsets=]
brew = function(df, value="value", color="color", palette="RdYlBu", na=NA, quantize=100) {
    if (length(quantize) == 1) {
        n_col = quantize
        quantize = seq(from = min(df[[value]], na.rm=TRUE),
                       to = max(df[[value]], na.rm=TRUE),
                       length.out = n_col)
    } else
        n_col = length(quantize)

    max_cols = RColorBrewer::brewer.pal.info[palette,'maxcolors']
    if (n_col > max_cols)
        pal_col = max_cols
    else
        pal_col = n_col

    bins = cut(df[[value]], breaks=quantize, include.lowest=TRUE,
               label=cut(quantize, breaks=quantize)[-1])
    cols = RColorBrewer::colorRampPalette(brewer.pal(pal_col, palette))(n_col)
    df[[color]] = cols[as.numeric(bins)]
    df[[color]][is.na(df[[color]])] = na
    df
}
