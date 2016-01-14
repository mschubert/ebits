#' Takes a data.frame and calculates colors according to p-value
#'
#' @param df      Data frame
#' @param pvalue  Field name to use for p-values
#' @param cmap    The color map to be used
pvalue = function(df, pvalue="p.value", cmap="") {
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
p_effect = function(df, pvalue="p.value", effect="estimate", dir=1, thresh=0.05) {
    df[[effect]] = sign(dir) * df[[effect]]
    df$color = rep(rgb(0, 151, 30, 120, maxColorValue=255), nrow(df)) # green
    df$color[df[[effect]] < 0] = rgb(225, 0, 0, 120, maxColorValue=255) # red
    df$color[df[[pvalue]] > thresh] = rgb(200, 200, 200, 120, maxColorValue=255) # grey
    df[[effect]] = sign(dir) * df[[effect]]
    df$.x = df[[effect]]
    df$.y = df[[pvalue]]
    df
}

# Graphics helpers
import_('../base/functional', attach = TRUE)

# Color helper functions {{{
transparent = function (colors, alpha = 0.5) {
    c = col2rgb(colors)
    rgb(c['red', ], c['green', ], c['blue', ], alpha * 255, maxColorValue = 255)
}

lighten = function (colors, factor = 0.5) {
    c = col2rgb(colors)
    l = function (c) 255 * factor + c * (1 - factor)
    rgb(l(c['red', ]), l(c['green', ]), l(c['blue', ]), maxColorValue = 255)
}

darken = function (colors, factor = 0.5) {
    c = col2rgb(colors)
    d = function (c) c * (1 - factor)
    rgb(d(c['red', ]), d(c['green', ]), d(c['blue', ]), maxColorValue = 255)
}

hsv2col = function (col)
    apply(col, COLS, lpartial(do.call, hsv) %.% as.list)
# }}}
