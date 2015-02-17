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
