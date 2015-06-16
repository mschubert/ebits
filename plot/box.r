library(ggplot2)
library(reshape2)
library(modules)
b = import('base')
ar = import('array')

#' Plots a given data.frame as a boxplot with optional subsets
#'
#' @param df         data.frame holding at least the columns specified by x, y, and label
#' @param x          Column to be plotted on the horizontal axis
#' @param y          Column to be plotted on the vertical axis
#' @param label      Column of label to be used for each sample; indicates subsets
#' @param quantiles  If `x="group"` quantiles will eb used to assign `up`, `down`, and `null`
#' @param drop       Whether to drop unused factor levels in `label`
box = function(df, x="group", y="y", label="label", quantiles=c(0.2,0.8), drop=T) {
    df$group = st$map.quantiles(score, quantiles, c('down', 'null', 'up'))

    ggplot(df, aes_string(x="group", y="y")) +
        geom_point(aes_string(fill=label), position=position_jitter(width=.25),
            pch=21, size=3, colour="black") +
        geom_boxplot(fill="grey", outlier.shape = NA) +
        scale_fill_discrete(drop=drop) +
        scale_colour_discrete(drop=drop)
}
