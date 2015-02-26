library(ggplot2)
library(reshape2)
.b = import('../base')
.st = import('../stats')

#' Plots a given data.frame as a linear fit with optional subsets
#'
#' @param df        data.frame holding at least the columns specified by x, y, and label
#' @param x         Column to be plotted on the horizontal axis
#' @param y         Column to be plotted on the vertical axis
#' @param label     Column of label to be used for each sample; indicates subsets
#' @param drop      Whether to drop unused factor levels in `label`
#' @param pt.size   Size of the points indicating the samples
#' @param fit.size  Width of the line(s) used for indicating the fit
linear_fit = function(df, x="x", y="y", label="label", drop=T, pt.size=4, fit.size=5) {
    result = st$assocs(df[[y]], df[[x]], subsets=df[[label]], sumsq=T)
    print(result$p.value)

    if (length(unique(df[[label]])) > 1)
        df = df[df[[label]] %in% result$subset[result$p.value<0.05],]

#    rsq = round(result$main*1000) / 10
#    if (drop)
#        df$tissue = sapply(as.character(df$tissue), function(t) paste(t, "-", rsq[t], "%"))
#    if (!drop && !is.na(only) && length(only)==1)
#        tit = paste(only, "-", rsq[only], "%")
#    else 
#        tit = paste("Correlation between", pathway, "activity and", drug, "response")

    ggplot(df, aes_string(x=x, y=y, colour=label)) +
        geom_smooth(aes_string(fill=label), size=fit.size, method=stats::lm, se=F, na.rm=T, alpha=0.1) +
        geom_point(aes_string(fill=label), pch=21, size=pt.size, colour="black", alpha=1, na.rm=T) +
        scale_fill_discrete(drop=drop) +
        scale_colour_discrete(drop=drop)
}
