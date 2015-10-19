.b = import('../base')
.st = import('../stats')
#.spf = import('../stats/process_formula')

#' Plots a given data.frame as a linear fit with optional subsets
#'
#' @param df        data.frame holding at least the columns specified by x, y, and label
#' @param x         Column to be plotted on the horizontal axis
#' @param y         Column to be plotted on the vertical axis
#' @param label     Column of label to be used for each sample; indicates subsets
#' @param drop      Whether to drop unused factor levels in `label`
#' @param pt.size   Size of the points indicating the samples
#' @param fit.size  Width of the line(s) used for indicating the fit
linear_fit = function(formula, subsets=NULL, data=parent.frame(),
                      drop=TRUE, pt.size=4, fit.size=5) {

#TODO: formula length=3, [[1]]="~"
#TODO: if subsets is single char, take column in data
    x = as.matrix(base::eval(formula[[3]], envir=data))
    y = as.matrix(base::eval(formula[[2]], envir=data))

    # allow either subsets or multiple columns in one matrix
    if (((ncol(x) > 1) + (ncol(y) > 1) + (!is.null(subsets))) > 1)
        stop("can only take multiple cols in one matrix or subsets")
    if (ncol(x) > 1) {
        subsets = c(sapply(colnames(x), function(i) rep(i, nrow(x))))
        y = rep(y, ncol(x))
        x = c(x)
    } else if (ncol(y) > 1) {
        subsets = c(sapply(colnames(y), function(i) rep(i, nrow(y))))
        x = rep(x, ncol(y))
        y = c(y)
    } else
        subsets = rep(1, nrow(x))

    result = st$lm(y ~ x, subsets=subsets) %>%
        filter(term == "x" & p.value < 0.05)

    df = data.frame(x=x, y=y, subsets=subsets) %>%
        filter(subsets %in% result$subset)
#    print(result$p.value)

#    if (length(unique(df[[label]])) > 1)
#        df = df[df[[label]] %in% result$subset[result$p.value<0.05],]

#    rsq = round(result$main*1000) / 10
#    if (drop)
#        df$tissue = sapply(as.character(df$tissue), function(t) paste(t, "-", rsq[t], "%"))
#    if (!drop && !is.na(only) && length(only)==1)
#        tit = paste(only, "-", rsq[only], "%")
#    else 
#        tit = paste("Correlation between", pathway, "activity and", drug, "response")

    ggplot(df, aes(x=x, y=y, colour=subsets)) +
        geom_smooth(aes(fill=subsets), size=fit.size, method=stats::lm, se=F, na.rm=T, alpha=0.1) +
        geom_point(aes(fill=subsets), pch=21, size=pt.size, colour="black", alpha=1, na.rm=T) +
        scale_fill_discrete(drop=drop) +
        scale_colour_discrete(drop=drop)
}
