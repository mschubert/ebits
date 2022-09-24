import_package("SummarizedExperiment", attach=TRUE)
import_package("ggplot2", attach=TRUE)
import_package("patchwork", attach=TRUE)
.calc = import('./calc')
.plt = import('../../plot')

#' Volcano plots from DESeq2 results
#'
#' @param ...    Plot title (first) and genes or gene set result tibbles
#' @param nrow   Number of rows to wrap the plots in (default: auto)
#' @param title_size    Text size of the title
#' @param title_height  Multiplier for the title panel height
#' @param add_design  NULL or eset to add design plot for (at first position)
#' @return  A patchwork object
plot_volc = function(..., nrow=NULL, title_size=6, title_height=0.1, add_design=NULL) {
    args = rlang::dots_list(..., .named=TRUE)
    res = args[-1]

    plots = mapply(function(x, xn) .plt$volcano(x) + ggtitle(xn),
                   x=res, xn=names(res), SIMPLIFY=FALSE)

    if (!is.null(add_design))
        plots = c(list(plot_design(add_design)), plots)

    (.plt$text(args[[1]], size=title_size) / wrap_plots(plots, nrow=nrow)) +
        plot_layout(heights=c(title_height, max(c(1,nrow))))
}

#' Plot the design matrix as table
#'
#' @param eset  A DESeq2 data set
plot_design = function(eset) {
    eset = .calc$clean_obj(eset)
    mm = model.matrix(DESeq2::design(eset), data=colData(eset))
    patchwork::wrap_elements(gridExtra::tableGrob(mm))
}
