import_package("SummarizedExperiment", attach=TRUE)
import_package("ggplot2", attach=TRUE)
import_package("patchwork", attach=TRUE)
.calc = import('./calc')
.plt = import('../../plot')

plot_pca = function(eset) {
    vst = DESeq2::varianceStabilizingTransformation(eset)

    pcadata = DESeq2::plotPCA(vst, intgroup=c("sample name", "cell line", "treatment", "amplification", "DM amount"), returnData=TRUE)
    pcavar = round(100 * attr(pcadata, "percentVar"))
    shapes = c(HeLa=21, HT29=22)

    ggplot(pcadata, aes(PC1, PC2)) +
        geom_point(aes(fill=treatment, shape=cell.line, size=DM.amount), alpha=0.5) +
        scale_fill_discrete(guide=guide_legend(override.aes=list(shape=21))) +
        scale_shape_manual(values=shapes) +
        scale_size_manual(values=c(none=3, low=5, mid=6, high=7)) +
        xlab(paste0("PC1: ", pcavar[1], "% variance")) +
        ylab(paste0("PC2: ", pcavar[2], "% variance")) +
        ggrepel::geom_text_repel(aes(label=sample.name))
}

#' Volcano plots from DESeq2 results
#'
#' @param ...    Plot title (first) and genes or gene set result tibbles
#' @param nrow   Number of rows to wrap the plots in (default: auto)
#' @param title_size    Text size of the title
#' @param title_height  Multiplier for the title panel height
#' @return  A patchwork object
plot_volc = function(..., nrow=NULL, title_size=6, title_height=0.1) {
    args = rlang::dots_list(...)
    res = args[-1]

    plots = mapply(function(x, xn) .plt$volcano(x) + ggtitle(xn),
                   x=res, xn=names(res), SIMPLIFY=FALSE)

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
