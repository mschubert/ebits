import_package('ggplot2', attach=TRUE)

#' Plot read count correlation for chromstar models
#'
#' @param models   Chromstar combined model or list of model objects
#' @param cluster  cluster the axes (default: TRUE)
#' @return         GGplot correlation matrix
cor = function(models, cluster=TRUE) {
    if (any(grepl("[mM]ultiHMM", class(models))))
        bin_counts = model$bins$counts.rpkm
    else {
        bin_counts = sapply(models, function(m) m$bins$counts.rpkm)
        colnames(bin_counts) = sapply(models, function(m) m$info$ID)
    }

    mat = cor(bin_counts)
    df = reshape2::melt(mat, value.name='cor')

    if (cluster) {
        hc = stats::hclust(stats::dist(mat))
        df$Var1 = factor(df$Var1, levels=unique(df$Var1)[hc$order])
        df$Var2 = factor(df$Var2, levels=unique(df$Var2)[hc$order])
    }

    ggplot(df, aes(x=Var1, y=Var2)) +
        geom_tile(aes(fill=cor)) +
        geom_text(aes(label = sprintf("%.2f", cor)), size=min(4, 40/nrow(mat))) +
        labs(title = "Read count correlation", x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0)) +
        scale_fill_gradient2(low='blue', mid='white', high='red', limits=c(-1,1))
}
