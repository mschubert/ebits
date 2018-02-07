import_package('ggplot2', attach=TRUE)

#' Plot genomic frequencies of states
#'
#' We extract from the model because chromstaR::genomicFrequencies is only
#' showing the number of bins, not the fraction of the genome covered (if
#' per.mark=FALSE; otherwise, it includes the width - don't know why)
#'
#' @param model  Chromstar model object
#' @return       ggplot2 object (bar graph)
frequencies = function(model) {
    freqs = chromstaR::genomicFrequencies(model)
    fdf = reshape2::melt(as.array(freqs$frequency))
    p = ggplot(fdf, aes(x=Var1, y=value)) +
        scale_y_log10() +
        labs(x="modification", y="number") +
        geom_bar(stat="identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, hjust=1))

    if ("Var2" %in% colnames(fdf))
        p + facet_wrap(~ Var2)
    else
        p
}
