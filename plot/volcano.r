library(ggplot2)
library(reshape2)
library(gridExtra)
library(RColorBrewer)
library(pheatmap)
library(plotrix)
import('./helpers', attach=T)
color = import('./color')
label = import('./label')

drawVolcano = function(df, base.size=1, p=0.05, ylim=c(NA,NA)) {
    df = df %>% #TODO: warn if removing points
        filter(pvalue < p | abs(effect)<max(abs(effect[pvalue<p]), na.rm=T)) %>%
        mutate(size = size*base.size)

    if (!'label' %in% colnames(df))
        stop("Column 'label' not found. You need to specify a label for your points")
    if (!'color' %in% colnames(df))
        stop("Column 'color' not found. Did you call plt$color$...?")

    ggplot(df, aes(x=effect, y=pvalue)) + 
        scale_y_continuous(trans=reverselog_trans(10),
                           label=scientific_10,
                           limits=ylim) +
#                           limits=c(1, min(df$pvalue)/10)) + # error if no signficant matches
        geom_point(size=sqrt(df$size), colour=df$color, na.rm=T) +
        geom_vline(xintercept=0, lwd=0.3) +
        geom_hline(yintercept=p, lwd=0.3, linetype=2) +
#        annotate("text", x=min(df$effect), y=0.05, hjust=1, vjust=2, 
#                 size=3.5, label="0.05", colour="black") +
        xlab("Effect size") + 
        ylab("Adjusted P-value") +
        theme_bw() +
        geom_text(mapping=aes(x=effect, y=pvalue, label=label), 
                  colour="#353535", size=2, vjust=-1, na.rm=T)
}
