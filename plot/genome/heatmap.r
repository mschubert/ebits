import_package("ggplot2", attach=TRUE)
import_package("dplyr", attach=TRUE)
#af = readRDS("../../../mad2-transposon/data/wgs/413s.rds")
#af2 = readRDS("../../../mad2-transposon/data/wgs/401t.rds")
#aneu = list("413s"=af, "401t"=af2)

#' Plot a copy number heatmap along the genome
#'
#' from stefano/util.r
#'
#' @param segs   A data.frame of genomic segments
#' @param aes    Aesthetics mapping: 'fill' (copy color), 'y' (cell), 'facet' (sample; optional)
#' @return       A ggplot2 object with copy number segments along the genome
heatmap = function(segs, aes=aes(fill=copy.number, y=cell), switch=NULL) {
    if (!is.factor(segs$cell))
        segs$cell = factor(segs$cell)
    segs$copy.number = pmin(segs$copy.number, 8)
    segs$copy.number = factor(segs$copy.number)
    levels(segs$copy.number) = sub("8", "8+", levels(segs$copy.number))
    facet = tryCatch(as.character(rlang::quo_get_expr(aes$facet)), error=function(e) ".")

    ggplot(segs) +
        geom_rect(aes(xmin=start/1e6, xmax=end/1e6,
                      ymin=as.integer(cell)-0.5, ymax=as.integer(cell)+0.5,
                      fill=copy.number), na.rm=TRUE) +
        facet_grid(paste(facet, "~ seqnames"), scales="free", space="free", switch=switch) +
        scale_fill_manual(values=cnacol, na.value="white") +
        guides(fill = guide_legend(override.aes=list(size=5))) +
        theme_classic() +
        theme(panel.spacing = unit(0.1, "lines"),
              panel.background = element_rect(fill = NA),
              panel.ontop = TRUE,
              panel.grid.minor.y = element_line(colour="white", size=0.2),
              axis.text.x = element_text(angle=60, hjust=1)) +
        scale_x_continuous(expand=c(0,0), name="Position (Mb)", breaks=c(50,100,150)) +
        scale_y_continuous(expand=c(0,0), breaks=seq_along(levels(segs$cell)),
                           labels=levels(segs$cell), trans=scales::reverse_trans())
}

#' @inheritParams heatmap
heatmap_aneuHMM = function(aneu) {
    cluster_sample = function(ah_list) {
        bins = t(sapply(ah_list, function(x) x$bins$copy.number))
        tibble(cell = sapply(ah_list, function(x) x$ID),
               ord = rank(uwot::umap(bins, n_components=1))) %>%
            arrange(ord)
    }
    ex_segs = function(ah_list) {
        ah_list %>%
            lapply(function(x) as_tibble(x$segments)) %>%
            bind_rows(.id="cell")
    }

    if (class(aneu[[1]]) == "aneuHMM" || inherits(aneu[[1]], "GRanges")) { # one sample
        ords = cluster_sample(aneu)
        segs = ex_segs(aneu) %>%
            left_join(ords, by="cell") %>%
            mutate(cell = factor(cell, levels=ords$cell))
        heatmap(segs, aes(fill=copy.number, y=cell))
    } else if (class(aneu[[1]][[1]]) == "aneuHMM" || inherits(aneu[[1]][[1]], "GRanges")) { # multiple samples
        if (is.null(names(aneu)))
            names(aneu) = seq_along(aneu)
        ords = lapply(aneu, cluster_sample) %>%
            bind_rows(.id="sample")
        segs = lapply(aneu, ex_segs) %>%
            bind_rows(.id="sample") %>%
            mutate(sample = factor(sample, levels=names(aneu))) %>%
            left_join(ords, by=c("sample", "cell")) %>% # assumes "cell" is unique across samples
            mutate(cell = factor(cell, levels=ords$cell))

        heatmap(segs, aes(fill=copy.number, y=cell, facet=sample), switch="y") +
            theme(strip.background.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  strip.placement = "outside",
                  panel.spacing.y=unit(0.3, "lines"))
    } else
        stop("class of object needs to be aneuHMM")
}

cnacol = c(`2`="#ffffbf", `0`="#0530a1", `1`="#66c2a5", `3`="#fdae61", `4`="#d53e4f",
           `5`="#9e0142", `6`="#87001f", `7`="#540010", `8+`="#000000")
