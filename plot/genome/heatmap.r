import_package("ggplot2", attach=TRUE)
import_package("dplyr", attach=TRUE)

.cluster_sample = function(ah_list) {
    bins = t(sapply(ah_list, function(x) x$bins$copy.number))
    tibble(cell = sapply(ah_list, function(x) x$ID),
           ord = rank(uwot::umap(bins, n_components=1))) %>%
        arrange(ord)
}

.extract_aneuHMM = function(aneu) {
    ex_segs = function(ah_list) {
        ah_list %>%
            lapply(function(x) as_tibble(x$segments)) %>%
            bind_rows(.id="cell")
    }

    if (class(aneu[[1]]) == "aneuHMM" || inherits(aneu[[1]], "GRanges")) { # one sample
        ords = .cluster_sample(aneu)
        ex_segs(aneu) %>%
            left_join(ords, by="cell") %>%
            mutate(cell = factor(cell, levels=ords$cell))
    } else if (class(aneu[[1]][[1]]) == "aneuHMM" || inherits(aneu[[1]][[1]], "GRanges")) { # multiple samples
        if (is.null(names(aneu)))
            names(aneu) = seq_along(aneu)
        ords = lapply(aneu, .cluster_sample) %>%
            bind_rows(.id="sample")
        lapply(aneu, ex_segs) %>%
            bind_rows(.id="sample") %>%
            mutate(sample = factor(sample, levels=names(aneu))) %>%
            left_join(ords, by=c("sample", "cell")) %>% # assumes "cell" is unique across samples
            mutate(cell = factor(cell, levels=ords$cell))
    } else
        stop("class of object needs to be aneuHMM")
}

cnacol = c(`2`="#ffffbf", `0`="#0530a1", `1`="#66c2a5", `3`="#fdae61", `4`="#d53e4f",
           `5`="#9e0142", `6`="#87001f", `7`="#540010", `8+`="#000000")

#' Plot a copy number heatmap along the genome
#'
#' from stefano/util.r
#'
#' @param segs     A data.frame of genomic segments
#' @param fill     Column name for segment fill (i.e., copy number)
#' @param cell     Column name for cells within a sample
#' @param chrs     Column name for chromosomes
#' @param sample   Column name for samples
#' @param cluster  Whether to cluster cells using uwot::umap (default: if "cell" is non-factor)
#' @param max_copies  Maximum number of copy states
#' @return         A ggplot2 object with copy number segments along the genome
heatmap = function(segs, sample = c("Sample", "sample", "."), cell=c("Cell", "cell"),
                   chrs=c("seqnames", "chr", "chrom"), fill=c("copy.number", "ploidy"),
                   cluster=NULL, max_copies=8) {
    if (!is.data.frame(segs))
        segs = .extract_aneuHMM(segs)

    msg = c()
    if (length(sample) > 1) {
        sample = intersect(sample, c(colnames(segs), "."))[1]
        msg = c(msg, paste(sQuote(sample), "(sample)"))
    }
    if (length(cell) > 1) {
        cell = intersect(cell, colnames(segs))[1]
        msg = c(msg, paste(sQuote(cell), "(cell)"))
    }
    if (length(chrs) > 1) {
        chrs = intersect(chrs, colnames(segs))[1]
        msg = c(msg, paste(sQuote(chrs), "(chrs)"))
    }
    if (length(fill) > 1) {
        fill = intersect(fill, colnames(segs))[1]
        msg = c(msg, paste(sQuote(fill), "(fill)"))
    }
    if (length(msg) > 0)
        message("[plot/genome/heatmap] using ", paste(msg, collapse=", "))

    if (!is.factor(segs[[fill]])) {
        segs[[fill]] = factor(segs[[fill]])
        # cluster here
    }
    segs[[fill]] = factor(pmin(as.integer(as.character(segs[[fill]])), max_copies))
    levels(segs[[fill]]) = sub(max_copies, paste0(max_copies, "+"), levels(segs[[fill]]))
    sc = rlang::sym(cell)

    ggplot(segs) +
        geom_rect(aes(xmin=start/1e6, xmax=end/1e6,
                      ymin=as.integer(!!sc)-0.5, ymax=as.integer(!!sc)+0.5,
                      fill=copy.number), na.rm=TRUE) +
        facet_grid(paste(sample, "~ seqnames"), scales="free", space="free", switch="y") +
        scale_fill_manual(values=cnacol, na.value="white") +
        guides(fill = guide_legend(override.aes=list(size=3))) +
        theme_classic() +
        theme(panel.spacing = unit(0.1, "lines"),
              panel.background = element_rect(fill = NA),
              panel.grid.minor.y = element_line(colour="white", size=0.2),
              panel.ontop = TRUE,
              axis.text.x = element_text(angle=60, hjust=1),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              strip.background.y = element_blank(),
              strip.placement = "outside",
              panel.spacing.y=unit(0.3, "lines")) +
        scale_x_continuous(expand=c(0,0), name="Position (Mb)", breaks=c(50,100,150)) +
        scale_y_continuous(expand=c(0,0), breaks=seq_along(levels(segs[[cell]])),
                           labels=levels(segs[[cell]]), trans=scales::reverse_trans())
}
