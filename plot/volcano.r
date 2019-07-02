library(ggrepel)
.b = import('../base')
import('./helpers', attach=TRUE)
color = import('./color')

#' Draw a volcano plot from calculated associations
#'
#' @param df         A data.frame obtained from the stats module.
#'                   It has to have the following fields:
#'                     .x    : x coordinate in the plot
#'                     .y    : y coordinate in the plot
#'                     size  : size of the filled circle
#'                   Optionally, it can have the fields:
#'                     circle: draw a border around the circle [TRUE/FALSE]
#' @param base.size  Scaling factor for the points drawn
#' @param p          Line between significant and insignificant associations
#' @param ceil       Minimum p-value to set top associations to; default: 0, no filter
#' @param check_overlap  Don't print overlapping labels in geom_text()
#' @param xlim       Limits along the horizontal axis; default: fit data
#' @param ylim       Limits along the vertical axis; default: fit data
#' @param simplify   Drop some insignificant points and labels to reduce file size
#' @param repel      Whether to use repel package for labels
#' @param x_label_bias   Multiplier to focus more on effect size than significance
#' @param pos_label_bias Multiplier to focus more on positive than negative labels
#' @return           A ggplot2 object of the volcano plot
volcano = function(df, base.size=1, p=0.05, label_top=20, ceil=0, check_overlap=FALSE,
        text.size=3.5, xlim=c(NA,NA), ylim=c(NA,NA), simplify=TRUE, repel=FALSE,
        x_label_bias=1, pos_label_bias=1) {
    if (nrow(df) == 0)
        stop("No observations to plot")
    if (!'label' %in% colnames(df))
        stop("Column 'label' not found. You need to specify a label for your points")
    if (!'color' %in% colnames(df))
        stop("Column 'color' not found. Did you call plt$color$...?")
    if (!'circle' %in% colnames(df))
        df$circle = FALSE

    # remove insignificant points outside x limits, adjust size
    if (any(df$.y < p, na.rm=TRUE))
        df = dplyr::filter(df, .y < p |
                           abs(.x)<max(abs(.x[.y<p]),
                           na.rm=TRUE))

    df = dplyr::mutate(df, size = size*base.size)

    # set very low p-values to the cutoff value and label point
    # .Machine$double.eps too large, first != 0 too small
    if (ceil == 0 && any(df$.y == 0, na.rm=TRUE))
        ceil = min(df$.y[df$.y > 1e-300], na.rm=TRUE)
    pmin = df$.y < ceil
    if (any(pmin, na.rm=TRUE)) {
        df[pmin,] = mutate(df[pmin,],
            label = paste0(label, " (p < 1e", ceiling(log10(.y)), ")"),
            .y = ceil)
    }

    # filter labels, but only for points we don't highlight
    rel_effect = df$.x/max(abs(df$.x), na.rm=TRUE)
    rel_pval = 2 * log10(df$.y) / min(log10(df$.y), na.rm=TRUE)
    point_dist = rel_effect^2 * x_label_bias + rel_pval^2
    point_dist[rel_effect > 0] = point_dist[rel_effect > 0] * pos_label_bias
    point_dist[is.na(df$label)] = NA # only keep points where we have labels
    df$label[rank(-point_dist) > label_top & !df$circle] = NA

    # make sure we don't plot too many insignificant points
    if (simplify && sum(df$.y > p, na.rm=TRUE) > 300) {
        set.seed(123456)
        idx = which(df$.y >= .b$minN(df$.y[df$.y > p], 100))
        prob = 1 - df$.y[idx]+.Machine$double.eps*2
        prob[df$circle[idx]] = 1
        keep = sample(idx, size=200, replace=FALSE, prob=prob)
        df$.y[setdiff(idx, keep)] = NA
        df$label[idx] = NA
    }

    # and do the actual plot
    p = ggplot(df, aes(x = .x, y = .y)) + 
        scale_y_continuous(trans = reverselog_trans(10),
                           label = scientific_10,
                           limits = ylim) +
        scale_x_continuous(limits = xlim) +
        geom_point(size = sqrt(df$size), colour = df$color, na.rm = TRUE) +
        geom_point(size = ifelse(df$circle, sqrt(df$size), NA), shape=1, colour = '#00000088', na.rm = TRUE) +
        geom_vline(xintercept = 0, lwd = 0.3) +
        geom_hline(yintercept = p, lwd = 0.3, linetype = 2) +
#        annotate("text", x=min(df$.x), y=0.05, hjust=1, vjust=2, 
#                 size=3.5, label="0.05", colour="black") +
        xlab("Effect size") + 
        ylab("Adjusted P-value") +
        theme_bw()

    if (repel)
        p + ggrepel::geom_text_repel(mapping = aes(x = .x, y = .y, label = label),
                colour = "#353535", size = text.size, na.rm = TRUE, segment.alpha=0.5)
    else
        p + geom_text(mapping = aes(x = .x, y = .y, label = label),
                colour = "#353535", size = text.size, vjust = -1, na.rm = TRUE, check_overlap=check_overlap)
}

if (is.null(module_name())) {
    library(testthat)

    color = import('./color')
    df = data.frame(estimate = -12:12/12)
    df$adj.p = 10^(-10*abs(df$estimate))
    df$label = LETTERS[1:25]
    df = color$p_effect(df, "adj.p")
    df$size = 1:25
    df$circle = rep(c(T,F,T,F,T),5)

    p = volcano(df)

    df$label[df$label %in% c("L", "N")] = NA
    expect_equal(df, p$data)
}
