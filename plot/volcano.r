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
        text.size=3.5, xlim=NULL, ylim=NULL, simplify=TRUE, repel=FALSE,
        x_label_bias=1, pos_label_bias=1) {
    if (nrow(df) == 0)
        stop("No observations to plot")
    if (!'label' %in% colnames(df))
        stop("Column 'label' not found. You need to specify a label for your points")
    if (!'color' %in% colnames(df))
        stop("Column 'color' not found. Did you call plt$color$...?")
    if (!'circle' %in% colnames(df))
        df$circle = FALSE
    if (!'fill' %in% colnames(df))
        df$fill = TRUE

    # workaround display bugs for very small p-values
    ylab = "Adjusted p-value (FDR)"
    df$.y = pmax(df$.y, 1e-300)
    if (any(df$.y <= 1e-300, na.rm=TRUE)) {
        df$.y = 2^log10(pmin(df$.y, 300))
        ylab = "Pseudo p-value (values too close to zero)"
    }

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
    rel_effect = df$.x / abs(df$.x[rank(-abs(df$.x), ties.method="first") == 10])
    rel_effect[is.na(rel_effect)] = 0
    rel_pval = log10(df$.y) / log10(df$.y[rank(df$.y, ties.method="first") == 10])
    point_dist = abs(rel_effect) * x_label_bias + abs(rel_pval)
    point_dist[rel_effect > 0] = point_dist[rel_effect > 0] * pos_label_bias
    point_dist[df$.y > p] = NA
    point_dist[is.na(df$label)] = NA # only keep points where we have labels
    df$label[rank(-point_dist) > label_top & xor(df$fill, df$circle)] = NA

    # make sure we don't plot too many insignificant points
    if (simplify && sum(df$.y > p, na.rm=TRUE) > 800) {
        set.seed(123456)
        idx = which(df$.y >= .b$minN(df$.y[df$.y > p], 100))
        prob = 1 - df$.y[idx]+.Machine$double.eps*2
        prob[df$circle[idx]] = 1
        keep = sample(idx, size=500, replace=FALSE, prob=prob)
        df$.y[setdiff(idx, keep)] = NA
        df$label[idx] = NA
    }

    x = p # no idea why this is required
    breaks_with_thresh = function(...) c(x, scales::log_breaks(base=10)(df$.y, 5))

    # and do the actual plot
    df = dplyr::arrange(df, -.y)
    p = ggplot(df, aes(x = .x, y = .y)) + 
        scale_y_continuous(trans = reverselog_trans(base=10),
                           label = scientific_10,
                           limits = ylim,
                           breaks = breaks_with_thresh) +
        scale_x_continuous(limits = xlim) +
        geom_point(size = ifelse(df$fill, sqrt(df$size), NA), color=df$color, na.rm=TRUE) +
        geom_point(size = ifelse(df$circle, sqrt(df$size), NA),
                   shape=1, color=ifelse(df$fill, '#00000088', df$color), na.rm=TRUE) +
        geom_vline(xintercept=0, color="#858585") +
        geom_hline(yintercept=p, linetype = "dashed", color="#858585") +
        xlab("Effect size") + 
        ylab(ylab) +
        theme_classic()

    if (repel)
        p + ggrepel::geom_text_repel(mapping = aes(x = .x, y = .y, label = label),
                colour= "#353535", size = text.size, na.rm = TRUE, segment.alpha=0.2, max.iter=5e4)
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
    testthat::expect_equal(df, p$data)
}
