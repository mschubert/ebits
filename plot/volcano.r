import_package("ggplot2", attach=TRUE)
.b = import('../base')

.reverselog_trans = function(base=exp(1)) {
    scales::trans_new(paste0("log-", format(base)),
                      function(x) -log(x, base),
                      function(x) base^-x,
                      scales::log_breaks(base = base),
                      domain = c(1e-100, Inf))
}

.scientific_10 = function(x) {
    fmt = ifelse(x < 0.01, scales::scientific_format()(x), x)
    parse(text=gsub("1e", "10^", fmt))
}

#' Draw a volcano plot from calculated associations
#'
#' @param df         A data.frame obtained from the stats module.
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
#' @param max_overlaps   Parameter passed to ggrepel if repel is TRUE
#' @param x_label_bias   Multiplier to focus more on effect size than significance
#' @param pos_label_bias Multiplier to focus more on positive than negative labels
#' @return           A ggplot2 object of the volcano plot
volcano = function(df, x = c("log2FoldChange", "estimate", ".x"),
                   label = c("label", "name", "gene_name", "gene", "external_gene_name", "set_name", "set"),
                   y = c("adj.p", "padj", "p.value", "pval", ".y"),
                   size = c("size", "n", "baseMean"),
                   base.size=1, p=0.05, label_top=20, ceil=0, check_overlap=FALSE,
                   text.size="auto", xlim=NULL, ylim=NULL, simplify=TRUE, repel=TRUE, max.overlaps=20,
                   x_label_bias=1, pos_label_bias=1) {

    msg = c()
    if (length(x) > 1) {
        x = intersect(x, colnames(df))[1]
        msg = c(msg, paste(sQuote(x), "(x)"))
    }
    if (length(y) > 1) {
        y = intersect(y, colnames(df))[1]
        msg = c(msg, paste(sQuote(y), "(y)"))
    }
    if (length(label) > 1) {
        label = intersect(label, colnames(df))[1]
        msg = c(msg, paste(sQuote(label), "(label)"))
    }
    if (length(size) > 1) {
        size = intersect(size, colnames(df))[1]
        if (is.na(size)) {
            df$size = 1
            size = "size"
        }
        if (size == "baseMean") # better circle sizes
            df[[size]] = sqrt(df[[size]]) # should probably scale_size(...)
        msg = c(msg, paste(sQuote(size), "(size)"))
    }
    if (length(msg) > 0)
        message("[plot/volcano] using ", paste(msg, collapse=", "))

    sx = rlang::sym(x)
    sy = rlang::sym(y)
    sl = rlang::sym(label)

    if (nrow(df) == 0)
        stop("No observations to plot")
    if (!'circle' %in% colnames(df))
        df$circle = FALSE
    if (!'fill' %in% colnames(df))
        df$fill = TRUE
    if (is.character(text.size))
        text.size = 1.5 + 5 * 1/sqrt(mean(nchar(df[[label]])))

    df$color = rep(rgb(0, 151, 30, 120, maxColorValue=255), nrow(df)) # green
    df$color[df[[x]] < 0] = rgb(225, 0, 0, 120, maxColorValue=255) # red
    df$color[df[[y]] > p] = rgb(200, 200, 200, 120, maxColorValue=255) # grey

    # workaround display bugs for very small p-values
    ylab = waiver()
    df[[y]] = pmax(df[[y]], 1e-300)
    if (any(df[[y]] <= 1e-300, na.rm=TRUE)) {
        df[[y]] = 2^log10(pmin(df[[y]], 300))
        ylab = "Pseudo p-value (values too close to zero)"
    }

    # remove insignificant points outside x limits, adjust size
    if (any(df[[y]] < p, na.rm=TRUE))
        df = dplyr::filter(df, !! sy < p |
                           abs(!! sx) < max(abs((!! sx)[(!! sy)<p]),
                           na.rm=TRUE))

    df[[size]] = df[[size]] * base.size

    # set very low p-values to the cutoff value and label point
    # .Machine$double.eps too large, first != 0 too small
    if (ceil == 0 && any(df[[y]] == 0, na.rm=TRUE))
        ceil = min(df[[y]][df[[y]] > 1e-300], na.rm=TRUE)
    pmin = df[[y]] < ceil
    if (any(pmin, na.rm=TRUE)) {
        df[pmin,] = mutate(df[pmin,],
            !! sl := paste0(!! sl, " (p < 1e", ceiling(log10(!! sy)), ")"),
            !! sy := ceil)
    }

    # filter labels, but only for points we don't highlight
    rel_effect = df[[x]] / abs(df[[x]][rank(-abs(df[[x]]), ties.method="first") == 10])
    rel_effect[is.na(rel_effect)] = 0
    rel_pval = log10(df[[y]]) / log10(df[[y]][rank(df[[y]], ties.method="first") == 10])
    point_dist = abs(rel_effect) * x_label_bias + abs(rel_pval)
    point_dist[rel_effect > 0] = point_dist[rel_effect > 0] * pos_label_bias
    point_dist[df[[y]] > p] = NA
    point_dist[is.na(df[[label]])] = NA # only keep points where we have labels
    df[[label]][rank(-point_dist) > label_top & xor(df$fill, df$circle)] = NA

    # make sure we don't plot too many insignificant points
    if (simplify && sum(df[[y]] > p, na.rm=TRUE) > 800) {
        set.seed(123456)
        idx = which(df[[y]] >= .b$minN(df[[y]][df[[y]] > p], 100))
        prob = 1 - df[[y]][idx]+.Machine$double.eps*2
        prob[df$circle[idx]] = 1
        keep = sample(idx, size=500, replace=FALSE, prob=prob)
        df[[y]][setdiff(idx, keep)] = NA
        df[[label]][idx] = NA
    }

    breaks_with_thresh = function(...) c(p, scales::log_breaks(base=10)(df[[y]], 5))

    # and do the actual plot
    df = dplyr::arrange(df, - !! sy)
    pl = ggplot(df, aes_string(x = x, y = y)) +
        scale_y_continuous(trans = .reverselog_trans(base=10),
                           labels = .scientific_10,
                           limits = ylim,
                           breaks = breaks_with_thresh) +
        scale_x_continuous(limits = xlim) +
        geom_point(size = ifelse(df$fill, sqrt(df[[size]]), NA), color=df$color, na.rm=TRUE) +
        geom_point(size = ifelse(df$circle, sqrt(df[[size]]), NA),
                   shape=1, color=ifelse(df$fill, '#00000088', df$color), na.rm=TRUE) +
        geom_vline(xintercept=0, color="#858585") +
        geom_hline(yintercept=p, linetype="dashed", color="#858585") +
        ylab(ylab) +
        theme_classic()

    if (repel) {
        pl + ggrepel::geom_label_repel(aes_string(x=x, y=y, label=label),
                colour="#353535", size=text.size, na.rm=TRUE, segment.alpha=0.3,
                max.iter=1e5, max.time=5, max.overlaps=max.overlaps,
                label.size=NA, fill="#ffffff80",
                label.padding = unit(0.12, "lines"), box.padding = unit(0.01, "lines"))
    } else {
        pl + geom_text(aes_string(x=x, y=y, label=label), colour="#353535", size=text.size,
                      vjust=-1, na.rm=TRUE, check_overlap=check_overlap)
    }
}

if (is.null(module_name())) {
    library(testthat)

    df = data.frame(estimate = -12:12/12)
    df$adj.p = 10^(-10*abs(df$estimate))
    df$label = LETTERS[1:25]
    df$size = 1:25
    df$circle = rep(c(T,F,T,F,T),5)

    p = volcano(df)

    df$label[df$label %in% c("L", "N")] = NA
#    testthat::expect_equal(df, p$data)
}
