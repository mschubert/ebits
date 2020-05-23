.scales = import_package_('scales')

reverselog_trans = function(base=exp(1)) {
    trans = function(x) -log(x, base)
    inv = function(x) base^-x
    .scales$trans_new(paste0("log-", format(base)),
                      trans,
                      inv,
                      .scales$log_breaks(base = base), 
                      domain = c(1e-100, Inf))
}

# function for relabeling the y-axis
scientific_10 = function(x) {
    fmt = ifelse(x < 0.01, .scales$scientific_format()(x), x)
    parse(text=gsub("1e", "10^", fmt))
}

# http://stackoverflow.com/questions/6738085
fill_brewer = function(fill, palette) {
    n = RColorBrewer::brewer.pal.info$maxcolors[palette == rownames(brewer.pal.info)]
    discrete.fill = call("quantcut", match.call()$fill, q=seq(0, 1, length.out=n))
    list(
        do.call(aes, list(fill=discrete.fill)),
        RColorBrewer::scale_fill_brewer(palette=palette)
    )
}

discretize = function(x, discr, lim=c(NA,NA)) {
    lower = lim[1]
    upper = lim[2]
    if (is.na(lower))
        lower = min(x, na.rm=T)
    if (is.na(upper))
        upper = max(x, na.rm=T)

    breaks = seq(lower, upper, (upper-lower)/length(discr))
    discr[as.integer(cut(x, breaks=breaks, include.lowest=T))]
}

