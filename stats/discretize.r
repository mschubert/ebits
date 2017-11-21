mad = function(x, constant=1.5, levels=c("low", "mid", "high")) {
    med = median(x, na.rm=TRUE)
    delta = mad(x, constant=constant, na.rm=TRUE)
    re = factor(cut(x, c(-Inf, med-delta, med+delta, Inf)))
    levels(re) = levels
    re
}

quartile = function(x, levels=c("low", "mid", "high")) {
    re = factor(cut(x, c(-Inf, quantile(x), Inf)))
    levels(re) = c(levels[1], levels, levels[length(levels)])
    re
}

sd = function(x, levels=c("low", "mid", "high")) {
    med = median(x, na.rm=TRUE)
    sd = sd(x, na.rm=TRUE)
    re = factor(cut(x, c(-Inf, med-sd, med+sd, Inf)))
    levels(re) = levels
    re
}
