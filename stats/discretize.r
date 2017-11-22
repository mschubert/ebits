mad = function(x, constant=1.5, levels=c("low", "mid", "high")) {
    med = median(x, na.rm=TRUE)
    delta = stats::mad(x, constant=constant, na.rm=TRUE)
    re = factor(cut(x, c(-Inf, med-delta, med+delta, Inf)))
    levels(re) = levels
    re
}

quantile = function(x, levels=c("low", "mid", "mid", "high")) {
    probs = c(0, seq_along(levels)/length(levels))
    re = factor(cut(x, c(-Inf, stats::quantile(x, probs=probs, na.rm=TRUE))))
    levels(re) = c(levels[1], levels, levels[length(levels)])
    re
}

sd = function(x, constant=1, levels=c("low", "mid", "high")) {
    med = median(x, na.rm=TRUE)
    sd = stats::sd(x, na.rm=TRUE) * constant
    re = factor(cut(x, c(-Inf, med-sd, med+sd, Inf)))
    levels(re) = levels
    re
}
