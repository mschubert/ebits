.b = import('../base')

overlap_normals = function(mu1, sd1, mu2, sd2) {
    min.f1f2 = function(x, mu1, mu2, sd1, sd2) {
        f1 = dnorm(x, mean=mu1, sd=sd1)
        f2 = dnorm(x, mean=mu2, sd=sd2)
        pmin(f1, f2)
    }

    integrate(min.f1f2, -Inf, Inf, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2)$value %catch% NA
}

median_test = function(x,y) {
    z = c(x,y)
    g = rep(1:2, c(length(x),length(y)))
    m = median(z)
    fisher.test(z<m,g)$p.value
}

#' Scales a vector setting median=0 and sd=1
#'
#' @param x          The vector
#' @param ref_index  Indices to use to derive median/sd
#' @return           A scaled vector
median_scale = function(x, ref_index=TRUE) {
    ref = x[ref_index]
    (x - median(ref)) / sd(ref)
}
