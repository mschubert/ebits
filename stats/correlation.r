#' Converts a correlation metric (rho) to a z-score
#'
#' @param rho  A Pearson's r
#' @return     A z-score
fisher_r2z = function(rho) 0.5 * log((1 + rho)/(1 - rho))

#' Tests for significant correlations in a population
#'
#' @param mat  A sample matrix with [obs x samples]
#' @return     A matrix with p-values
cor_test = function(mat, self=0) {
    mat = as.matrix(mat)
    n = ncol(mat)
    p.mat = matrix(NA, n, n)
    diag(p.mat) = self
    for (i in 1:(n - 1))
        for (j in (i + 1):n)
            p.mat[i,j] = p.mat[j,i] = cor.test(mat[,i], mat[,j])$p.value
    p.mat
}

#' Tests for significant changes in correlation between two populations
#'
#' @param x  A sample matrix with [obs x samples]
#' @param y  A samples matrix with [obs x samples]
#' @return     A matrix with p-values
cor_diff_test = function(x, y) {
    #TODO: move this somewhere proper
    rep_row = function(x,n){
        matrix(rep(x,each=n),nrow=n)
    }
    rep_col = function(x,n){
        matrix(rep(x,each=n), ncol=n, byrow=TRUE)
    }

    za = fisher_r2z(cor(x))
    zb = fisher_r2z(cor(y))

    na = rep_col(apply(x, 2, function(x) sum(!is.na(x))), ncol(x))
    nb = rep_row(apply(y, 2, function(x) sum(!is.na(x))), ncol(y))

    se = sqrt((1/(na-3))+(1/(nb-3)))
    z = (za-zb)/se

    2*pnorm(-abs(z))
}
