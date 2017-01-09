#' Converts a correlation metric (rho) to a z-score
#'
#' @param rho  A Pearson's r
#' @return     A z-score
fisher_r2z = function(rho) 0.5 * log((1 + rho)/(1 - rho))

#' Tests for significant correlations in a population
#'
#' @param mat  A sample matrix with [obs x samples]
#' @return     A matrix with p-values
test = function(mat, self=0) {
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
#' TODO: all stat tests should return a data.frame
#'
#' @param x  A sample matrix with [obs x variables]
#' @param y  A samples matrix with [obs x variables]
#' @return   A symmetric matrix of p-values [variables x variables]
diff_test = function(x, y) {
    #TODO: move this somewhere proper (array?)
    rep_row = function(x,n){
        matrix(rep(x,each=n),nrow=n)
    }
    rep_col = function(x,n){
        matrix(rep(x,each=n), ncol=n, byrow=TRUE)
    }

    corx = cor(x)
    cory = cor(y)
    delta_cor = cor(y) - cor(x)

    za = fisher_r2z(corx)
    zb = fisher_r2z(cory)

    na = rep_col(apply(x, 2, function(x) sum(!is.na(x))), ncol(x))
    nb = rep_row(apply(y, 2, function(x) sum(!is.na(x))), ncol(y))

    se = sqrt((1/(na-3))+(1/(nb-3)))
    z = (za-zb)/se

    pval = 2*pnorm(-abs(z))

    list(delta_cor = delta_cor, p.value = pval)
}
