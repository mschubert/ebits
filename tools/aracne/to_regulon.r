#' Convert result to regulon class
#'
#' @param df  data.frame with fields: Regulator, Target, MI, pvalue
#' @return  object of class regulon
to_regulon = function(df) {
    ll = lapply(split(df, df$Regulator), function(x)
                list(tfmode=setNames(x$MI, x$Target), likelihood=1-x$pvalue))
    class(ll) = "regulon"
    ll
}
