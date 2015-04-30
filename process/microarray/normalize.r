#' Function to normalize expression objects
#'
#' @param rawData  A raw data object, or a list thereof
#' @param method   Method to use for normalization: rma, gcrma, frma
#' @return         The normalized data
normalize = function(rawData, method="rma") {
    UseMethod("normalize")
}

normalize.list = function(rawData, method="rma") {
    lapply(rawData, normalize)
}

normalize.FeatureSet = function(rawData, method="rma") {
    if (method == "rma")
        oligo::rma(rawData)
    else
        stop("invalid method")
}

normalize.AffyBatch = function(rawData, method="rma") {
    if (method == "rma")
        affy::rma(rawData)
    else if (method == "gcrma")
        gcrma::gcrma(rawData)
    else if (method == "frma")
        expr = frma::frma(rawData, target="core")
    else
        stop("invalid method")
}
