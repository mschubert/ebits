#' Function to normalize expression objects
#'
#' @param rawData  A raw data object
#' @param method   Method to use for normalization: rma, gcrma, frma
#' @return         The normalized data
normalize = function(rawData, method="rma") {
    if (method == "rma")
        expr = affy::rma(rawData)
#        expr = oligo::rma(rawData)
    else if (method == "gcrma") {
#        rawData = affy::ReadAffy(filenames=files)
        expr = gcrma::gcrma(rawData)
    } else if (method == "frma") {
#        if (class(rawData) != "GeneFeatureSet")
#            rawData = affy::ReadAffy(filenames=files)
        expr = frma::frma(rawData, target="core")
        colnames(expr) = fnames
    } else
        stop("invalid method")

    expr
}
