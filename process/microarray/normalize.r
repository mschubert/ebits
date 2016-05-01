.b = import_('../../base')

#' Function to normalize expression objects
#'
#' @param rawData  A raw data object, or a list thereof
#' @param method   Method to use for normalization: rma, gcrma, frma
#' @return         The normalized data
normalize = function(rawData, ...) {
    UseMethod("normalize")
}

normalize.list = function(rawData, ...) {
    re = lapply(rawData, function(x) normalize(x, ...) %catch% NA)
    if (any(is.na(re)))
        warning("dropping ", names(re)[is.na(re)])
    if (all(is.na(re)))
        stop("all normalizations failed")
    re[!is.na(re)]
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
        frma::frma(rawData, target="core")
    else
        stop("invalid method")
}

normalize.EListRaw = function(rawData, method="quantile") {
    rawData %>%
        limma::backgroundCorrect(method="normexp") %>%
        limma::normalizeBetweenArrays(method=method)
}

normalize.RGList = function(rawData, method="loess") {
    rawData %>%
        limma::backgroundCorrect(method="normexp") %>%
        limma::normalizeWithinArrays(method=method)
}

normalize.NChannelSet = function(rawData, ...) {
    ad = as.list(Biobase::assayData(rawData))
    ad$genes = Biobase::fData(rawData)

    if ("E" %in% names(ad)) { # one-color
        norm = normalize.EListRaw(new("EListRaw", ad), ...)
        ad = with(norm, Biobase::assayDataNew(E = E))
    } else { # two-color
        norm = normalize.RGList(new("RGList", ad), ...)
        ad = with(norm, Biobase::assayDataNew(R = R, G = G))
    }

    features = new("AnnotatedDataFrame", norm$genes)
    normeset = new("NChannelSet", assayData = ad)
    featureData(normeset) = features
    phenoData(normeset) = phenoData(rawData)
    normeset
}
