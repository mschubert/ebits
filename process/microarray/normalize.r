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
        expr = frma::frma(rawData, target="core")
    else
        stop("invalid method")
}

normalize.EListRaw = function(rawData, method="quantile") {
    rawData %>%
        limma::backgroundCorrect() %>%
        limma::normalizeBetweenArrays(method=method) %>%
        limma::avereps(., ID=.$genes$ProbeName)
}

normalize.RGList = function(rawData, method="loess") {
    rawData %>%
        limma::backgroundCorrect() %>%
        limma::normalizeWithinArrays(method=method) %>%
        limma::avereps(., ID=.$genes$ProbeName)
}

normalize.NChannelSet = function(rawData, ...) {
    ad = as.list(Biobase::assayData(rawData))
    ad$genes = Biobase::fData(rawData)

    if ("E" %in% names(ad)) # one-color
        norm = normalize.EListRaw(new("EListRaw", ad), ...)
    else # two-color
        norm = normalize.RGList(new("RGList", ad), ...)

    Biobase::fData(rawData) = norm$genes
    norm$genes = NULL
    #FIXME: the next line doesn't quite work
    Biobase::assayData(rawData) = new("list", norm)
    rawData
}
