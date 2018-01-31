#' Filter peaks that do not pass a posterior probability threshold
#'
#' @param model  Chromstar model object
#' @param pp     Posterior probability cutoff
filter_peaks = function(model, pp=1e-4) {
    if (is.list(model) && !grepl("HMM", class(model)))
        lapply(model, chromstaR::changeFDR, fdr=pp)
    else
        chromstaR::changeFDR(model, fdr=pp)
}
