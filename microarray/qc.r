#' Quality control function
#'
#' @param rawData  An `oligo::batch` object
#' @param NUSE     tolerance of meadian NUSE score around 1, set to NA for no QC
#' @param RLE      tolerance of meadian RLE score around 0, set to NA for no QC
#' @return         `rawData` filtered for QC criteria
qc = function(rawData, NUSE=0.1, RLE=0.1) {
    plmFit = oligo::fitProbeLevelModel(rawData, target='core')

    discard = F
    if (!is.na(NUSE.tol)) {
        med = apply(oligo::NUSE(plmFit, type="values"), 2, function(x) median(x, na.rm=T))
        discard = discard | med > 1+NUSE.tol | med < 1-NUSE.tol
    }
    if (!is.na(RLE.tol)) {
        med = apply(oligo::RLE(plmFit, type="values"), 2, function(x) median(x, na.rm=T))
        discard = discard | med > RLE.tol | med < -RLE.tol
    }
    if (any(discard)) {
        warning(paste("Discarding", sum(discard), "arrays"))
        rawData = rawData[,!discard]
    }
    rawData
}
