.p = import('../path')
.io = import('../../io')

#' List all available ID types
id_types = c("patient", "specimen", "full")
.id_lengths = c(12, 16, 100)

.make_id = function(obj, id_type, along=2) {
    slen = setNames(.id_lengths, id_types)[id_type]
    dimnames(obj)[[along]] = substr(dimnames(obj)[[along]], 1, slen)
    obj
}

#' List all available tissues
tissues = function(id_type="specimen") {
    tt = list.files(.p$path("tcga"), pattern="_voom\\.RData")
    sub("_voom.RData", "", tt)
}

#' Get a data.frame listing all clinical data available
#'
#' @param tissue   Limit selection to a single/set of tissues
#' @param id_type  Where to cut the barcode, either "patient", "specimen", or "full"
#' @return         A data.frame with data for all the clinical data
clinical = function(tissue=NULL, id_type=NULL) {
    re = .p$load("tcga", "clinical_full.RData")
    if (!is.null(tissue))
        re = dplyr::filter(re, study %in% tissue)
    if (!is.null(id_type))
        rownames(re) = substr(re$Tumor_Sample_Barcode, 1,
                              setNames(.id_lengths, id_types)[id_type])
    re
}

#' Get a matrix for all RNA-seq measurements
#'
#' @param tissue   The tissue to get expression for
#' @param id_type  Where to cut the barcode, either "patient", "specimen", or "full"
#' @return         A matrix with HGNC symbols x TCGA samples
rna_seq = function(tissue, id_type="specimen") {
    .make_id(.p$load("tcga", paste0(tissue, "_voom.RData")), id_type)
}

#' Get a matrix for all RPPA measurements
#'
#' @param id_type  Where to cut the barcode, either "patient", "specimen", or "full"
#' @return         A matrix with antibodies x TCGA samples
rppa = function(id_type="specimen") {
    .make_id(.p$load("tcga", "rppa.RData"), id_type)
}

#' Get a data.frame listing all mutations and types
#'
#' @param id_type  Where to cut the barcode, either "patient", "specimen", or "full"
#' @return         A data.frame with data for all the simple mutations
mutations = function(id_type="specimen") {
    .p$load("tcga", "mutations.RData")
}
