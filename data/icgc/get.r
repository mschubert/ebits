.b = import('base')
.ar = import('array')
.io = import('io')
.i = import('./indexing')
.p = import('../path')

.icgc_data_dir = .p$path('icgc')

#' Returns a list containing row- and column names for clinical data
#TODO: give index=, infer id type from it
clinical = function(specimen=NULL, donors=NULL) {
    re = .i$getRData('clinical.RData')

    if (!is.null(donors))
        re[match(donors, re$icgc_donor_id),]
    else if (!is.null(specimen))
        re[match(specimen, re$icgc_specimen_id),]
    else
        re
}

#' Returns a list containing row- and column names for clinical sample data
clinical_sample = function() .i$getRData('clinicalsample.RData')

#' Finds identifiers where data is available
#'
#' @param rna_seq   RNA sequencing data [T/F]
#' @param rppa      RPPA protein data [T/F]
#' @param clinical  Clinical data [T/F]
#' @param map_to    Which identifers to map to ('specimen' or 'donor')
#' @return          List of identifiers where all data types specified are available
available = function(clinical=NULL, rna_seq=NULL, rppa=NULL, mutations=NULL, map_to="icgc_specimen_id") {
    valid = list()
    if (grepl("specimen", map_to))
        to = "icgc_specimen_id"
    else if (grepl("donor", map_to))
        to = "icgc_donor_id"
    else
        stop("invalid map_to, need to be 'specimen' or 'donor'")

    if (!is.null(clinical))
        valid$clinical = clinical()[[to]]
    if (!is.null(rna_seq))
        valid$rna_seq = .b$match(.i$getNames('expr_seq_norm')[[1]],
                                 from = "icgc_sample_id",
                                 to = to,
                                 data = clinical_sample())
    if (!is.null(rppa))
        valid$rppa = .b$match(.i$getNames('protein')[[1]],
                              from = "icgc_sample_id",
                              to = to,
                              data = clinical_sample())

    if (!is.null(mutations))
        valid$mut = .b$match(.i$getNames('mutations')[[1]],
                             from = "icgc_sample_id",
                             to = to,
                             data = clinical_sample())

    do.call(.b$intersect, valid)
}

#' Function to retrieve the RNA seq data from the processed ICGC object
#'
#' @param index       HDF5 index, either numerical or character;
#'                    If a character vector ICGC ids will be matched:
#'                    SA*: sample ID, SP*: specimen ID, SD*: donor ID
#' @param raw_counts  Get the raw counts (as opposed to normalized)
#' @param map_ids     character vector to map identifiers to: 'icgc_sample_id', 
#'                     'icgc_specimen_id', 'donor_id' [default: same as requested identifiers]
#' @return             The requested sample matrix
rna_seq = function(index=available(rna_seq=TRUE), raw_counts=FALSE, voom=FALSE, map_ids=TRUE) {
    if (voom)
        fname = "expr_seq_voom"
    else if (raw_counts)
        fname = "expr_seq_raw"
    else
        fname = "expr_seq_norm"

    .i$getHDF5(index=index, map_ids=map_ids, fname=fname)
}

#' Function to retrieve the RPPA protein data from the processed ICGC object
#'
#' @param index       HDF5 index, either numerical or character;
#'                    If a character vector ICGC ids will be matched:
#'                    SA*: sample ID, SP*: specimen ID, SD*: donor ID
#' @param map_ids     character vector to map identifiers to: 'icgc_sample_id', 
#' @return             The requested sample matrix
rppa = function(index=available(rppa=TRUE), map_ids=TRUE) {
    .i$getHDF5(index=index, map_ids=map_ids, fname="protein")
}

#' Function to retrieve the mutation data from the processed ICGC object
#'
#' Can do subsetting using either `index`, `samples`, `specimen`, or `donors`.
#'
#' @param index    HDF5 index, either numerical or character;
#'                 If a character vector ICGC ids will be matched:
#'                 SA*: sample ID, SP*: specimen ID, SD*: donor ID
#' @param map_ids  character vector to map identifiers to: 'icgc_sample_id', 
#' @param minN     Minimum number of mutated genes to be included
#' @return         The requested sample matrix
mutations = function(index, map_ids=TRUE, minN=0) {
    re = .i$getHDF5(index=index, map_ids=map_ids, fname="mutations")
    re[rowSums(re) > minN,]
}
