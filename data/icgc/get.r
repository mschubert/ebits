.b = import('base')
.ar = import('array')
.io = import('io')
.h = import('./helpers')
.p = import('../path')
.n = import('./names')

.icgc_data_dir = .p$path('icgc')

#' Returns a list containing row- and column names for clinical data
clinical = function() .h$getRData('.clinical', 'clinical.RData')

#' Returns a list containing row- and column names for clinical sample data
clinical_sample = function() .h$getRData('.clinicalsample', 'clinicalsample.RData')

#' Finds identifiers where data is available
#'
#' @param rna_seq   RNA sequencing data [T/F]
#' @param rppa      RPPA protein data [T/F]
#' @param clinical  Clinical data [T/F]
#' @param map_to    Which identifers to map to ('specimen' or 'donor')
#' @return          List of identifiers where all data types specified are available
available = function(clinical=NULL, rna_seq=NULL, rppa=NULL, map_to="specimen") {
    valid = list()
    if (grepl("specimen", map_to))
        to = "icgc_specimen_id"
    else if (grepl("donor", map_to))
        to = "icgc_donor_id"
    else
        stop("invalid map_to, need to be 'specimen' or 'donor'")

    if (!is.null(clinical))
        valid$clinical = getClinical()[[to]]
    if (!is.null(rna_seq))
        valid$RNA = .h$idmap(.n$rna_seq()[[1]], from="icgc_sample_id", to=to)[,2]
    if (!is.null(rppa))
        valid$rppa = .h$idmap(.n$rppa()[[1]], from="icgc_sample_id", to=to)[,2]

    do.call(.b$intersect, valid)
}

#' Function to retrieve the RNA seq data from the processed ICGC object
#'
#' Can do subsetting using either `index`, `samples`, `specimen`, or `donors`.
#'
#' @param index        HDF5 index, either numerical or dimension names
#' @param samples      ICGC sample ids
#' @param specimen     ICGC specimen ids
#' @param donors       ICGC donor ids
#' @param raw.counts   Get the raw counts (as opposed to normalized)
#' @param map.samples  character vector to map identifiers to: 'sample', 
#'                     'specimen', 'donor' [default: same as requested identifiers]
#' @param tissues      Get only certain (TCGA) tissues
#' @return             The requested sample matrix
rna_seq = function(index=NULL, samples=NULL, specimen=NULL, donors=NULL,
                   raw.counts=FALSE, voom=FALSE, map.ids=TRUE) {
    args = list(index=index, samples=samples, specimen=specimen,
                donors=donors, map.ids=map.ids)
    args$valid = .n$rna_seq()[[1]]

    if (voom)
        args$fname = "expr_seq_voom"
    else if (raw.counts)
        args$fname = "expr_seq_raw"
    else
        args$fname = "expr_seq_norm"

    do.call(.h$getHDF5, args)
}

#' Function to retrieve the RPPA protein data from the processed ICGC object
#'
#' Can do subsetting using either `index`, `samples`, `specimen`, or `donors`.
#'
#' @param index        HDF5 index, either numerical or dimension names
#' @param samples      ICGC sample ids
#' @param specimen     ICGC specimen ids
#' @param donors       ICGC donor ids
#' @param map.samples  character vector to map identifiers to: 'sample', 
#'                     'specimen', 'donor' [default: same as requested identifiers]
#' @return             The requested sample matrix
rppa = function(index=NULL, samples=NULL, specimen=NULL, donors=NULL, map.ids=TRUE) {
    args = list(index=index, samples=samples, specimen=specimen,
                donors=donors, map.ids=map.ids)

    args$valid = .n$rppa()[[1]]
    args$fname = "protein"

    do.call(.h$getHDF5, args)
}

mutation_types = function(bits=FALSE) {
    tab = io$read_table(io$file_path(module_file(), 'mutation_types.txt'), header=T, sep="\t")
    if (bits)
        tab
    else
        tab$consequence_type
}

#getMutations = function(types=getMutationTypes()) {
## convert to logical matrix here
#    bits = getMutationTypes(bits=TRUE)
#    # load matrix here
#    mat = ...
#    mat[] = bitwAnd(mat, )
#}
