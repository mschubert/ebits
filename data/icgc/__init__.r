.b = import('base')
.ar = import('array')
.io = import('io')
h = import('./helpers')

.icgc_data_dir = getOption('icgc_data_dir') %or% stop("need option 'icgc_data_dir'")

#' Function that processes a raw data download from ICGC to .RData/hdf5 files that can be used for analysis
#'
#' @param force  overwrite existing files instead of skipping
process_raw_data = function(force=FALSE) {
    d = import('./process_data')
    tfun = function(x) mutate(x, tissue = .b$grep("^(\\w+)", project_code))

    d$df("clinical.RData", "clinical\\.", transform=tfun, force=force)
    d$df("clinicalsample.RData", "clinicalsample\\.", force=force)
    d$mat("expr_seq_raw.h5", '^exp_seq',
          raw_read_count ~ gene_id + icgc_sample_id, map.hgnc=T, force=force)
    d$mat("expr_seq_norm.h5", '^exp_seq',
          normalized_read_count ~ gene_id + icgc_sample_id, map.hgnc=T, force=force)
    d$mat("protein.h5", '^protein_expression',
          normalized_expression_level ~ antibody_id + icgc_sample_id, map.hgnc=F, force=force)

#    d$mat("cnv.h5", '^copy_number_somatic_mutation',
#          segment_median ~ gene_affected + icgc_sample_id,
#          fun.aggregate = mean, map.hgnc=T, force=force)

    mut_aggr = function(x) {
        if (length(x) == 0)
            return(0)
        if (!is.numeric(x)) {
            mtypes = getMutationTypes(bits=TRUE)
            x = mtypes$bit_code[mtypes$consequence_type %in% x]
        }
        Reduce(bitwOr, x)
    }
    d$mat('mutations.h5', '^simple_somatic',
          consequence_type ~ gene_affected + icgc_sample_id,
          fun.aggregate = length, force=force, map.hgnc=TRUE)

    voomfile = file.path(.icgc_data_dir,"expr_seq_voom.h5")
    if (identical(force, TRUE) || !file.exists(voomfile)) {
        expr = getRNASeq(raw.counts=TRUE) %>% na.omit()
        expr = limma::voom(expr)$E
        h5store::h5save(t(expr), file=voomfile)
    }
}

#' Returns a list containing row- and column names for clinical data
getClinical = function() h$getRData('.clinical', 'clinical.RData')

#' Returns a list containing row- and column names for clinical sample data
getClinicalSample = function() h$getRData('.clinicalsample', 'clinicalsample.RData')

#' Returns a list containing row- and column names for RNA seq data
namesRNASeq = function() h$names('.names_expr', 'expr_seq_norm.h5')

#' Returns a list containing row- and column names for mutational data
namesMutation = function() h$names('.names_mut', 'mutations.h5')

#' Returns a list containing row- and column names for RPPA data
namesRPPA = function() h$names('.names_protein', 'protein.h5')

#' Finds identifiers where data is available
#'
#' @param RNASeq    RNA sequencing data [T/F]
#' @param RPPA      RPPA protein data [T/F]
#' @param clinical  Clinical data [T/F]
#' @param map_to    Which identifers to map to ('specimen' or 'donor')
#' @return          List of identifiers where all data types specified are available
dataAvailable = function(clinical=NULL, RNASeq=NULL, RPPA=NULL, map_to="specimen") {
    valid = list()
    if (grepl("specimen", map_to))
        to = "icgc_specimen_id"
    else if (grepl("donor", map_to))
        to = "icgc_donor_id"
    else
        stop("invalid map_to, need to be 'specimen' or 'donor'")

    if (!is.null(clinical))
        valid$clinical = getClinical()[[to]]
    if (!is.null(RNASeq))
        valid$RNA = h$idmap(namesRNASeq()[[1]], from="icgc_sample_id", to=to)[,2]
    if (!is.null(RPPA))
        valid$RPPA = h$idmap(namesRPPA()[[1]], from="icgc_sample_id", to=to)[,2]

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
getRNASeq = function(index=NULL, samples=NULL, specimen=NULL, donors=NULL,
                     raw.counts=FALSE, voom=FALSE, map.ids=TRUE) {
    args = list(index=index, samples=samples, specimen=specimen,
                donors=donors, map.ids=map.ids)
    args$valid = namesRNASeq()[[1]]

    if (voom)
        args$fname = "expr_seq_voom"
    else if (raw.counts)
        args$fname = "expr_seq_raw"
    else
        args$fname = "expr_seq_norm"

    do.call(h$getHDF5, args)
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
getRPPA = function(index=NULL, samples=NULL, specimen=NULL, donors=NULL, map.ids=TRUE) {
    args = list(index=index, samples=samples, specimen=specimen,
                donors=donors, map.ids=map.ids)

    args$valid = namesRPPA()[[1]]
    args$fname = "protein"

    do.call(h$getHDF5, args)
}

getMutationTypes = function(bits=FALSE) {
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
