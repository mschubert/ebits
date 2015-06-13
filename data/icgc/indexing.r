.b = import('base')
.io = import('io')
.p = import('../path')

.icgc_data_dir = .p$path('icgc')
.icgc_raw_dir = .p$file('icgc', 'version_17')
.data_cache = list()
.name_cache = list()

#' Helper function to get the names of an HDF5 object
#'
#' @param file  The HDF5 file name
#' @return      A list containing the `dimnames`
getNames = function(file) {
    re = .name_cache[[file]]
    if (is.null(re)) {
        re = h5store::h5names(.io$file_path(.icgc_data_dir, file, ext=".h5"), path="/")
        .name_cache[[file]] = re
    }
    re
}

#' Helper function to get ICGC RData contents
#'
#' @param file  The file name, either 'clinical.RData' or 'clinicalsample.RData'
#' @return      A data.frame containing the file contents
getRData = function(file) {
    re = .data_cache[[file]]
    if (is.null(re)) {
        re = .io$load(.io$file_path(.icgc_data_dir, file))
        .data_cache[[file]] = re
    }
    re
}

#' Helper function to get ICGC HDF5 objects
#'
#' @param fname    The file name
#' @param index    Indexing along rows (SA*, SP*, D*, or numeric)
#' @param map_ids  Map IDs to: icgc_sample_id, icgc_specimen_id, icgc_donor_id;
#'                 default: TRUE, same as requested IDs
#' @return         A matrix with mapped IDs
getHDF5 = function(fname, index, map_ids=TRUE) {
    valid = getNames(fname)[[1]]
    mapping = varmap(valid=valid, map_ids=map_ids, filter_to=index)

    if (all(is.na(mapping)))
        stop("none of requested identifiers available")

    mapping = mapping[!is.na(mapping)]
    ndrop = length(unique(index)) - length(unique(mapping))
    if (ndrop != 0)
        warning("dropping ", ndrop, " identifiers (not avail)")

    re = t(h5store::h5load(.io$file_path(.icgc_data_dir, fname, ext=".h5"),
                index=names(mapping)))
    colnames(re) = unname(mapping)
    re
}

#' Function to map the given variables to an HDF5 index and name mappings
#'
#' @param index      Numeric, or character vector of IDs
#' @param map_ids    Type of IDs to map to, default: same as requested
#' @param filter_to  Set of IDs to map to
#' @return           List with file index and mapped column names
varmap = function(valid, map_ids=TRUE, filter_to=NULL) {
    if (is.character(map_ids))
        to = map.ids
    else if (identical(map_ids, FALSE) || all(grepl("^SA", filter_to)))
        to = "icgc_sample_id"
    else if (all(grepl("^SP", filter_to)))
        to = "icgc_specimen_id"
    else if (all(grepl("^D", filter_to)))
        to = "icgc_donor_id"
    else
        stop("invalid ids - all must start with SA, D, or SP")

    .b$match(x = valid,
             from = "icgc_sample_id",
             to = to,
             filter_to = filter_to,
             data = getRData('clinicalsample.RData'))
}
