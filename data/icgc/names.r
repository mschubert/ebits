.io = import('io')
.p = import('../path')

.icgc_data_dir = .p$path('icgc')

.clinical = NULL
.clinicalsample = NULL

.names_expr = NULL
.names_mut = NULL
.names_protein = NULL

#' Helper function to get the names of an HDF5 object
#'
#' @param var   The variable name to cache to, i.e. `.names_(expr|mut|protein)`
#' @param file  The HDF5 file name
#' @return      A list containing the `dimnames`
.names = function(var, file) {
    re = base::get(var, envir=parent.env(environment()))
    if (is.null(re)) {
        re = h5store::h5names(.io$file_path(.icgc_data_dir, file), path="/")
        assign(var, re, envir=parent.env(environment()))
    }
    re
}

#' Returns a list containing row- and column names for RNA seq data
rna_seq = function() .names('.names_expr', 'expr_seq_norm.h5')

#' Returns a list containing row- and column names for mutational data
mutations = function() .names('.names_mut', 'mutations.h5')

#' Returns a list containing row- and column names for RPPA data
rppa = function() .names('.names_protein', 'protein.h5')
