.b = import('../../base') # %or%
.io = import('../../io')
.p = import('../path')

#' Landmark probes
landmarks = as.character(read.table(file.path(module_file(),'rid_landmarks.txt'))$V1)

#' Projected probes
projected = as.character(read.table(file.path(module_file(),'rid_projected.txt'))$V1)

#' Best inferred probes
bing = as.character(read.table(file.path(module_file(),'rid_bing.txt'))$V1)

#' Returns a subset of a .gctx object as named R matrix
#'
#' @param fname  File name
#' @param rid    Vector of probe IDs
#' @param cid    Vector of experiment IDs
parse_gctx = function(fname, rid=NULL, cid=NULL) {
    rows = gsub("\\ ", "", rhdf5::h5read(fname, "/0/META/ROW/id"))
    cols = gsub("\\ ", "", rhdf5::h5read(fname, "/0/META/COL/id"))

    if (is.null(cid))
        stop("need to specify column (experiment) ids")
    else
        col_i = match(cid, cols)

    if (is.null(rid))
        row_i = 1:length(rows)
    else
        row_i = match(rid, rows)

    structure(rhdf5::h5read(fname, "/0/DATA/0/matrix", index=list(row_i, col_i)),
              .Dimnames = list(rows[row_i], cols[col_i]))
}

#' Returns z-scores for a subset of experiments
#'
#' @param cid        Vector of experiment IDs to subset
#' @param rid        Vector of probe IDs to subset
#' @param map.genes  BioMart identifier (eg. 'hgnc_symbol') of IDs to map to, or FALSE
get_z = function(cid, rid=landmarks, map.genes=FALSE) {
    #TODO: handle transpose better?
    fname = .p$file("lincs", "zspc_n1328098x22268.gctx")
    re = parse_gctx(fname=fname, cid=cid, rid=rid)

    if (is.character(map.genes)) {
        ann = import('../../process/microarray/annotate')
        ann$annotate(re, annotation="hthgu133a.db", summarize="hgnc_symbol")
    } else
        re
}

#' Returns the LINCS metadata
#'
#' @return  A data.frame containing the experimental metadata
get_index = function() {
    fname = .p$file("lincs", "inst.info")
    .io$read_table(fname, quote="", header=TRUE, sep="\t")
}
