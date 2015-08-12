.b = import('../../base') # %or%
.io = import('../../io')
.p = import('../path')
.ar = import('../../array')

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
#' @param map.genes  BioMart identifier of IDs to map to, or FALSE. Supported
#'                   identifiers are: hgnc_symbol, hgnc_id, entrezgene,
#'                   ensembl_gene_id, ensembl_transcript_id
get_z = function(cid, rid=landmarks, map.genes=FALSE) {
    #TODO: handle transpose better?
    fname = .p$file("lincs", "zspc_n1328098x22268.gctx")
    re = parse_gctx(fname=fname, cid=cid, rid=rid)

    if (is.character(map.genes))
        .ar$summarize(re, along=1, from="affy_hg_u133_plus_2", to=map.genes,
                      data=.probe_annotations(), FUN=function(x) mean(x, na.rm=TRUE))
    else
        re
}

#' Returns the LINCS metadata
#'
#' @return  A data.frame containing the experimental metadata
get_index = function() {
    fname = .p$file("lincs", "inst.info")
    .io$read_table(fname, quote="", header=TRUE, sep="\t")
}

#' Returns a data.frame of probe annotations
#'
#' @return  A data.frame with different annotation columns
.probe_annotations = function() {
    fname = .p$file('lincs', 'annot.RData')
    if (!file.exists(fname)) {
        mart = biomaRt::useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")
        annot = biomaRt::getBM(attributes = c("hgnc_symbol", "hgnc_id", "entrezgene",
            "ensembl_gene_id", "ensembl_transcript_id", "affy_hg_u133_plus_2"),
            filter = "affy_hg_u133_plus_2", values = projected, mart=mart)
        annot[annot==''] = NA

        save(annot, file=fname)
        annot
    } else
        .io$load(fname)
}
