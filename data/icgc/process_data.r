mat = function(fname, regex, formula, map.hgnc=FALSE, force=FALSE, fun.aggregate=sum) {
    if (!force && file.exists(file.path(icgc_data_dir, fname)))
        return()

    idmap = import('process/idmap')
    efiles = list.files(icgc_raw_dir, regex, recursive=T, full.names=T)

    file2matrix = function(fname) {
        message(fname)
        mat = .ar$construct(read.table(fname, header=T, sep="\t"),
                           formula = formula,
                           fun.aggregate=fun.aggregate)
        mat = mat[!rownames(mat) %in% c('?',''),] # aggr fun might handle his?
        if (map.hgnc)
            mat = idmap$gene(mat, to='hgnc_symbol', summarize=fun.aggregate)
        mat
    }
    obj = lapply(efiles, file2matrix)

    if (length(obj) == 0)
        stop("all file reads failed, something is wrong")

    mat = t(.ar$stack(obj, along=2, fill=0))
    if (grepl("\\.h5$", fname))
        h5store::h5save(mat, file=file.path(icgc_data_dir, fname))
    else
        save(mat, file=file.path(icgc_data_dir, fname))
}

df = function(fname, regex, transform=function(x) x, force=FALSE) {
    if (!force && file.exists(file.path(icgc_data_dir, fname)))
        return()

    files = list.files(icgc_raw_dir, regex, recursive=TRUE, full.names=TRUE)
    mat = do.call(rbind, lapply(files, function(f) cbind(
        study = b$grep("/([^/]+)/[^/]+$", f),
        read.table(f, header=TRUE, sep="\t") #FIXME: io$read should work here
    ))) %>% transform()

    if (grepl("\\.h5$", fname))
        h5store::h5save(mat, file=file.path(icgc_data_dir, fname))
    else
        save(mat, file=file.path(icgc_data_dir, fname))
}

#' Function that processes a raw data download from ICGC to .RData/hdf5 files that can be used for analysis
#'
#' @param force  overwrite existing files instead of skipping
process_raw_data = function(force=FALSE) {
    tfun = function(x) mutate(x, tissue = .b$grep("^(\\w+)", project_code))

    df("clinical.RData", "clinical\\.", transform=tfun, force=force)
    df("clinicalsample.RData", "clinicalsample\\.", force=force)
    mat("expr_seq_raw.h5", '^exp_seq',
          raw_read_count ~ gene_id + icgc_sample_id, map.hgnc=TRUE, force=force)
    mat("expr_seq_norm.h5", '^exp_seq',
          normalized_read_count ~ gene_id + icgc_sample_id, map.hgnc=TRUE, force=force)
    mat("protein.h5", '^protein_expression',
          normalized_expression_level ~ antibody_id + icgc_sample_id, map.hgnc=FALSE, force=force)

#    mat("cnv.h5", '^copy_number_somatic_mutation',
#          segment_median ~ gene_affected + icgc_sample_id,
#          fun.aggregate = mean, map.hgnc=T, force=force)

#    mut_aggr = function(x) {
#        if (length(x) == 0)
#            return(0)
#        if (!is.numeric(x)) {
#            mtypes = getMutationTypes(bits=TRUE)
#            x = mtypes$bit_code[mtypes$consequence_type %in% x]
#        }
#        Reduce(bitwOr, x)
#    }
    mut_aggr = function(x) {
        any(x != 0)
    }
    mat('mutations.h5', '^simple_somatic',
        consequence_type ~ gene_affected + icgc_sample_id,
        fun.aggregate = mut_aggr, force=force, map.hgnc=TRUE)

    voomfile = file.path(icgc_data_dir, "expr_seq_voom.h5")
    if (identical(force, TRUE) || !file.exists(voomfile)) {
        expr = getRNASeq(raw.counts=TRUE) %>% na.omit()
        expr = limma::voom(expr)$E
        h5store::h5save(t(expr), file=voomfile)
    }
}
