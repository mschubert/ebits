# read raw data from .txt.gz files
# save into R objects for quicker loading
.p = import('../path')
.b = import('../../base')
.io = import('../../io')
.ar = import('../../array')

.list_files = function(file_regex, dir="stddata__2015_06_01") {
    dir = .p$file("tcga", dir)
    list.files(dir, pattern=file_regex, full.names=TRUE, recursive=TRUE)
}

.unpack = function(files, unpack_dir=tempdir()) {
    #TODO: only unpack if file is not there
    for (file in files)
        untar(file, exdir = unpack_dir)
    file.path(unpack_dir, sub(".tar", "", sub(".gz", "", basename(files))))
}

.select = function(newdir, tar_regex) {
    list.files(newdir, pattern=tar_regex, full.names=TRUE, recursive=TRUE)
}

clinical = function(file) {
    clist = .list_files("[A-Z]+\\.Merge_Clinical.Level_1.*\\.tar\\.gz$") %>%
        .unpack() %>%
        .select("clin\\.merged\\.txt")

    clist = clist[-24] #FIXME: remove OV, invalid number of fields

    cdata = clist %>%
        lapply(function(fname) {
            ff = t(.io$read_table(fname, sep="\t", quote=""))
            colnames(ff) = ff[1,]
            ff = ff[-1,]
            ff = as.data.frame(ff)
            ff$study = .b$grep("gdac.broadinstitute.org_([A-Z]+)", fname)
            ff
        })

    cnames = do.call(.b$intersect, lapply(cdata, colnames))
    cc = do.call(rbind, lapply(cdata, function(x) x[,cnames]))
    rownames(cc) = 1:nrow(cc)
    save(cc, file=.p$file("tcga", file))
    cc
}

rna_seq_voom = function(file) {
    elist = .list_files("mRNAseq_Preprocess\\.Level_3.*\\.tar(\\.gz)?$") %>%
        .unpack() %>%
        .select("[^2]\\.mRNAseq_raw_counts\\.txt") %>%
        lapply(function(fname) {
            re = .io$read_table(fname, header=TRUE, check.names=FALSE)
            mat = data.matrix(re[,-1])
            rownames(mat) = re[,1]

            # voom-specific below, separate this later
            mat = limma::voom(mat)$E
            rownames(mat) = sub("\\|[0-9]+$", "", rownames(mat))
            mat = mat[rownames(mat) != "?",]
            limma::avereps(mat)
        }) %>%
        .ar$stack(along=2)

    .io$h5save(elist, file=.p$file("tcga", file))
}

rna_seq2_voom = function(file) {
    elist = .list_files("RSEM_genes__data.Level_3.*\\.tar(\\.gz)?$") %>%
        .unpack() %>%
        .select("rnaseqv2")
    
    expr = elist %>%
        lapply(function(fname) {
            re = .io$read_table(fname, header=TRUE, check.names=FALSE)
            re = re[-1, re[1,] %in% c("gene_id", "raw_count")]
            mat = data.matrix(re[,-1])
            rownames(mat) = re[,1]

            # voom-specific below, separate this later
            mat = limma::voom(mat)$E
            rownames(mat) = sub("\\|[0-9]+$", "", rownames(mat))
            mat = mat[rownames(mat) != "?",]
            limma::avereps(mat)
        }) #%>%
#        .ar$stack(along=2, fun.aggregate=mean) #TODO: see which sample + how to handle

    names = .b$grep("gdac.broadinstitute.org_([A-Z]+)", elist)

#    .io$h5save(elist, file=.p$file("tcga", file))
    setNames(expr, names)
}

# need to map clinical identifiers to sample identifiers

if (is.null(module_name())) {
    rna_seq2_voom(file="rna_seq_voom.h5")
    cc = clinical(file="clinical_full.RData")
}
