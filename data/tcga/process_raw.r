# read raw data from .txt.gz files
# save into R objects for quicker loading
.p = import('../path')
.b = import('../../base')
.io = import('../../io')

.list_files = function(file_regex, dir="stddata__2015_06_01") {
    dir = .p$file("tcga", dir)
    list.files(dir, pattern=file_regex, full.names=TRUE, recursive=TRUE)
}

.unpack = function(files, unpack_dir=tempdir()) {
    for (file in files)
        untar(file, exdir = unpack_dir)
    file.path(unpack_dir, sub(".tar", "", sub(".gz", "", basename(files))))
}

.select = function(newdir, tar_regex) {
    list.files(newdir, pattern=tar_regex, full.names=TRUE, recursive=TRUE)
}

clinical = function(save=NULL) {
    clist = .list_files("[A-Z]+\\.Clinical_Pick_Tier1\\.Level_4.*\\.tar\\.gz$") %>%
        .unpack() %>%
        .select("clin\\.merged\\.picked\\.txt") %>%
        lapply(function(fname) {
            ff = t(.io$read_table(fname))
            colnames(ff) = ff[1,]
            ff = ff[-1,]
            ff = as.data.frame(ff)
            ff$study = .b$grep("gdac.broadinstitute.org_([A-Z]+)", file)
            ff
        })

    cnames = do.call(.b$intersect, lapply(clist, colnames))
    cc = do.call(rbind, lapply(clist, function(x) x[,cnames]))
    rownames(cc) = 1:nrow(cc)
    if (!is.null(save))
        save(cc, file=save)
    cc
}

rna_seq_raw(save=NULL) {
    elist = .list_files("mRNAseq_Preprocess\\.Level_3.*\\.tar(\\.gz)?$") %>%
        .unpack() %>%
        .select("[^2]\\.mRNAseq_raw_counts\\.txt") %>%
        lapply(function(fname) {
            re = .io$read_table(fname, header=TRUE, check.names=FALSE)
            mat = data.matrix(re[,-1])
            rownames(mat) = re[,1]
            mat
        })
}
