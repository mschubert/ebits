io = import('ebits/io')

#' Calculate ARACNe network from gene expression matrix
#'
#' @param mat      A gene expression matrix (genes x samples)
#' @param tfs      A list of TF symbols (protected by DPI)
#' @param folder   Directory to save temporary output files in
#' @param dpi      Perform edge filtering using Data Processing Inequality
#' @param bootstrap  Number of bootstraps to perform
#' @param consolidate  Return consensus network instead of all bootstraps
#' @param p.value  P-value cutoff for keeping edges
#' @param cleanup  Delete temp folder when done
#' @return         A data.frame with columns (Regulator, Target, MI, pvalue)
aracne = function(mat, tfs=NULL, folder=tempdir(), dpi=!is.null(tfs),
                  bootstrap=100, consolidate=TRUE, p.value=1e-8, cleanup=TRUE) {
    cmd = sprintf("java -Xmx5G -jar %s -o %s", module_file("Aracne.jar"), folder)
    if (!dpi)
        cmd = paste(cmd, "--noDPI")

    # write matrix and TFs to temp file
    if (!file.exists(folder))
        dir.create(folder)
    mdf = tibble::rownames_to_column(as.data.frame(mat), "gene")

    mfile = file.path(folder, "matrix.txt")
    io$write_table(mdf, file=mfile, col.name=TRUE)
    tfile = file.path(folder, "tfs.txt")
    io$write_table(tfs, file=tfile, col.names=FALSE)

    cmd_mat = sprintf("%s -e %s --tfs %s --pvalue %E", cmd, mfile, tfile, p.value)

    # calculate ARACNe threshold
    system(sprintf("%s --seed 1 --calculateThreshold", cmd_mat))

    # perform one MI calc or the bootstraps
    if (bootstrap == 0) {
        system(sprintf("%s --seed 1 --nobootstrap", cmd_mat))
    } else {
        for (i in seq_len(bootstrap))
            system(sprintf("%s --seed %i", cmd_mat, i))
    }

    # consolidate network or return all bootstraps
    if (consolidate) {
        system(sprintf("%s --consolidate", cmd))
        result = io$read_table(file.path(folder, "network.txt"), header=TRUE)
    } else {
        bootstraps = list.files(folder, "^bootstrapNetwork_")
        read_fun = function(i) {
            re = io$read_table(bootstraps[i], header=TRUE)
            re$Bootstrap = i
            re
        }
        result = dplyr::bind_rows(lapply(seq_along(bootstraps), read_fun))
    }

    if (cleanup)
        unlink(folder, recursive=TRUE)

    result
}

if (is.null(module_name())) {
    library(testthat)

    mdf = io$read_table("matrix.txt", header=TRUE)
    mat = data.matrix(mdf[,2:ncol(mdf)])
    rownames(mat) = mdf[[1]]
    tfs = io$read_table("tfs.txt", header=FALSE)[[1]]
    result = aracne(mat, tfs, bootstrap=10)

    expect_equal(colnames(result),
                 c("Regulator", "Target", "MI", "pvalue"))
}
