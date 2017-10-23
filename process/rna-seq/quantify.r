#' Return cached file name of reference genome
#'
#' This will download the reference from Ensembl if it does not yet exist
#'
#' @param spcies    Species (eg. homo_sapiens, mus_musculus[_strain])
#' @param assembly  Genome assembly version (eg. GRCh38)
#' @param release   Ensembl release (default: latest for assembly)
.reference = function(species, assembly, release="latest") {
    if (release == "latest") {
        #TODO: query ensembl for release version
        release = 90
        message("Using Ensembl release: ", release)
    }

    cache_dir = file.path(module_file(), "cache")
    if (!file.exists(cache_dir))
        dir.create(cache_dir)
    ref_file = file.path(cache_dir, sprintf("%s-%s-r%s.fa.gz",
                                            species, assembly, release))

    if (!file.exists(ref_file)) {
        cap_species = paste0(toupper(substring(species, 1,1)), substring(species, 2))
        ens = sprintf("ftp://ftp.ensembl.org/pub/release-%i/fasta/%s/%%s/%s.%s.%%s.fa.gz",
                      release, species, cap_species, assembly)
        files = sprintf(ens, c("cdna", "ncrna"), c("cdna.all", "ncrna"))
        outfiles = setNames(file.path(module_file("cache"), basename(files)), files)
        for (f in files)
            download.file(f, destfile=outfiles[f])
        system(sprintf("cat %s > %s", paste(outfiles, collapse=" "), ref_file))
        unlink(outfiles)
    }

    ref_file
}

star = function(...) {
    stop("not implemented")
}

#' Quantify RNA-sequencing data using Kallisto
#'
#' @param files            Input fastq or directory containing fastq
#' @param paired_end       Library is single-end reads (default: FALSE)
#' @param fragment_length  Average fragment length if paired_end=FALSE
#' @param fragment_sd      Fragment sd if paired_end=FALSE
#' @return
kallisto = function(files, species="homo_sapiens", assembly="GRCh38", release="latest",
                    paired_end=FALSE, fragment_length=NA, fragment_sd=NA, verbose=TRUE) {
    if (!paired_end && (is.na(fragment_length) || is.na(fragment_sd)))
        stop("need fragment_{length,sd} for single-end reads")

    if (verbose)
        message(match.call())

    if (length(files) == 1 && file.info(files)[1,"isdir"]) {
        if (verbose)
            message("Using files in directory: ", files)
        files = list.files(files, pattern="\\.fastq(\\.gz)?$", full.names=TRUE)
    }

    index_file = file.path(module_file(), "cache",
           sprintf("%s_%s_%s.kallisto_idx", species, assembly, release))
    if (!file.exists(index_file)) {
        ref = .reference(species, assembly, release)
        cmd = sprintf("kallisto index -i %s %s", index_file, ref)
        if (verbose)
            message(cmd)
        system(cmd)
    }

    tmp = tempdir()
    files = paste(files, collapse=" ")
    if (paired_end)
        cmd = sprintf("kallisto quant -i %s -o %s %s", index, tmp, files)
    else
        cmd = sprintf("kallisto quant -i %s -o %s --single --fragment-length %.3f --sd %.3f %s",
                      index_file, tmp, fragment_length, fragment_sd, files)

    if (verbose)
        message(cmd)
    system(cmd)

    mapped = readr::read_tsv(file.path(tmp, "abundance.tsv"))
    unlink(tmp, recursive=TRUE)
    mapped
}

salmon = function(...) {
    stop("not implemented")
}
