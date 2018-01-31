library(dplyr)
io = import('io')
seq = import('seq')
sys = import('sys')

#' Run aneufinder with clean output structure
#'
#' Does not support strandseq so far
#'
#' @param files          Character vector of BAM or BED files, or directory
#' @param assembly       Character string identifying the assembly; e.g.
#'                       GRCh38, mm10, etc.; sequence lengths derived from that
#' @param chromosomes    Restrict analysis to the following chromosomes
#' @param blacklist      BED file name with blacklisted regions, BAM file with
#'   reads to derive blacklist from, or NULL if no blacklist
#' @param binsize        Average bin size for reads
#' @param correction     "GC", "mappability" or both in a vector
#' @param states         Max number of chromosome copies to model
#' @param bin_width_ref  Reference BAM file for creating variable width bins
#'   value can either be NULL (fixed bins) or a path to a file to use for reference
#' @param method         Aneufinder method: "edivisive", "HMM" or "dnacopy"
#' @return               A list of models, one for each input file
run = function(files, assembly, chromosomes=NULL, blacklist=NULL,
               binsize=1e6, correction="GC", states=10,
               mappability_reference=NULL, paired_reads=FALSE,
               duplicate_reads=FALSE, min_mapq=10, max_time=-1, n_trials=15,
               bin_width_ref=NULL, method="edivisive") {

    stopifnot(length(files) >= 1)
    stopifnot(length(binsize) == 1) # change [[1]] below if removing

    if (length(files) == 1 && file.info(files)[1,"isdir"])
        files = list.files(files, pattern="\\.(bam|bed)(\\.gz)?$",
                           full.names=TRUE)

    # get assembly information
    genome = seq$genome(assembly)
    chr_lengths = seq$chr_lengths(genome)
    if (is.null(chromosomes))
        chromosomes = grep("^((chr)?[0-9]+|X)$", names(chr_lengths), value=TRUE)
    assembly_df = data.frame(chromosome = names(chr_lengths), length=chr_lengths)

    # partition the reads into bins
    if (!is.null(bin_width_ref)) {
        ref_reads = seq$read_granges(bin_width_ref, assembly=assembly_df)

        if (!is.null(blacklist) && grepl("\\.bam$", blacklist))
            blacklist = blacklist_from_ref(ref_reads, chr_lengths,
                                           chromosomes=chromosomes)

        bins = AneuFinder::variableWidthBins(ref_reads, binsizes=binsize,
            chromosomes = chromosomes)[[1]]
    } else
        bins = AneuFinder::fixedWidthBins(chromosomes = chromosomes,
            chrom.lengths = chr_lengths, binsizes = binsize)[[1]]

    # load BAM or BED files to granges objects
    reads = seq$read_granges(as.list(files), chromosomes=chromosomes,
                             pairedEndReads=paired_reads, assembly=assembly_df,
                             remove.duplicate.reads=!duplicate_reads,
                             min.mapq=min_mapq, blacklist=blacklist)

    binned_reads = mapply(bin_reads, reads=reads, id=basename(files),
                          MoreArgs = list(bins=bins, min_mapq=min_mapq))

    # gc or mappability correction #TODO: preserve order
    if ("GC" %in% correction) {
        binned_reads = AneuFinder::correctGC(binned_reads, GC.BSgenome=genome,
                                             same.binsize=TRUE)
    }
    if ("mappability" %in% correction)
        binned_reads = AneuFinder::correctMappability(binned_reads,
            is.null(bin_width_ref), mappability_reference, chr_lengths)

    # run HMM/dnacopy
    models = lapply(binned_reads, function(br) {
        m = try(AneuFinder::findCNVs(br,
                eps = 0.1,
                max.time = max_time,
                max.iter = 5000,
                num.trials = n_trials,
                states = c("zero-inflation", sprintf("%i-somy", 0:states)),
                most.frequent.state = "2-somy",
                method = method))
        if (class(m$segments) == "GRanges") {
            idx = unique(as.character(GenomeInfoDb::seqnames(m$segments)))
            GenomeInfoDb::seqinfo(m$segments) = GenomeInfoDb::seqinfo(genome)[idx]
        }
        m
    })
}

#' Plot AneuFinder model
#'
#' @param model  An AneuFinder model object
#' @return       A cowplot object with profiles and histograms
plot = function(model) {
    p1 = graphics::plot(model, type='profile')
    p2 = graphics::plot(model, type='histogram')
    cowplt = cowplot::plot_grid(p1, p2, nrow=2, rel_heights=c(1.2,1))
}

#' Create a blacklist from WGS read numbers
#'
#' @param ref_reads    GRanges object with reads (or character string of file)
#' @param chr_lengths  Numeric vector of chromosome lengths
#' @param chromosomes  Only include these chromosomes
#' @param lower        Lower quantile to cut off
#' @param upper        Upper quantile to cut off
#' @return             GRanges object of blacklisted regions
blacklist_from_ref = function(ref_reads, chr_lengths, lower=0.05, upper=0.999,
                              chromosomes=names(chr_lengths)) {
    assembly_df = data.frame(chromosome = names(chr_lengths), length=chr_lengths)
    if (is.character(ref_reads))
        ref_reads = seq$read_granges(ref_reads, assembly=assembly_df)

    # select bin size so avg >=100 reads/bin (down to 1kb), blacklist outliers
    bsize = round(100 * sum(as.numeric(chr_lengths)) / length(ref_reads))
    bsize = max(bsize, 1000)

    ref_bins = AneuFinder::fixedWidthBins(binsizes=as.integer(bsize),
        chromosomes=chromosomes, chrom.lengths=chr_lengths)[[1]]
    ref_counts = bin_reads(reads=ref_reads, bins=ref_bins)
    lower = ref_counts$counts < quantile(ref_counts$counts, lower)
    upper = ref_counts$counts > quantile(ref_counts$counts, upper)
    blacklist = ref_bins[lower | upper]
}

#' Assign reads to pre-existing bins
#'
#' This function exists in AneuFinder, but it is re-bins all reads and ignores
#' already provided reads. The functionality here is basically the same as:
#' https://github.com/ataudt/aneufinder/blob/master/R/binReads.R#L269-L303
#'
#' @param reads     A GRanges object of reads
#' @param bins      A GRanges object of bins
#' @param min_mapq  Min mapping quality (added to object for info)
#' @param id        Sample ID (added to object for info)
#' @return          A GRanges object of the read counts per bin
bin_reads = function(reads, bins, min_mapq=NULL, id=NULL) {
    grc = function(r) GenomicRanges::countOverlaps(bins, r)
    S4Vectors::mcols(bins) = S4Vectors::DataFrame(
        counts = grc(reads),
        mcounts = grc(reads[BiocGenerics::strand(reads) == '-']),
        pcounts = grc(reads[BiocGenerics::strand(reads) == '+']))

    qualityInfo = list(complexity = c(MM=NA),
                       coverage = NA,
                       spikiness = AneuFinder:::qc.spikiness(bins$counts),
                       entropy = AneuFinder:::qc.entropy(bins$counts))

    attr(bins, 'qualityInfo') = qualityInfo
    attr(bins, 'min.mapq') = min_mapq
    attr(bins, 'ID') = id
    bins
}

#' Calculate the consensus ploidy state per chromosome across single cell models
#'
#' @param mods  List of AneuFinder models or GRanges objects
#' @return      Data.frame with fields: seqnames (chromosome name),
#'              length (chromosome length, coverage (% of chr covered by bins),
#'              and ploidy (average ploidy along chromosome)
consensus_ploidy = function(mods) {
    if (class(mods) != 'list')
        mods = list(mods)
    if (class(mods[[1]]) == "aneuHMM")
        mods = lapply(mods, function(m) m$segments)

    suppressWarnings(mods %>%
        lapply(as_tibble) %>%
        dplyr::bind_rows() %>%
        group_by(seqnames) %>%
        summarize(ploidy = weighted.mean(copy.number, weights=width),
                  bases_covered = sum(as.numeric(width)) / length(mods)) %>%
        transmute(seqnames = as.character(seqnames),
                  length = unname(GenomeInfoDb::seqlengths(mods[[1]])[seqnames]),
                  coverage = bases_covered / length,
                  ploidy = ploidy))
}

sys$run({
    args = sys$cmd$parse(
        opt('d', 'directory', 'directory prefix to look for files', '.'),
        opt('o', 'outfile', 'file to save model to', 'model.RData'),
        opt('p', 'plotfile', 'file to save plots to', 'model.pdf'),
        opt('a', 'assembly', 'genome assembly identifier', ''),
        opt('b', 'bin_width_ref', 'euploid reference for bin width', ''),
        opt('r', 'min_reads', 'minimum numer of reads for sample', '5e4'),
        arg('infiles', 'BAM/BED files, directory or sample file', arity='*'))

    if (length(args$infiles) == 1 && grepl("\\.yaml$", args$infiles)) {
        config = io$read_yaml(args$infiles)
        args$infiles = file.path(config$directory, config$files)
        args$assembly = config$assembly
        args$bin_width_ref = config$bin_width_ref
        args$blacklist = config$blacklist
    }

    print(args)

    message("Running aneufinder ...")
    all_models = run(args$infiles,
                     assembly = args$assembly,
                     bin_width_ref = args$bin_width_ref,
                     blacklist = args$blacklist)
    names(all_models) = sapply(all_models, function(x) x$ID)

    nreads = sapply(all_models, function(m) sum(m$bincounts[[1]]$counts)) %>%
        setNames(names(all_models))
    excl_reads = nreads[nreads < as.numeric(args$min_reads)]
    excl_reads = setNames(paste(excl_reads, "reads"), names(excl_reads))
    exclude = c(unlist(config$exclude), excl_reads)

    models = all_models[!names(all_models) %in% names(exclude)]
    if (length(exclude) > 0) {
        warning("Dropping models: ", paste(names(exclude), collapse=", "))
        for (i in seq_along(all_models)) {
            model_name = names(all_models)[i]
            excl_idx = which(names(exclude) == model_name)
            if (length(excl_idx) > 0) {
                reason = paste(exclude[excl_idx], collapse=", ")
                all_models[[i]]$ID = sprintf("%s [%s]", model_name, reason)
            }
        }
    }
    save(models, file=args$outfile)

    if (!is.null(args$plotfile)) {
        message("Plotting sample heatmap ...")

        pdf(args$plotfile, width=20, height=10)

        # plot models we keep
        print(AneuFinder::heatmapGenomewide(models, cluster=TRUE))

        # plot all models to see why we exclude
        if (length(exclude) > 0)
            print(AneuFinder::heatmapGenomewide(all_models, cluster=TRUE))

        # plot individual model read bin counts, distributions, etc.
        for (i in seq_along(all_models)) {
            message("Read density plot for: ", names(all_models)[i])
            if (class(all_models[[i]]) == "aneuHMM")
                print(plot(all_models[[i]]))
        }

        dev.off()
    }
})
