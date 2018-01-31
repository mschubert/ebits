library(dplyr)
library(ggplot2)
io = import('io')
seq = import('seq')
sys = import('sys')

`.seqinfo<-` = function(x, value) {
    idx = unique(as.character(GenomeInfoDb::seqnames(x)))
    x@seqinfo = value[idx]
    if (inherits(x, "GRanges"))
        x@seqnames = droplevels(x@seqnames)
    x
}

#' Run chromstar using a clean API and no side effects
#'
#' @param exp_table    A data.frame or tabular file with fields:
#'    'file': bam file with the mark we are investigating
#'    'mark': the ChIP mark that we are investigating
#'    'condition': sample identifier or e.g. basal vs perturbed
#'    'replicate': number of replicate condition (all '1' if no replicates)
#'    'pairedEndReads': whether fibrary is paired end (TRUE/FALSE)
#'    'controlFiles': bam file for the input DNA
#' @param assembly     Character string identifying the assembly; e.g.
#'    GRCh38, mm10, etc.; sequence lengths derived from that
#' @param chromosomes  Restrict analysis only to those chromosomes listed
#' @param duplicate_reads  Whether to allow duplicate reads (default: FALSE)
#' @param min_mapq     Minimum mapping quality (default: 10)
#' @return             A list of GRanges objects for all aligned reads
read_files = function(exp_table, assembly, chromosomes=NULL, duplicate_reads=FALSE,
                      min_mapq=10, blacklist=NULL) {
    files = unique(c(exp_table$file, exp_table$controlFiles))
    paired_end = unique(c(exp_table$pairedEndReads))
    if (length(paired_end) != 1)
        stop("pairedEndReads must be common to experiment")

    genome = seq$genome(assembly)
    chr_lengths = seq$chr_lengths(genome)
    if (is.null(chromosomes))
        chromosomes = grep("^((chr)?[0-9]+|X)$", names(chr_lengths), value=TRUE)
    assembly_df = data.frame(chromosome = names(chr_lengths), length=chr_lengths)

    # get and bin reads
    reads = seq$read_granges(as.list(files), chromosomes=chromosomes,
                             pairedEndReads=paired_end, assembly=assembly_df,
                             remove.duplicate.reads=!duplicate_reads,
                             min.mapq=min_mapq, blacklist=blacklist)
    names(reads) = files
    for (i in seq_along(reads))
        .seqinfo(reads[[i]]) = seq$info(genome)
    reads
}

#' Bin reads into segments with read counts
#'
#' @param reads    GRanges object of aligned reads for all files
#' @param binsize  Width of bins to assign reads to (default: 1000)
#' @return         A list of GRanges objects representing the aligned read counts
bin_reads = function(reads, binsize=1000) {
    bins = chromstaR::fixedWidthBins(
        chrom.lengths = seq$lengths(reads[[1]]),
        chromosomes = levels(seq$names(reads[[1]])),
        binsizes = binsize)

    lapply(reads, chromstaR::binReads, assembly=NULL,
        binsizes=NULL, reads.per.bin=NULL, stepsize=binsize/2, bins=bins)
}

#' Run chromstar in univariate mode
#'
#' @param binned_reads  A list of GRanges objects represnting binned reads
#' @return              A list of univariate Chromstar model objects
univ = function(binned_reads, exp_table) {
    exp_table$ID = sprintf("%s-%s-rep%s", exp_table$mark,
                           exp_table$condition, exp_table$replicate)
    univ = split(exp_table, seq_len(nrow(exp_table)))
    lapply(univ, function(exp) {
        m = chromstaR::callPeaksUnivariate(
            binned.data = binned_reads[[exp$file]],
            input.data = binned_reads[[exp$controlFiles]],
            keep.posteriors = FALSE)
        # lengths are copied, genome is not
        .seqinfo(m$bins) = seq$info(binned_reads[[1]])
        .seqinfo(m$peaks) = seq$info(binned_reads[[1]])
        m$info = exp
        m
    })
}

#' Combine multivariate models
#'
#' @param models  Univariate Chromstar models ('info' field: exp_table row)
#' @param mode    A character string that describes how to build the model:
#'    'separate': each condition and mark separately
#'    'combinatorial': combined model of different marks in one condition
#'    'differential': combined model for one mark in different condition/sample
#'    'full': combined conditions and marks
#' @param marks    Which marks to include from table (default: all)
#' @param samples  Which conditions to include from table (default: all)
#' @return  Combined Chromstar result
combine = function(models, mode, marks=NULL, samples=NULL) {
    # make sure all models have the same genome
    sinfo = unique(lapply(models, function(m) GenomeInfoDb::seqinfo(m$peaks)))
    if (length(sinfo) != 1)
        stop("models must all have same seqinfo: ", paste(sinfo, collapse=", "))
    sinfo = sinfo[[1]]

    # use only models that have requested marks and conditions
    if (!is.null(marks))
        models = models[sapply(models, function(m) m$info$mark %in% marks)]
    if (!is.null(samples))
        models = models[sapply(models, function(m) m$info$condition %in% samples)]

    # create combined experiment table
    exp_table = lapply(models, function(m) m$info) %>% dplyr::bind_rows()
    names(models) = exp_table$ID
    brew_mode = mode

    # create experiment table for conditions
    switch(mode,
        "separate" = {
            reps = paste0(exp_table$mark, '-', exp_table$condition)
            exps = split(exp_table, reps)
            brew_mode = "combinatorial"
            mode = "replicate"
        },
        "full" = {
            exps = list(exp_table)
        },
        "combinatorial" = {
            exps = split(exp_table, exp_table$condition)
        },
        "differential" = {
            exps = split(exp_table, exp_table$mark)
        }
    )

    # combine models
    multi = lapply(exps, function(exp) {
        re = chromstaR::callPeaksMultivariate(
            hmms = models[exp$ID],
            use.states = chromstaR::stateBrewer(select(exp, -ID), mode=brew_mode))
        re
    })
    cmb = chromstaR::combineMultivariates(multi, mode=mode)

    # annotate GRanges objects with seqinfo information
    .seqinfo(cmb$segments) = sinfo
    for (i in seq_along(cmb$peaks))
        .seqinfo(cmb$peaks[[i]]) = sinfo

    cmb
}

#' Function to split genome in regions with constant copy number
#'
#' @param gr       A GRanges object (eg. AneuFinder model$segments)
#' @param field    Categorical field in GRanges on which to split
#' @param binsize  Size of bins to split (instead of exact matches)
#' @return         A list of GRanges objects
split_granges = function(gr, field="state", binsize=NULL) {
    split_fun = function(x) {
        if (opt$exact)
            end(x) = start(x)-1+floor(width(x)/binsize)*binsize
        unlist(tile(x, width=binsize))
    }
    lapply(somies, as.factor(gr[[field]]))
}

#' Filter peaks that do not pass a posterior probability threshold
#'
#' @param model  Chromstar model object
#' @param pp     Posterior probability cutoff
filter_peaks = function(model, pp=1e-4) {
    if (is.list(model) && !grepl("HMM", class(model)))
        lapply(model, chromstaR::changeFDR, fdr=pp)
    else
        chromstaR::changeFDR(model, fdr=pp)
}

#' Plot transition probabilities for model
#'
#' @param model  Chromstar model object
plot_transitions = function(model) {
    chromstaR::heatmapTransitionProbs(model) +
        ggtitle('Transition probabilities')
}

#' Plot read count correlation for chromstar models
#'
#' @param models   Chromstar combined model or list of model objects
#' @param cluster  cluster the axes (default: TRUE)
#' @return         GGplot correlation matrix
plot_cor = function(models, cluster=TRUE) {
    if (any(grepl("[mM]ultiHMM", class(models))))
        bin_counts = model$bins$counts.rpkm
    else {
        bin_counts = sapply(models, function(m) m$bins$counts.rpkm)
        colnames(bin_counts) = sapply(models, function(m) m$info$ID)
    }

    mat = cor(bin_counts)
    df = reshape2::melt(mat, value.name='cor')

    if (cluster) {
        hc = stats::hclust(stats::dist(mat))
        df$Var1 = factor(df$Var1, levels=unique(df$Var1)[hc$order])
        df$Var2 = factor(df$Var2, levels=unique(df$Var2)[hc$order])
    }

    ggplot(df, aes(x=Var1, y=Var2)) +
        geom_tile(aes(fill=cor)) +
        geom_text(aes(label = sprintf("%.2f", cor)), size=min(4, 40/nrow(mat))) +
        labs(title = "Read count correlation", x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0)) +
        scale_fill_gradient2(low='blue', mid='white', high='red', limits=c(-1,1))
}

#' Plot genomic frequencies of states
#'
#' @param model  Chromstar model object
plot_frequencies = function(model) {
    freqs = chromstaR::genomicFrequencies(model)
    fdf = reshape2::melt(as.array(freqs$domains))
    p = ggplot(fdf, aes(x=Var1, y=value)) +
        scale_y_log10() +
        labs(x="modification", y="number") +
        geom_bar(stat="identity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle=45, hjust=1))

    if ("Var2" %in% colnames(fdf))
        p + facet_wrap(~ Var2)
    else
        p
}

#' Plots mark enrichment around a set of coordinates
#'
#' @param model  Chromstar model object
#' @param dset   Ensembl data set to use for gene annotations
#' @param ref    Center around either gene body or TSS
#' @param flank  Number  of bp in flanking region
#' @param pp     Filter peaks by 1-posterior probability (default: no filter)
#' @param ...    Additional parameters passed to chromstaR::plotEnrichment
#' @return       A ggplot2 object
plot_enrichment = function(model, dset, ref=c("gene", "TSS"), flank=1.5e4, pp=NULL, ...) {
    ref_lookup = setNames(c(c("start", "inside", "end"), "start"),
                          c("gene", "TSS"))
    ref = match.arg(ref)
    coords = seq$coords$gene('ensembl_gene_id', dset=dset, granges=TRUE)

    if (!is.null(pp))
        model = filter_peaks(model, pp=pp)

    add = function(p) p +
        xlab("distance from gene body") +
        theme_minimal() +
        guides(color=FALSE) +
        theme(axis.text.x = element_text(angle=45, hjust=1)) +
        facet_wrap(~ combination)

    p = chromstaR::plotEnrichment(model, coords, region=ref_lookup[ref],
            bp.around.annotation=flank, ...)
    lapply(p, add)
}

#' Parses a YAML file according to specification
#'
#' @param cfg  List with the following fields: assembly, paired_end, directory,
#'  sample (sample name: input/marks), comparisons(comp name: sample/marks/mode)
#' @return     A data.frame to be used as experimental table
parse_spec = function(cfg) {
    fpath = function(f) lapply(f, function(fi)
        file.path(cfg$directory, unlist(strsplit(unlist(fi), " "))))

    parse_sample = function(s, sn) {
        re = tibble::tibble(condition = sn,
                            file = unname(fpath(s$marks)),
                            mark = names(s$marks))
        input = unlist(setNames(fpath(s$input), names(s$input)))
        if (length(input) == nrow(re))
            re$controlFiles = input[match(re$mark, names(s$input))]
        else if (length(input) == 1)
            re$controlFiles = rep(input, nrow(re))
        else
            stop("'input' fields needs to be common or list one input per mark")
        re
    }
    samples = mapply(parse_sample, s=cfg$sample, sn=names(cfg$sample),
                     SIMPLIFY=FALSE) %>%
        setNames(names(cfg$sample)) %>%
        bind_rows() %>%
        tidyr::unnest() %>%
        group_by(condition, mark) %>%
        mutate(replicate = seq_len(n()),
               pairedEndReads = cfg$paired_end) %>%
        ungroup() %>%
        select(file, mark, condition, replicate, pairedEndReads, controlFiles) %>%
        as.data.frame()
}

sys$run({
    args = sys$cmd$parse(
        opt('d', 'directory', 'directory containing source files', '.'),
        opt('s', 'samples', 'file containing sample table', 'SBT003.yaml'),
        opt('o', 'outfile', 'file to save model to', '/dev/null'),
        opt('u', 'unifile', 'file to save univariate model to', '/dev/null'),
        opt('r', 'readfile', 'file to save reads to', '/dev/null'),
        opt('b', 'bedfile', 'file to save track to', '/dev/null'),
        opt('p', 'plotfile', 'file to save plots to', '/dev/null'))

    print(args)

    config = io$read_yaml(args$samples)
    exp_table = parse_spec(config)
    print(exp_table)

    reads = read_files(exp_table, config$assembly)
    if (args$readfile != "/dev/null")
        save(reads, file=args$readfile)
    binned = bin_reads(reads)
    rm(reads)

    univ = univ(binned, exp_table)
    if (args$unifile != "/dev/null")
        save(univ, file=args$unifile)

    do_cmp = function(cmp) do.call(combine, c(list(models=univ), cmp))
    models = lapply(config$comparisons, do_cmp)

    if (!is.null(args$outfile))
        save(models, file=args$outfile)

#    if (!is.null(args$bedfile))
#        chromstaR:::exportCombinedMultivariatePeaks(model, filename=args$bedfile,
#            trackname=paste(basename(args$samples), args$mode, sep="-"),
#            separate.files=FALSE)

    if (!is.null(args$plotfile)) {
        if (grepl("GRCh|hg", config$assembly))
            dset = "hsapiens_gene_ensembl"
        else if (grepl("GRCm|mm", config$assembly))
            dset = "mmusculus_gene_ensembl"
        else
            stop("do not know which gene annotation to use, can not plot")

        pdf(args$plotfile)

        print(plot_cor(univ))
        for (u in univ)
            print(chromstaR::plotHistogram(u))

        for (i in seq_along(models)) {
            m = models[[i]]
            mn = names(models)[i]

#            for (pp in c(NA, 1e-4, 1e-10)) {
#                if (is.na(pp))
                    mod_tit = sprintf("%s (mode=%s, max posterior probability)",
                                      mn, config$comparisons[[mn]]$mode)
#                else {
#                    mod_tit = sprintf("%s (mode=%s, pp=%.0g)",
#                                      mn, config$comparisons[[mn]]$mode, pp)
#                    m = filter_peaks(m, pp)
#                }
                mod_desc = sprintf("%s; %s",
                                   paste(unique(m$info$mark), collapse=","),
                                   paste(unique(m$info$condition), collapse=","))
                print(plot_frequencies(m) +
                      labs(title=mod_tit, subtitle=mod_desc))

        #        print(plot_enrichment(model, ref="TSS"))
                ps = plot_enrichment(m, dset=dset, ref="gene")
                for (j in seq_along(ps)) {
                    tit = paste(names(ps)[j], mod_tit)
                    print(ps[[j]] + labs(title=tit, subtitle=mod_desc))
                }
#            }
        }

        dev.off()
    }
})
