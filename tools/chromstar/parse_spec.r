import_package('dplyr', attach=TRUE)

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
