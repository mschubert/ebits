# Sequence analysis basics

#' Reverse complement sequences
revcomp = function (strs) {
    rc = function (str) {
        bases = strsplit(str, '')[[1]]
        compl = vapply(bases, partial(switch, A='T', C='G', G='C', T='A'), '')
        paste(rev(compl), collapse = '')
    }
    vapply(strs, rc, '')
}
