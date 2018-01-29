.ops = import('../base/operators')

#' Intersect two GRanges objects
#'
#' @param query       GRanges object used as query
#' @param subject     GRanges object used as subject
#' @param keep_index  Return indices in query and subject
#' @return            Merged data.frame based on overlaps
intersect = function(query, subject, keep_index=FALSE) {
    cols_query = GenomicRanges::mcols(query) %>%
        as.data.frame() %>%
        dplyr::mutate(queryHits = seq_len(nrow(.)))

    cols_subject = GenomicRanges::mcols(subject) %>%
        as.data.frame() %>%
        dplyr::mutate(subjectHits = seq_len(nrow(.)))

    re = IRanges::findOverlaps(query, subject)
#    re$width = IRanges::ranges(re, query, subject)$width
    re = re %>%
        as.data.frame() %>%
        dplyr::left_join(cols_query, by="queryHits") %>%
        dplyr::left_join(cols_subject, by="subjectHits")

    if (keep_index)
        re
    else
        dplyr::select(re, -queryHits, -subjectHits)
}
