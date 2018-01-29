.ops = import('../base/operators')

#' Intersect two GRanges objects
#'
#' See GenomicRanges/IRanges::findOverlaps for detailed arg docs
#'
#' @param query       GRanges object used as query
#' @param subject     GRanges object used as subject
#' @param type        Type of the overlap, eg. 'any', 'start', 'end', 'within'
#' @param minoverlap  Minimum number of positions that need to overlap to count
#' @param select      Which match to return (eg. 'first', 'last'; default: 'all')
#' @param keep_index  Return indices in query and subject
#' @param ...         Further arguments passed to findOverlaps
#' @return            Merged data.frame based on overlaps
intersect = function(query, subject, type="any", minoverlap=0L,
                     select="all", keep_index=FALSE, ...) {

    cols_query = GenomicRanges::mcols(query) %>%
        as.data.frame() %>%
        dplyr::mutate(queryHits = seq_len(nrow(.)))

    cols_subject = GenomicRanges::mcols(subject) %>%
        as.data.frame() %>%
        dplyr::mutate(subjectHits = seq_len(nrow(.)))

    re = GenomicRanges::findOverlaps(
        query, subject, type=type, minoverlap=minoverlap, select=select, ...)
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
