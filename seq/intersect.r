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
#' @param return_widths  Return width of match and fraction of query covered
#' @param keep_index  Return indices in query and subject
#' @param ...         Further arguments passed to findOverlaps
#' @return            Merged data.frame based on overlaps
intersect = function(query, subject, type="any", minoverlap=0L, select="all",
                     return_widths=FALSE, keep_index=FALSE, ...) {

    cols_query = GenomicRanges::mcols(query) %>%
        as.data.frame() %>%
        dplyr::mutate(seqnames = as.character(GenomicRanges::seqnames(query)),
                      queryHits = seq_len(nrow(.)))

    cols_subject = GenomicRanges::mcols(subject) %>%
        as.data.frame() %>%
        dplyr::mutate(subjectHits = seq_len(nrow(.)))

    re = GenomicRanges::findOverlaps(
        query, subject, type=type, minoverlap=minoverlap, select=select, ...)

    re = re %>%
        as.data.frame() %>%
        dplyr::left_join(cols_query, by="queryHits") %>%
        dplyr::left_join(cols_subject, by="subjectHits")

    if (return_widths) {
        overlaps = GenomicRanges::pintersect(
            query[re$queryHits], subject[re$subjectHits])

        re = re %>%
            dplyr::mutate(query_width = GenomicRanges::width(query[re$queryHits]),
                ov_frac = GenomicRanges::width(overlaps) / query_width)
    }

    if (keep_index)
        re
    else
        dplyr::select(re, -queryHits, -subjectHits)
}
