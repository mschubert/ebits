library(dplyr)

#' Intersect two GRanges objects
#'
#' @param query    GRanges object used as query
#' @param subject  GRanges object used as subject
#' @return         Merged data.frame based on overlaps
intersect = function(query, subject) {
    cols_query = GenomicRanges::mcols(query) %>%
        as.data.frame() %>%
        mutate(queryHits = seq_len(nrow(.)))

    cols_subject = GenomicRanges::mcols(subject) %>%
        as.data.frame() %>%
        mutate(subjectHits = seq_len(nrow(.)))

    IRanges::findOverlaps(query, subject) %>%
        as.data.frame() %>%
        left_join(cols_query, by="queryHits") %>%
        left_join(cols_subject, by="subjectHits") %>%
        select(-queryHits, -subjectHits)
}
