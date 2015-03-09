#' Return matching indices comparing multiple columns
#'
#' @param haystack  data.frame to search for matches in
#' @param needle    data.frame to use as query
#' @param cols      columns for matching
match = function(haystack, needle, cols=intersect(colnames(haystack), colnames(needle))) {
    haystack = haystack[cols]
    needle = needle[cols]

    if ('haystack' %in% cols || 'needle' %in% cols)
        stop("data.frame's can not have the column names 'haystack' or 'needle'")

    haystack$haystack = 1:nrow(haystack)
    needle$needle = 1:nrow(needle)

    merge(needle, haystack, by=cols, all=FALSE)[c('haystack','needle')]
}

#' Subset a data.frame with a data.frame, always comparing characters
#'
#' @param df            data.frame to update
#' @param subs          data.frame to use for subsetting
#' @param one_per_row   restrict to unique matches per row of upd
#' @param add_cols      whether to add columns that are in subs but not df
subset = function(df, subs, one_per_row=FALSE, add_cols=FALSE) {
    indices = match(df, subs)

    if (one_per_row && any(duplicated(indices$haystack)))
        stop('matching does not yield exactly one per row')

    re = df[indices$haystack,]

    if (add_cols)
        cbind(re, subs[indices$needle, setdiff(colnames(subs), colnames(df)), drop=FALSE])
    else
        re
}

#' Update a data.frame with new values
#'
#' @param df            data.frame to update
#' @param upd           data.frame to use for updating
#' @param match_cols    column names that need to match in order to update
#' @param one_per_row   restrict to unique matches per row of upd
#' @param add_cols      whether to add columns that are in subs but not df
update = function(df, upd, match_cols, one_per_row=FALSE, add_cols=TRUE) {
    indices = match(df, upd, cols=match_cols)

    if (one_per_row && any(duplicated(indices$haystack)))
        stop('matching does not yield exactly one per row')

    cols = setdiff(colnames(upd), match_cols)
    if (add_cols) {
        new_cols = setdiff(colnames(upd), colnames(df))
        for (col in new_cols)
            df[[col]] = rep(NA, nrow(df))
    }

    df[indices$haystack, cols] = upd[indices$needle, cols]
    df
}
