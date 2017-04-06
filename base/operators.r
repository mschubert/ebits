# some operators for R

`%>%` = magrittr::`%>%`

`%$%` = magrittr::`%$%`

#' If `lhs` is `FALSE`, `NULL`, `NA`, etc. return `rhs`
`%or%` = function(a, b) {
    cmp = function(a,b) if (identical(a, FALSE) || 
                            is.null(a) || 
                            is.na(a) || 
                            is.nan(a) || 
                            length(a) == 0 ||
                            nchar(a) == 0) b else a

    if (is.list(a))
        lapply(1:length(a), function(i) cmp(a[[i]], b[[i]]))
    else if (length(a) > 1) #TODO: does that do what we want?
        mapply(cmp, a, b)
    else
        cmp(a, b)
}

#' `%or%` that also catches errors
`%OR%` = function(a, b) {
    tryCatch(
        a %or% b,
        error = function(e) b
    )
}

#' If `lhs` produces an error, return `rhs`
`%catch%` = function(a, b) {
    tryCatch(
        a,
        error = function(e) b
    )
}

#' If `lhs` produces a warning, return `rhs`
`%catchw%` = function(a, b) {
    tryCatch(
        a,
        warning = function(w) b
    )
}

#' If `lhs` writes a message, return `rhs`
`%catchm%` = function(a, b) {
    withCallingHandlers(
        a,
        message = function(w) b
    )
}
