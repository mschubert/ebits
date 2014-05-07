# some operators for R

`%or%` = function(a, b) {
    if (identical(a, FALSE) || is.null(a) || is.na(a) || is.nan(a) || length(a) == 0) b else a
}

`%OR%` = function(a, b) {
    tryCatch(
        a %or% b,
        error = function(cond) b
    )
}

`%catch%` = function(a, b) {
    tryCatch(a,
             error = function(cond) b)
}

`%|%` = function(x, command) {
    if (class(command) == 'function') {
        command(x)
    } else {
        stopifnot(class(x) %in% c('character', 'numeric', 'integer'))
        system(command, input=as.character(x), intern=TRUE)
    }
}

`%|=%` = function(a, b) {
    a <<- a %OR% b
}

