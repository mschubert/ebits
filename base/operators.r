# some operators for R

`%or%` = function(a, b) {
    cmp = function(a,b) if (identical(a, FALSE) || 
                            is.null(a) || 
                            is.na(a) || 
                            is.nan(a) || 
                            length(a) == 0) b else a

    if (is.list(a))
        mapply(cmp, a, b, SIMPLIFY=F)
    else if (is.vector(a))
        mapply(cmp, a, b)
    else
        cmp(a, b)
}

`%OR%` = function(a, b) {
    tryCatch(
        a %or% b,
        error = function(e) b
    )
}

`%catch%` = function(a, b) {
    tryCatch(
        a,
        error = function(e) b
    )
}

`%|%` = function(x, command) {
    if (class(command) == 'function') {
        command(x)
    } else {
        stopifnot(class(x) %in% c('character', 'numeric', 'integer'))
        system(command, input=as.character(x), intern=TRUE)
    }
}

