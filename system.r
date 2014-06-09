#' Silence all output from an expression by redirecting the sink
silent <- function (.expr) {
    on.exit(sink())
    sink(file = (if (Sys.info()['sysname'] == 'Windows') 'NUL' else '/dev/null'))
    eval(.expr, envir = parent.frame())
}
