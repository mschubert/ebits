# I/O helper functions on text files
.b = import('../base', attach_operators=F)

#' Add \code{ext}ension parameter to \link{\code{base::file.path}}
file_path = function (..., ext = NULL, fsep = .Platform$file.sep) {
    dots = list(...)
    if (! is.null(ext)) {
        ilast = length(dots)
        dots[ilast] = sprintf('%s.%s', dots[ilast], sub('^\\.', '', ext))
    }

    do.call(base::file.path, c(dots, fsep = fsep))
}

.set_defaults = function (call, .formals = formals(sys.function(sys.parent()))) {
    for (n in names(.formals))
        if (n != '...' && ! (n %in% names(call)))
            call[[n]] = .formals[[n]]

    call
}

#' Augment \code{\link{utils::read.table}} by a mechanism to guess the file format.
#'
#' For the moment, only separators are handled based on the file extension.
#' This might change in the future to be more powerful, think Python’s
#' \code{csv.Sniffer} class.
read_table = function (file, ..., stringsAsFactors = FALSE, na.strings=c(NA, ""), check.names=FALSE) {
    call = .set_defaults(match.call(expand.dots = TRUE))

    if (missing(file)) {
        call[[1]] = quote(read.table)
        return(eval.parent(call))
    }

    extension = .b$grep('\\.(\\w+)(\\.gz)?$', file)[1]

    if (! ('sep' %in% names(call))) {
        separators = list(csv = ',',
                          tsv = '\t',
                          txt = '\t')
        call$sep = separators[[extension]]
    }

    call[[1]] = if (identical(extension, 'xlsx'))
        quote(xlsx::read.xlsx) else quote(read.table)
    eval.parent(call)
}

write_table = function (x, file = '', quote = FALSE, row.names = FALSE) {
    call = .set_defaults(match.call(expand.dots = TRUE))
    call[[1]] = quote(write.table)

    if (file != '' && ! 'sep' %in% names(call)) {
        extension = .b$grep('\\.(\\w+)(\\.gz)?$', file)[1]
        separators = list(csv = ',',
                          tsv = '\t',
                          txt = '\t')
        call$sep = separators[[extension]]
    }

    eval.parent(call)
}

read_full_table = function(file, sep="\t", stringsAsFactors=FALSE, check.names=FALSE, ...) {
    index = utils::read.table(file, sep=sep, stringsAsFactors=stringsAsFactors, ...)
    colnames(index) = index[1,]
    rownames(index) = index[,1]
    index[-1,-1]
}
