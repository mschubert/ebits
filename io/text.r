# I/O helper functions on text files
.b = import_('../base', attach_operators = FALSE)

#' Add \code{ext}ension parameter to \link{\code{base::file.path}}
file_path = function (..., ext = NULL, fsep = .Platform$file.sep) {
    dots = list(...)
    if (! is.null(ext)) {
        ilast = length(dots)
        dots[ilast] = sprintf('%s.%s', dots[ilast], sub('^\\.', '', ext))
    }

    do.call(base::file.path, c(dots, fsep = fsep))
}

#' Augment \code{\link{utils::read.table}} by a mechanism to guess the file format.
#'
#' For the moment, only separators are handled based on the file extension.
#' This might change in the future to be more powerful, think Python’s
#' \code{csv.Sniffer} class.
read_table = function (file, ..., stringsAsFactors = FALSE, na.strings = c(NA, ''), check.names = FALSE) {
    call = .b$match_call_defaults()

    if (missing(file) || is_connection(file)) {
        call[[1]] = quote(read.table)
        return(.b$add_class(eval.parent(call), 'tbl_df'))
    }

    extension = .b$grep('\\.(\\w+)(\\.gz)?$', file)[1]

    if (! ('sep' %in% names(call))) {
        separators = list(csv = ',',
                          tsv = '\t',
                          txt = '\t')
        call$sep = separators[[extension]]
    }

    call[[1]] = if (identical(extension, 'xlsx')) {
        call$na.strings = NULL
        quote(xlsx::read.xlsx)
    } else quote(read.table)
    .b$add_class(eval.parent(call), 'tbl_df')
}

write_table = function (x, file = '', ..., quote = FALSE, row.names = FALSE) {
    call = .b$match_call_defaults()
    call[[1]] = quote(write.table)

    if (! is_connection(file) && file != '' && ! 'sep' %in% names(call)) {
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

is_connection = function (x) {
    inherits(x, 'connection')
}
