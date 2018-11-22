#' Function that extracts the data a formula references
#'
#' @param from  The formula
#' @param data  A superset of the data; default: calling environment
get_formula_data = function(form, data=parent.frame()) {
    acall = R6::R6Class("call",
        public = list(
            initialize = function(sym, name, str) {
                self$sym = sym
                self$name = name
                self$str = str
            },
            sym = NULL,
            name = NULL,
            str = NULL
        )
    )

    extract_calls = function(f, ops=c("~","+",":","*")) {
        if (length(f) > 1 && as.character(f[[1]]) %in% ops)
            sapply(f[2:length(f)], extract_calls)
        else {
            if (is.call(f))
                name = paste(f[2:length(f)], collapse="_")
            else
                name = as.character(f)
            acall$new(f, name, deparse(f))
        }
    }

    # extract all names/calls that are atomic for the formula
    vars = unlist(extract_calls(form))
    vars = vars[!sapply(vars, function(v) is.numeric(v$sym))] # exclude "0+" from parsing

    # go through vars, create data list w/ all evals
    processed_data = lapply(vars, function(v)
        if (v$str %in% names(data))
            base::eval(v$sym, envir=data)
        else
            base::eval(v$sym, envir=environment(form))
    )
    names(processed_data) = sapply(vars, function(v) v$name)

    # go through formula, replace every call by name
    form_str = deparse(form)
    for (v in vars)
        form_str = gsub(v$str, v$name, form_str, fixed=TRUE)

    list(form=formula(form_str), data=processed_data)
}

if (is.null(module_name())) {
    library(testthat)

    time = c(4,3,1,1,2,2,3)
    status = c(1,0,1,0,1,1,0)

    # directly reference data
    form = status ~ time
    d1 = get_formula_data(form)
    expect_equal(d1$form, form)
    expect_equal(d1$data$time, time)
    expect_equal(d1$data$status, status)

    test = list(time = c(4,3,1,1,2,2,2),
                x = c(0,2,1,1,1,0,0),
                sex = c(0,0,0,0,1,1,1))

    # provide data, prefer over formula env (fallback)
    d2 = get_formula_data(status ~ time, data=test)
    expect_equal(d2$form, form)
    expect_equal(d2$data$time, test$time)
    expect_equal(d2$data$status, status)

    # syntax for survival fits
    d3 = get_formula_data(status + time ~ 0 + x, data=test)
    expect_false("0" %in% names(d3$data))
    expect_equal(d3$form, status + time ~ 0 + x)
    expect_equal(d3$data$time, test$time)
    expect_equal(d3$data$status, status)
}
