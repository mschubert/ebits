#' Check if plot builds, replace with spacer otherwise
#'
#' @param ...  plots in parent environment
#' @param envir  which environment to save changed plots to (default: in-place)
#' @return  nothing, objects are modified in-place (or in envir)
build_or_spacer = function(..., envir=parent.frame()) {
    dots = pryr::named_dots(...)
    for (pn in names(dots))
        if (class(try(ggplot_build(dots[[pn]]))) == "try-error")
            assign(pn, patchwork::plot_spacer(), envir=envir)
}
