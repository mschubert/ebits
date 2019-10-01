.text = import('./text')$text

#' Attempt to build plot, show error if it fails
#'
#' @param p  ggplot2 object
#' @return   ggplot2 object that does not produce errors
try = function(p) {
    built = try(ggplot2::ggplot_build(p))
    if (class(built) == "try-error")
        .text(x)
    else
        p
}
