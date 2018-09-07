#' Plot a simple error message
#'
#' @param label  Character vector of the message
#' @return  empty ggplot2 object with only the label
error = function(label) {
    ggplot(data.frame()) +
        geom_text(x=1, y=1, label=label) +
        theme_void()
}
