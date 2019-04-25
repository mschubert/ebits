.text = import('./text')$text

#' Plot a simple error message
#'
#' @param label  Character vector of the message
#' @return  empty ggplot2 object with only the label
error = function(label) {
    .Deprecated(msg="'error' plot is deprecated, use 'text'")
    .text(label)
}
