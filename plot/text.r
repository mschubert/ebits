#' Plot a simple text message
#'
#' @param label  Character vector of the message
#' @param ...    Paramters passed to geom_text
#' @return  empty ggplot2 object with only the label
text = function(label, ...) {
    ggplot(data.frame(x=1, y=1)) +
        geom_text(aes(x=x, y=y), label=label, ...) +
        theme_void()
}
