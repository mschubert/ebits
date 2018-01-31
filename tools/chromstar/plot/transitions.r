import_package('ggplot2', attach=TRUE)

#' Plot transition probabilities for model
#'
#' @param model  Chromstar model object
transitions = function(model) {
    chromstaR::heatmapTransitionProbs(model) +
        ggtitle('Transition probabilities')
}
