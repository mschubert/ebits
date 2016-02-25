library(dplyr)

#' Computes precision-recall statistics
#'
#' @param df     A data.frame containing the fields value and hit
#' @param value  The numeric value associated with the score
#' @param hit    Whether (1) or not (0) the observation is a success
roc = function(df, value, hit) {
    mydf = data.frame(value = df[[value]], hit = df[[hit]], check.names=FALSE) %>%
        mutate(i = 1:nrow(.),
               value = value - min(value)) %>%
        mutate(value = value / max(value)) %>%
        arrange(-value) %>%
        mutate(TPR = cumsum(hit) / sum(hit),
               FPR = cumsum(1-hit) / sum(1-hit)) %>%
        arrange(i)

    df$FPR = mydf$FPR
    df$TPR = mydf$TPR
    df
}
