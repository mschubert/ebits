library(dplyr)

#' Computes precision-recall statistics
#'
#' @param df     A data.frame containing the fields value and hit
#' @param value  The numeric value associated with the score
#' @param hit    Whether (1/TRUE) or not (0/FALSE) the observation is a success
roc = function(df, value, hit) {
    if (any(is.na(df[[value]]) | is.na(df[[hit]])))
        stop("data.frame contains NAs, remove them first")

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

#' Computes precision-recall AUC
#'
#' @param df         A data.frame containing the fields value and hit
#' @param value      The numeric value associated with the score
#' @param hit        Whether (1/TRUE) or not (0/FALSE) the observation is a success
#' @param symmetric  count wrong sign as right (default:FALSE)
#' @return           The area under the curve
roc_auc = function(value, hit, symmetric=FALSE) {
    pROC::auc(as.logical(hit), c(1:length(value))/length(value),
              direction = ifelse(symmetric, "auto", "<"))
}
