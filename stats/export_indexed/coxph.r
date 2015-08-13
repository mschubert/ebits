import('../../base/operators')

#' Function to calculate and return a cleaned Cox proportional hazards model
#'
#' @param formula  A formula specifying the relationship between variables
#' @param data     A data.frame, list, or by default the formula env containing vars      
#' @param min_pts  Minimum number of points to create a model for
#' @param atomic_class  Variable classes that should not be split by default
#' @return         A data.frame containing the model result
coxph = function(formula, data=environment(formula), min_pts=3, atomic_class='vector') {
    pts = nrow(na.omit(do.call(cbind, data)))
    if (pts < min_pts)
        NULL
    else {
        fstr = strsplit(sub("\\+", ",", deparse(formula)), "~")[[1]]
        formula = formula(paste("survival::Surv(", fstr[1], ") ~", fstr[-1]))
        survival::coxph(formula, data) %>%
            broom::tidy() %>%
            cbind(size = pts)
    }
}

if (is.null(module_name())) {
}
