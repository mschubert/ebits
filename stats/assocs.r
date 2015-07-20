.b = import('../base')
.ar = import('../array')
.df = import('../data_frame')

lm = function(formula, data=environment(formula), min_pts=3, return_intercept=FALSE) {
    pts = nrow(na.omit(do.call(cbind, data)))
    if (pts < min_pts)
        NULL
    else {
        re = stats::lm(formula, data) %>%
            broom::tidy() %>%
            cbind(size = pts)
        if (return_intercept)
            re
        else
            dplyr::filter(re, term != "(Intercept)")
    }
}

coxph = function(formula, data=environment(formula), min_pts=3) {
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
# test the basic model functions here (wrap them when exporting)
}
