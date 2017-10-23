import('../../base/operators')

#' Function to calculate and return a cleaned Cox proportional hazards model
#'
#' @param formula  A formula specifying the relationship between variables
#' @param data     A data.frame, list, or by default the formula env containing vars      
#' @param min_pts  Minimum number of points to create a model for
#' @param atomic_class  Variable classes that should not be split by default
#' @return         A data.frame containing the model result
coxph = function(formula, data=environment(formula), min_pts=3, atomic_class='vector') {
    pts = model.frame(formula, data)
    size = lapply(pts, function(x) {
        # could potentially convert to factor first as well
        if (is.logical(x) || is.factor(x) || is.character(x))
            as.list(table(x))
        else
            sum(!(is.na(x) | x==0))
    })
    names(size) = names(pts)

    orig_names = names(size)
    size = unlist(size)
    for (n in orig_names)
        names(size) = sub(paste0(n,"\\."), n, names(size))

    #TODO: should drop covariates if unique, only if nothing left drop regression
    # (and display a warning of what was dropped)
    # also, drop factor levels individually if < min_pts
    if (nrow(pts) < min_pts)
        return(NULL)

	fstr = strsplit(sub("\\+", ",", deparse(formula)), "~")[[1]]
	formula = formula(paste("survival::Surv(", fstr[1], ") ~", fstr[-1]))
	survival::coxph(formula, data) %>%
		broom::tidy() %>%
		cbind(size = sapply(.$term, function(x) size[x]))
#		cbind(size = pts) %catch% NULL #FIXME: there must be a better way to do this
}

if (is.null(module_name())) {
    test1 = list(time=c(4,3,1,1,2,2,3),
                   status=c(1,1,1,0,1,1,0),
                   x=c(0,2,1,1,1,0,0),
                   sex=c(0,0,0,0,1,1,1))

	result = coxph(time + status ~ x + sex, data=test1)
}
