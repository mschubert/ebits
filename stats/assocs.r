# linear associations (anova-like)
.b = import('../base', attach_operators=FALSE)
.ar = import('../array')
.spf = import('./process_formula')
`%catch%` = .b$`%catch%`

assocs = function(formula, subsets=NULL, group=NULL, min_pts=3, p_adjust="fdr", include_intercept=FALSE, data=parent.frame()) {
    pp = .spf$process_formula(formula, data)
    data = lapply(pp$data, as.matrix)
    formula = pp$form
    matrix_vars = names(data)[sapply(data, ncol) > 1]

    # check groups
    if (!is.null(group) && !is.character(group))
        stop("group needs to be NULL or a character vector")
    if (!all(group %in% all.vars(formula)))
        stop("group is referencing a variable not present in the formula")
    diff = setdiff(group, matrix_vars)
    if (length(diff) > 0)
        stop(paste("Grouped iterations only make sense for matrix vars:", diff))

    #TODO: p-adjust: group by term, adjust for each (or do this in subsets func)

    if (!is.null(subsets))
        # split in subsets, calc assocs for each of them, and rbind results
        re = data %>%
            lapply(function(d) .ar$split(d, along=1, subsets=subsets)) %>%
            .b$list$transpose(simplify=FALSE) %>%
            lapply(function(d) .assocs_subset(formula, d, group, min_pts)) %>%
            mapply(function(x,n) mutate(x, subset=n), x=., n=names(.), SIMPLIFY=FALSE) %>%
            unname() %>%
            do.call(rbind, .)
    else
        re = .assocs_subset(formula, data, group, min_pts)

    if (include_intercept)
        re
    else
        dplyr::filter(re, term != "(Intercept)")
}

.assocs_subset = function(formula, data, group=NULL, min_pts=3) {
    formula_vars = all.vars(formula)
    matrix_vars = formula_vars[sapply(formula_vars, function(x) ncol(data[[x]]) > 1)]

    # create a data.frame that provides indices for formula data given
    anchor = group[1]
    grouped = group[2:length(group)]
    ungrouped = setdiff(formula_vars, grouped)
    index = do.call(.b$expand_grid, sapply(ungrouped, function(x)
        .b$descriptive_index(data[[x]], along=2),
        USE.NAMES=TRUE, simplify=FALSE)
    )
    for (var in grouped)
        index[[var]] = index[[anchor]]

    # replace data by their subsets, call assocs function with all of them
    irow2result = function(i) {
        index_row = index[i,,drop=TRUE] # named list
        cur_data = data[setdiff(formula_vars, matrix_vars)]
        for (var in matrix_vars)
            cur_data[[var]] = data[[var]][,index_row[[var]]]
        cur_data = na.omit(as.data.frame(cur_data))
        if (nrow(cur_data) >= min_pts)
            .lm(formula, data=cur_data, params=index_row)
    }
    do.call(rbind, lapply(1:nrow(index), irow2result))
}

.lm = function(formula, data, params) {
    rownames(params) = NULL
    cbind(params, broom::tidy(lm(formula, data=data)), size=nrow(data))
}

.cox = function(formula) {
}

# this will be somewhat complicated
.sem = function(formula) {
}
