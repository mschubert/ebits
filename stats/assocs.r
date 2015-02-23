# linear associations (anova-like)
.b = import('../base', attach_operators=FALSE)
.ar = import('../array')

assocs = function(formula, subsets=NULL, group=NULL, min_pts=3, p_adjust="fdr") {
    # get data from parent.env
    formula_vars = all.vars(formula)
    data = sapply(formula_vars, function(x)
        as.matrix(base::get(x, envir=parent.env(environment()))),
        USE.NAMES=TRUE, simplify=FALSE
    )

    matrix_vars = formula_vars[sapply(formula_vars, function(x) ncol(data[[x]] > 1))]

    # check groups
    diff = setdiff(group, matrix_vars)
    if (length(diff) > 0)
        stop(paste("Grouped iterations only make sense for matrix vars:", diff))

    data
#    .ar$map(.assocs_subset) # get all subset dfs, merge w/ additional column
}

assocs_subset = function(formula, data, group=NULL, min_pts=3, p_adjust="fdr") {
    formula_vars = all.vars(formula)
    matrix_vars = formula_vars[sapply(formula_vars, function(x) ncol(data[[x]]) > 1)]

    # create a data.frame that provides indices for formula data given
    anchor = group[1]
    grouped = group[2:length(group)]
    ungrouped = setdiff(formula_vars, grouped)
    index = do.call(expand.grid, sapply(ungrouped, function(x)
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
            cur_data[[var]] = data[[var]][,i]
        .lm(formula, data=cur_data)
    }
    lapply(1:nrow(index), irow2result)
}

.lm = function(formula, data) {
    fit = lm(formula, data=data) %catch% NA
# effect/pvalue for all variables in there?
# could just return (tidy) result object and let user filter it after
}

.cox = function(formula) {
}

# this will be somewhat complicated
.sem = function(formula) {
}
