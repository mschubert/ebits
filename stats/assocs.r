# linear associations (anova-like)
.b = import('../base', attach_operators=FALSE)
.ar = import('../array')

assocs = function(formula, subsets=NULL, group=NULL, min_pts=3, p_adjust="fdr") {
    # check if all referenced variables are either matrix or vector
    # get data from parent.env
    formula_vars = all.vars(formula)
    data = sapply(formula_vars, function(x)
        base::get(x, envir=parent.env(environment())),
        USE.NAMES=TRUE, simplify=FALSE
    )

    vector_vars = formula_vars[sapply(formula_vars, function(x) is.vector(data[[x]]))]
    matrix_vars = formula_vars[sapply(formula_vars, function(x) is.matrix(data[[x]]))]

    diff = setdiff(formula_vars, c(vector_vars, matrix_vars))
    if (length(diff) > 0)
        stop(paste("All variables need to be vector or matrix:", diff))

    # check groups
    diff = setdiff(group, matrix_vars)
    if (length(diff) > 0)
        stop(paste("Grouped iterations only make sense for matrix vars:", diff))

    data
#    .ar$map(.assocs_subset) # how?
}

assocs_subset = function(formula, data, group=NULL, min_pts=3, p_adjust="fdr") {
    formula_vars = all.vars(formula)
    matrix_vars = formula_vars[sapply(formula_vars, function(x) is.matrix(data[[x]]))]

    # create a data.frame that provides indices for formula data given
    anchor = group[1]
    grouped = group[2:length(group)]
    ungrouped = setdiff(matrix_vars, grouped)
    index = do.call(expand.grid, sapply(ungrouped, function(x)
        .b$descriptive_index(data[[x]], along=2),
        USE.NAMES=TRUE, simplify=FALSE)
    )
    for (var in grouped)
        index[[var]] = index[[anchor]]

    # replace data by their subsets, call assocs function with all of them
    if (nrow(index) == 0) {
        .lm(formula, data=data)
    } else {
        for (index_i in 1:nrow(index)) {
            index_row = index[index_i,,drop=TRUE] # named list
            cur_data = data[setdiff(formula_vars, matrix_vars)]
            for (var in matrix_vars)
                cur_data[[var]] = data[[var]][,index_i]
#            .lm(formula, data=cur_data)
            print(cur_data)
        }
    }
}

# formula should contain single vars here, no matrices
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
