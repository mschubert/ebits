# this should take a formula and return a data.frame
#  problem is, formulas can reference either names or calls
#  lm() takes care of resolving this, but can it do multiple cols
#    in both dep in indep vars?

# the data.frame returned could modify the calls to get the right data
#   problem with that is, it's not self-contained (no hpc$Q)
# alternatively, get all the data needed and append it somehow
#   here I can transform it at will, ie. make matrices
#   could add as a 'data' attribute on the df
# could i maybe use a grouped data_frame for this?

# start with st$.assocs_subset here, leave the row-wise calls to df$call
.b = import('base')
.gfd = import('./get_formula_data')

from_formula = function(formula, data=parent.frame(), group=NULL) {
    pp = .gfd$get_formula_data(formula, data)
    data = lapply(pp$data, as.matrix)
    formula = pp$form
    formula_vars = all.vars(formula)
    matrix_vars = names(data)[sapply(data, ncol) > 1]

    # check groups
    if (!is.null(group) && !is.character(group))
        stop("group needs to be NULL or a character vector")
    if (!all(group %in% all.vars(formula)))
        stop("group is referencing a variable not present in the formula")
    diff = setdiff(group, matrix_vars)
    if (length(diff) > 0)
        stop(paste("Grouped iterations only make sense for matrix vars:", diff))

    # create a data.frame that provides indices for formula data given
    # it might be easier to construct full df and remove non-groups?
    anchor = group[1]
    grouped = group[2:length(group)]
    ungrouped = setdiff(formula_vars, grouped)
    index = do.call(.b$expand_grid, sapply(ungrouped, function(x)
        .b$descriptive_index(data[[x]], along=2),
        USE.NAMES=TRUE, simplify=FALSE)
    )
    for (var in grouped)
        index[[var]] = index[[anchor]]

    attr(index, "data") = data
    attr(index, "formula") = formula
    class(index) = append(class(index),"attrs_as_args")
    index
}
