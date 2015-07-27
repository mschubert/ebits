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
.b = import('../base', attach_operators=FALSE)
source(file.path(module_file(), "IndexedCall.r"))
#.b = import('../base')
.gfd = import('./get_formula_data')

#' Gathers all data required for a formula and creates a subsetting index
#'
#' @param formula  A standard R formula
#' @param data     Where to look for the data the `formula` references
#' @param group    Names of variables that should be column-interated together
#' @param subsets  How to divide each variable in `formula` along first axis
#' @param atomic   Names of variables that should not be iterated through
create_formula_index = function(formula, data=parent.frame(), group=NULL, subsets=NULL, atomic=NULL) {
    pp = .gfd$get_formula_data(form=formula, data=data)
    data = lapply(pp$data, as.matrix)
    formula = pp$form
    formula_vars = setdiff(all.vars(formula), atomic)
    matrix_vars = names(data)[sapply(data, ncol) > 1]

    # allow groups only when they make sense
    if (!is.null(group) && !is.character(group))
        stop("group needs to be NULL or a character vector")
    if (!all(group %in% all.vars(formula)))
        stop("group is referencing a variable not present in the formula")
    diff = setdiff(group, matrix_vars)
    if (length(diff) > 0)
        stop(paste("Grouped iterations only make sense for matrix vars:", diff))

    # define anchor to iterate only non-grouped variables first
    anchor = group[1]
    grouped = group[2:length(group)]
    ungrouped = setdiff(formula_vars, grouped)

    # get all individual indices, then expand.grid around them
    index_items = sapply(ungrouped, function(x)
        .b$descriptive_index(data[[x]], along=2),
        USE.NAMES=TRUE, simplify=FALSE
    )
    index_items$subset = unique(subsets)
    index = do.call(.b$expand_grid, index_items)

    # add grouped items to be the same as the var they are grouped with
    for (var in grouped)
        index[[var]] = index[[anchor]]

    # add data as attribute
    new("IndexedFormula",
        index=index,
        args = list(data=data, formula=formula),
        subsets=subsets)
}

if (is.null(module_name())) {
    A = matrix(1:4, nrow=2, ncol=2, dimnames=list(c('a','b'),c('x','y')))
    B = matrix(5:6, nrow=2, ncol=1, dimnames=list(c('b','a'),'z'))
    C = matrix(4:5, nrow=2, ncol=2)
    # (A)   x y    (B)   z    (C)     [,1] [,2]
    #     a 1 3        b 5      [1,]    4    4
    #     b 2 4        a 6      [2,]    5    5

    x1 = create_formula_index(A ~ B + C)
    #   A B C
    # 1 x z 1
    # 2 y z 1
    # 3 x z 2
    # 4 y z 2

    x2 = create_formula_index(A ~ B + C, group=c("A","C"))
    #   A B C
    # 1 x z x
    # 2 y z y

    x3 = create_formula_index(A ~ B + C, group=c("A","C"), subsets=c('w','o'))
    #   A B subset C
    # 1 x z      w x
    # 2 y z      w y
    # 3 x z      o x
    # 4 y z      o y

    testthat::expect_equal(x1@args$data, x2@args$data)
    testthat::expect_equal(x2@args$data, x3@args$data)

    testthat::expect_equal(x1@index,
    structure(list(A = c("x", "y", "x", "y"), B = c("z", "z", "z",
    "z"), C = c(1L, 1L, 2L, 2L)), .Names = c("A", "B", "C"), class = "data.frame",
    row.names = c(NA, -4L)))

    testthat::expect_equal(x2@index,
    structure(list(A = c("x", "y"), B = c("z", "z"), C = c("x", "y"
    )), .Names = c("A", "B", "C"), row.names = c(NA, -2L), class = "data.frame"))

    testthat::expect_equal(x4@index,
    structure(list(A = c("x", "y", "x", "y"), B = c("z", "z", "z",
    "z"), subset = c("w", "w", "o", "o"), C = c("x", "y", "x", "y"
    )), .Names = c("A", "B", "subset", "C"), row.names = c(NA, -4L
    ), class = "data.frame"))
}
