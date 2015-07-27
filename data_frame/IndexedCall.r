.b = import('../base', attach_operators=FALSE)

#' Class to represent a series of function calls
IndexedCall = setClass(
    "IndexedCall",

    slots = c(
        index = "data.frame",
        args = "list"
    )
)

#' Function to create a call index and constant call variables
#'
#' If there is a formula in `index`, parse variables referenced :<group>
#' and put them into index
#'
#' @param index  Variables that should be indexed
#' @param ...    Other Variables
setMethod("initialize", signature(.Object="IndexedCall"),
    function(.Object, ..., args=list(), expand_grid=FALSE) {
#TODO: if ... == index df, don't process further
        if (expand_grid)
            index = .b$expand_grid(...)
        else
            index = dplyr::as_data_frame(list(...))

        # set class so that our object can handle it
        attr(index, "class") = "data.frame"
        callNextMethod(.Object, index=index, args=args)
})

#' Display the contents
setMethod("print", "IndexedCall", definition = function (x, ...) { 
    cat("Indexed values:\n")
    callNextMethod(x = x@index)
    cat("Constant args:\n")
    callNextMethod(x = x@args)
})

#' Simple access to print method
setMethod("show", signature(object="IndexedCall"), function(object) {
    print(object)
})

##########################################################################

.ff = import('./from_formula')

#' Derived indexed class to handle formulas
IndexedFormula = setClass(
    "IndexedFormula",

    contains = "IndexedCall",

    slots = c(
        subsets = "character"
    )
)

#' Function to create a call index and constant call variables
#'
#' If there is a formula in `index`, parse variables referenced :<group>
#' and put them into index
#'
#' @param index  Variables that should be indexed
#' @param ...    Other Variables
setMethod("initialize", signature(.Object="IndexedFormula"),
    function(.Object, formula, data=environment(formula), subsets=NULL, ...) {
        index = .ff$from_formula(formula, data, subsets=subsets)
#        args = c(list(...), attr(index, "args"))
        args = attr(index, "args")
        subsets = args$subsets
        args$subsets = NULL
        attr(index, "args") = NULL
        class(index) = "data.frame"
        callNextMethod(.Object, index=index, args=args, subsets=subsets)
})
