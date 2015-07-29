#' Class to represent a series of function calls
IndexedCall = setClass(
    "IndexedCall",

    slots = c(
        index = "data.frame",
        args = "list",
        subsets = "ANY"
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
    function(.Object, index, args=list(), subsets=NULL) {
        callNextMethod(.Object, index=index, args=args, subsets=subsets)
})

#' Display the contents
setMethod("print", "IndexedCall", definition = function (x, ...) { 
    cat("Indexed values:\n")
    cat("data.frame with", ncol(x@index), "columns and", nrow(x@index), "rows\n")
    callNextMethod(x = head(x@index,10))
    cat("\nConstant args:\n")
    callNextMethod(x = x@args)
    if (!is.null(subsets))
    cat("Subsets:")
    callNextMethod(x = table(x@subsets))
})

#' Simple access to print method
setMethod("show", signature(object="IndexedCall"), function(object) {
    print(object)
})

##########################################################################

#' Derived indexed class to handle formulas
IndexedFormula = setClass(
    "IndexedFormula",

    contains = "IndexedCall",
)

#' Function to create a call index and constant call variables
#'
#' If there is a formula in `index`, parse variables referenced :<group>
#' and put them into index
#'
#' @param index  Variables that should be indexed
#' @param ...    Other Variables
setMethod("initialize", signature(.Object="IndexedFormula"),
    function(.Object, index, args, subsets=NULL) {
        callNextMethod(.Object, index=index, args=args, subsets=subsets)
})
