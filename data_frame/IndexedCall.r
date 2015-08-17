R6 = import_package('R6')

#' Class to represent a series of function calls
IndexedCall = R6$R6Class("IndexedCall",
    public = list(
        index = data.frame(),
        args = list(),
        subsets = NULL,

        #' Function to create a call index and constant call variables
        #'
        #' If there is a formula in `index`, parse variables referenced :<group>
        #' and put them into index
        #'
        #' @param index  Variables that should be indexed
        #' @param ...    Other Variables
        initialize = function(index, args=list(), subsets=NULL) {
            self$index = index
            self$args = args
            self$subsets = subsets
        },

        #' Display the contents
        show = function() {
            cat("Indexed values:\n")
            cat("data.frame with", ncol(self$index), "columns and", nrow(self$index), "rows\n")
            cat(head(self$index,10))
            if (length(args) > 0) {
                cat("\nConstant args:\n")
                cat(self$args)
            }
            if (!is.null(self$subsets)) {
                cat("Subsets:")
                cat(table(self$subsets))
            }
        }
    )
)

#' Derived indexed class to handle formulas
IndexedFormula = R6$R6Class("IndexedFormula",
    inherit = IndexedCall
)
