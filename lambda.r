import('./functional', attach = TRUE)

#' Define a single-argument function
#'
#' @note \textbf{Warning} This breaks \code{<-} assignment because it overloads
#' the operator. \code{=} assignment \textbf{must} be used instead.
#' The rationale for introducing this redefinition and breaking existing code is
#' two-fold:
#'
#' \begin{enumerate}
#'  \item R is a functional programming language, yet has no concise way of
#'      defining an anonymous function. This seriously prevents good code. There
#'      are ways of alleviating this (e.g. by using the \code{.} function) but
#'      none of these approaches the ease of using anonymous functions in other
#'      modern functional (or even non-functional) languages.
#'  \item Even though there’s a general preference in the R community of using
#'      \code{<-} over \code{=} for assignment, the truth is that there is no
#'      single compelling argument for this preference, other than history, and
#'      the need for concise lambdas clearly overrides a whimsical preference of
#'      one redundant operator over another.
#' \end{enumerate}
`<-` = function (body, param) {
    eval(substitute(f(param -> body), list(f = f, param = substitute(param),
                                           body = substitute(body))),
         envir = parent.frame())
}
