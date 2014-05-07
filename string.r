ebits
=====

R modules for general reuse


#' Replace characters at certain positions of a string with another charachter.
#'
#' @param string String to be manipulated
#' @param pos One or more positions corresponding to charachters to be changed
#' @param char Replacement charachter
#' @keywords replace char
#' @export
#' @examples
#' replaceChar('ABC', 2, 'X')
replaceChar <- function(string, pos, char) { 
  for(i in pos)
    substr(string, i, i) <- char
  string 
} 

#' Extracts digits from a string and returns them in a numerical form
#'
#' @param string String to be manipulated
#' @keywords digits numerical
#' @export
#' @examples
#' extractDigits('A123F')
extractDigits <- function(string){
  as.numeric( gsub('[^\\d]', '', string, perl=T) )
}

#' Returns string without leading whitespace
#'
#' @param string String to be manipulated
#' @keywords trim string leading
#' @export
#' @examples
#' trim.leading( "  A  " )
trim.leading <- function (x)  sub("^\\s+", "", x)

#' Returns string without trailing whitespace
#'
#' @param string String to be manipulated
#' @keywords trim string trailing
#' @export
#' @examples
#' trim.trailing( "  A  " )
trim.trailing <- function (x) sub("\\s+$", "", x)

#' Returns string without trailing or leading whitespace
#'
#' @param string String to be manipulated
#' @keywords trim string trailing leading
#' @export
#' @examples
#' trim( "  A  " )
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

