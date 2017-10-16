#' Ensembl REST server address
server = "http://rest.ensembl.org"

#' Send HTTP GET request
#'
#' @param uri  URI of the endpoint, or parts thereof as character vector
#' @param ...  Key-value pairs that will be encoded appropriately
#' @return     Named list of values
get = function(uri=c(), ...) {
    ext = paste(uri, collapse="/")
    url = paste(server, ext, sep="/")

    req = httr::GET(url, query=list(...),
                    httr::content_type("application/json"))

    if (!req$status == 200)
        stop("HTTP query error: expected 200 OK, got ", req$status)

    rjson::fromJSON(intToUtf8(req$content))
}

#' Send HTTP POST request
#'
#' @param uri  URI of the endpoint, or parts thereof as character vector
#' @param ...  Key-value pairs that will be encoded appropriately
#' @return     Named list of values
post = function(uri=c(), ...) {
    ext = paste(uri, collapse="/")
    url = paste(server, ext, sep="/")

    req = httr::POST(url, body=rjson::toJSON(list(...)),
                     httr::content_type("application/json"),
                     httr::accept("application/json"))

    if (!req$status == 200)
        stop("HTTP query error: expected 200 OK, got ", req$status)

    rjson::fromJSON(intToUtf8(req$content))
}
