##' Generates URL to fetch water data from USGS
##'
##' Generates URL to fetch water data from USGS web site \url{http://nwis.waterdata.usgs.gov/nwis}
##' @title url.make.USGS
##' @param uri URI list with at least path component
##' @param form list with GET form parameters
##' @return string representing url and suitable for use in corresponding function to create a connection
##' @seealso \code{\link{read.rdb.USGS}}
##' @author Mikhail Titov
##' @export
url.make.USGS <- function(uri, form) {
    uri.default <- list(
                        scheme="http",
                        host="nwis.waterdata.usgs.gov"
                        )
    uri <- modifyList(uri, uri.default)
    q <- paste(
               sapply(names(form), function(x) {
                   paste(x, form[[x]], sep="=")
               }, USE.NAMES = FALSE),
               collapse="&"
               )
    sprintf("%s://%s/%s?%s",
            uri[["scheme"]],
            uri[["host"]],
            uri[["path"]],
            q)
}
