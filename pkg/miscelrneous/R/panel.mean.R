##' Panel function to add mean to bwplot
##'
##' <details>
##' @title My title
##' @param x
##' @param y
##' @param ...
##' @param horizontal passed from bwplot
##' @return Nothing
##' @author Mikhail Titov
##' @references \url{https://stat.ethz.ch/pipermail/r-help/2005-November/081980.html}
##' @export
panel.mean <- function(x, y, ..., horizontal) {
    if (horizontal) {
        tmp <- tapply(x, y, FUN = mean)
        panel.points(tmp, seq(tmp), ...)
    } else {
        tmp <- tapply(y, x, FUN = mean)
        panel.points(seq(tmp), tmp, ...)
    }
}
