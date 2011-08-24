##' Panel function to draw correct grid on log-log plot
##'
##' <details>
##' @title My title
##' @param base base of logarithm
##' @return Nothing
##' @author Mikhail Titov
panel.grid.log <- function(base=10) {
    lim <- current.panel.limits()
    v <- latticeExtra:::logTicks(base^lim$xlim, loc=1)
    h <- latticeExtra:::logTicks(base^lim$ylim, loc=1)
    panel.abline(h=log(h, base), v=log(v, base), col="LightGray")
}
