##' Panel function to draw correct grid on log-log plot
##'
##' <details>
##' @title Nice log grid
##' @param base base of logarithm
##' @return Nothing
##' @author Mikhail Titov
##' @export
panel.grid.log <- function(base=10, x=TRUE, y=TRUE) {
    lim <- current.panel.limits()
    v <- if (x)
        log(latticeExtra:::logTicks(base^lim$xlim, loc=1), base)
    else
        pretty(lim$xlim)
    h <- if (y)
        log(latticeExtra:::logTicks(base^lim$ylim, loc=1), base)
    else
        pretty(lim$ylim)
    panel.abline(h=h, v=v, col="LightGray", lty=2)
}
