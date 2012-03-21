##' Plots periodogram
##'
##' <details>
##' @title Periodogram plot
##' @param s
##' @return trellis object
##' @author Mikhail Titov
##' @examples
##' x <- EuStockMarkets[,"FTSE"]
##' a <- start(EuStockMarkets)
##' b <- end(EuStockMarkets)
##' ti <- seq(ISOdate(a[1],1,1)+a[2]*86400,
##'           ISOdate(b[1],1,1)+b[2]*86400,
##'           length.out=length(x))         # something is not quite right here but who cares
##' s <- spec.ls(as.numeric(ti)/86400, x, plot=FALSE)
##' bins <- cut(s)
##' conf.lim <- ci.spec(s)
##' conf.y <- max(s$spec)/conf.lim[2]
##' lm1 <- lm(10*log10(psd)~log10(period), bins)
##' psd.fig <- plot(s) +
##'     layer({
##'         panel.lines(rep(max(s$freq), 2), 10*log10(conf.y*conf.lim), col="blue")
##'         panel.abline(lm1)
##'         panel.xyplot(log10(out$period), 10*log10(out$psd), col="red")
##'         at <- log10(out[2,])
##'         panel.text(at$period, 10*at$psd,
##'                    substitute(Delta == d, list(d=coef(lm1)[2]/10)), adj=c(0.5,-8))
##'     })
##' plot(psd.fig)
##' @export
plot.spec <- function(s) {
    dat <- data.frame(period=1/s$freq, dB=log10(s$spec)*10)
    xyplot(dB~period, dat, type='l',
           scales=list(x=list(log=10)),
           xscale.components=xscale.components.log10ticks,
           panel=function(x,y,...) {
               panel.grid.log(y=FALSE)
               panel.xyplot(x,y,...)
           }
           , ylab="Spectrum, dB", xlab="Period, days"
           )
}
