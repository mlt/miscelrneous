import(lattice)

# the following two functions are taken from latticeExtra

# this function is not directly exported by latticeExtra
logTicks <- function (lim, loc = c(1, 5)) {
    ii <- floor(log10(range(lim))) + c(-1, 2)
    main <- 10^(ii[1]:ii[2])
    r <- as.numeric(outer(loc, main, "*"))
    r[lim[1] <= r & r <= lim[2]]
}
yscale.components.log10ticks <- function(lim, logsc = FALSE, at = NULL, ...) {
    ans <- yscale.components.default(lim = lim, logsc = logsc, at = at, ...)
    if (is.null(at)) return(ans)
    if (identical(logsc, FALSE)) return(ans)
    logbase <- logsc
    if (identical(logbase, TRUE)) logbase <- 10
    if (identical(logbase, "e")) logbase <- exp(1)
    tick.at <- logTicks(logbase^lim, loc = 1:9)
    tick.at.major <- logTicks(logbase^lim, loc = 1)
    major <- tick.at %in% tick.at.major
    ans$left$ticks$at <- log(tick.at, logbase)
    ans$left$ticks$tck <- ifelse(major, 1, 0.5)
    ans$left$labels$at <- log(tick.at, logbase)
    ans$left$labels$labels <- as.character(tick.at)
    ans$left$labels$labels[!major] <- ""
    ans$left$labels$check.overlap <- FALSE
    ans
}
# end of 'borrowing'

xscale.components.fi = function(...) {
    ans <- xscale.components.default(...)
    ans$bottom$ticks$at=c(0,10,40,60,90,100)
    ans$bottom$labels$labels=ans$bottom$ticks$at
    ans$top <- ans$bottom
    ans$top$labels$labels=c("High flows","Moist conditions","Mid-range flows","Dry conditions","Low flows")
    ans$top$ticks$at=c(5,25,50,75,95)
    ans$top$ticks$tck=0
    ans
}
panel.loggrid = function() {
    ylim=10^current.panel.limits()$ylim
    panel.abline(v=c(20,30,50,70,80),lty=3,col='gray90')
    panel.abline(h=log10(logTicks(ylim,2:9)),lty=3,col='gray90')
    panel.abline(v=c(0,10,40,60,90,100),lty=3,col='gray60')
    panel.abline(h=log10(logTicks(ylim,1)),lty=3,col='gray60')
}
