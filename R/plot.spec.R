plot.spec <- function(s) {
    dat <- data.frame(period=1/s$freq, dB=log10(s$spec)*10)
    xyplot(dB~period, dat, type='l',
           scales=list(x=list(log=10)),
           xscale.components=xscale.components.log10ticks,
           panel=function(x,y,...) {
               lim <- current.panel.limits()
               v <- latticeExtra:::logTicks(10^lim$xlim, loc=1)
               h <- pretty(lim$ylim)
               panel.abline(h=h, v=log10(v), col="LightGray")
               panel.xyplot(x,y,...)
               out <- ob(1/s$freq, s$spec)
               lm1 <- lm(log10(psd)*10~log10(period), head(out,4))
               panel.abline(lm1)
               lm2 <- lm(log10(psd)*10~log10(period), out[5:7,])
                                        #           print(out[c(5,7),"period"]/365)
               panel.abline(lm2)
               panel.xyplot(log10(out$period), log10(out$psd)*10, col="red")
               at <- log10(out[2,])
               d <- format(coef(lm1)[2]/10,digits=3)
               panel.text(at$period,at$psd,substitute(Delta == d,list(d=d)),adj=c(0.5,3))
               at <- log10(out[6,])
               d <- format(coef(lm2)[2]/10,digits=3)
               panel.text(at$period,at$psd,substitute(Delta == d,list(d=d)),adj=c(0.5,3))
           }
           , ylab="Intensity, dB", xlab="Period, days"
           )
}
