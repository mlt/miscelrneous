##' Octave binning for (raw) periodogram
##'
##' <details>
##' @title Octave binning
##' @param s spec object as returned by spec.ls or alike
##' @return data.frame with period, freq, psd, and sd
##' @author Mikhail Titov
##' @export
cut.spec <- function(s) {
    period.rng <- log2(range(1/s$freq))
    n <- ceiling(diff(period.rng))           # number of octaves, i.e. freq
    breaks <- seq(from=period.rng[1], by=1, length.out=n+1)
    bins <- cut(1/s$freq, 2^breaks, include.lowest=TRUE)#, labels=1:(n-1))
    period <- 2^(breaks[1:n] + 0.5)                 # middle of octave
    out <- data.frame(period = period,
                      freq = 1/period,
                      psd    = tapply(s$spec, bins, "mean"),
                      sd     = tapply(s$spec, bins, "sd")
                      )
    out
}
