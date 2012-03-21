##' Heavily modified version from cts package
##'
##' Similar to FFT but allows gaps in data and slow.
##' \url{https://stat.ethz.ch/pipermail/r-help/2009-June/201949.html}
##' @title Lomb-Scargle periodogram
##' @param ti time variable. POSIXct should be converted let's say to days like as.numeric(ti)/86400
##' @param x signal
##' @param spans
##' @param freq
##' @param kernel
##' @param taper
##' @param type
##' @param demean
##' @param detrend
##' @param plot
##' @param na.action
##' @param ... extra parameters for plot.spec
##' @return Object of class spec
##' @author Zhu Wang, Mikhail Titov
##' @references
##' [1] N. R. Lomb, "Least-squares frequency analysis of unequally spaced data," Astrophysics and space science, vol. 39, no. 2, pp. 447-462, 1976.
##' [2] J. D. Scargle, "Studies in astronomical time series analysis. II-Statistical aspects of spectral analysis of unevenly spaced data," The Astrophysical Journal, vol. 263, pp. 835-853, 1982.
##' @export
spec.ls <- function (ti, x, spans = NULL, freq = NULL, kernel = NULL, taper = 0.1,
                     type = "lomb", demean = FALSE, detrend = TRUE,
                     plot = TRUE, na.action = na.fail, ...) {
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    N <- length(x)
    if (!is.null(spans))
        if (is.tskernel(spans))
            kernel <- spans
        else kernel <- kernel("modified.daniell", spans%/%2)
    if (!is.null(kernel) && !is.tskernel(kernel))
        stop("must specify spans or a valid kernel")
    if (detrend) {
        t_hat <- ti - mean(ti)
        x_hat = x-mean(x)
        x <- x_hat - sum(x_hat * t_hat) * t_hat / sum(t_hat^2)
        ## x=resid(lm(x ~ t))
    }
    else if (demean) {
        x <- x-mean(x)                  # sweep(x, 2, colMeans(x))
    }

    ## apply taper:
    x <- spec.taper(x, taper)
    ## to correct for tapering: Bloomfield (1976, p. 194)
    ## Total taper is taper*2
    u2 <- (1 - (5/8)*taper*2)
    u4 <- (1 - (93/128)*taper*2)

    T=max(ti)-min(ti)
    ## from fundamental to Nyquist frequency
    if (is.null(freq)) {
        Nspec <- floor(N/2)
        freq <- seq(from = 1/T, by = 1/T, length = Nspec)
    } else {
        Nspec=length(freq)
    }
    pgram <- vector(length = Nspec)
    if (type == "lomb") {
        f4pi=4 * pi * freq
        for (k in 1:Nspec) {
            f4pitau <- atan( sum(sin(f4pi[k] * ti)) / sum(cos(f4pi[k] * ti)) )
            argu=(f4pi[k] * ti - f4pitau)/2
            cosargu=cos(argu)
            sinargu=sin(argu)
            pgram[k] <- 0.5 * ( sum(x * cosargu)^2 / sum(cosargu^2)
                               + sum(x * sinargu)^2 / sum(sinargu^2) )
        }
    }
    if (type == "ft") {
        f2pi = 2*pi*freq
        for (k in 1:length(freq)) {
            argu=f2pi[k] * ti
            cosargu=cos(argu)
            sinargu=sin(argu)
            pgram[k] <- ( sum(x * cosargu)^2 + sum(x * sinargu)^2 ) / N
        }
    }
    if (!is.null(kernel)) {
        spec <- kernapply(c(pgram,rev(pgram)), kernel, circular = TRUE)[1:Nspec]
        df <- df.kernel(kernel)
        bandwidth <- bandwidth.kernel(kernel)
    }
    else {
        spec=pgram
        df <- 2
        bandwidth <- sqrt(1/12)
    }
    bandwidth <- bandwidth / T
    ## correct for tapering
    df <- df/(u4/u2^2)
    ## useless due to 'normalization'
    ## spec<- spec/u2

    ## var(x) = 2* \int_0^{fmax} spec since spectrum is symmetrical for real x
    ## see also http://en.wikipedia.org/wiki/Spectral_density#Properties_of_the_power_spectral_density
    spec=T*spec/sum(spec)/2  # in units of var(x) # Parseval's theorem
    spg.out <- list(freq = freq, spec = spec,
                    kernel = kernel, df = df, bandwidth = bandwidth, n.used = nrow(x),
                    series = series, snames = colnames(x),
                    method = ifelse(!is.null(kernel),
                    "Smoothed Lomb-Scargle Periodogram", "Raw Lomb-Scargle Periodogram"),
                    detrend = detrend, demean = demean)
    class(spg.out) <- "spec"
    if (plot) {
        plot(spg.out, ...)
        return(invisible(spg.out))
    }
    else return(spg.out)
}
