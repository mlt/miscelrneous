##' Confidence interval for periodogram
##'
##' Taken from stats::plot.spec
##' @title Confidence interval for periodogram
##' @param spec.obj
##' @param coverage
##' @return array of length 2
##' @export
ci.spec <- function(spec.obj, coverage = 0.95) {
    if (coverage < 0 || coverage >= 1)
        stop("coverage probability out of range [0,1)")
    tail <- (1 - coverage)
    df <- spec.obj$df
    upper.quantile <- 1 - tail * pchisq(df, df, lower.tail = FALSE)
    lower.quantile <- tail * pchisq(df, df)
    1/(qchisq(c(upper.quantile, lower.quantile), df)/df)
}
