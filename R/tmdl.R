fdplot <-
function (flow, names, type, scales = list(), xlab = "Flow Duration Interval (%)",
    ylab, ...)
{
    names.default <- list(cfs = "cfs", exc = "exc")
    names <- modifyList(names.default, names)
    if (missing(ylab))
        ylab = names$cfs
    if (missing(type))
        type = "l"
    scales.default <- list(y = list(log = TRUE), x = list(alternating = 3,
        cex = c(1, 0.7)))
    scales <- modifyList(scales.default, scales)
    f <- formula(sprintf("%s~%s", names$cfs, names$exc))
    xyplot(f, flow, type = type, ..., scales = scales, xscale.components = xscale.components.fi,
        yscale.components = yscale.components.log10ticks, xlab = xlab,
        ylab = ylab, panel = function(...) {
            panel.loggrid()
            panel.xyplot(...)
        })
}
ldplot <-
function (data, names = list(), mult = 2.446576, target = 0.1,
    xlab = "Flow Duration Interval (%)", ylab = "Load", ...)
{
    names.default <- list(cfs = "cfs", exc = "exc", pol = "pol")
    names <- modifyList(names.default, names)
    # for box & whiskers plot
    sub = subset(data, !is.na(names$pol), c(names$cfs, names$exc,
        names$pol))
    sub$load = sub[, names$cfs] * sub[, names$pol] * mult
    sub$grp = factor(5, c(5, 25, 50, 75, 95))
    sub[with(sub, 10 <= exc & exc < 40), "grp"] = 25
    sub[with(sub, 40 <= exc & exc < 60), "grp"] = 50
    sub[with(sub, 60 <= exc & exc < 90), "grp"] = 75
    sub[90 <= sub$exc, "grp"] = 95
    target.load <- data[, names$cfs] * target * mult
    panel.ld = function(x, ...) {
        panel.loggrid()
        panel.lines(data[, names$exc], log10(target.load), col = key$lines$col[1])
        panel.points(sub[, names$exc], log10(sub$load), col = key$points$col[2])
        panel.bwplot(as.numeric(as.character(sub$grp)), box.width = 4,
            pch = "|", ...)
    }
    ylim = range(target.load, sub$load, na.rm = TRUE) * c(0.8,
        1.2)
    key = list(lines = Rows(trellis.par.get("superpose.line"),
        c(3, 3)), points = Rows(trellis.par.get("superpose.symbol"),
        c(2, 2)), text = list(lab = c("Target", "Measured")),
        columns = 2, rep = TRUE, space = "bottom")
    key$points$pch = c(NA, 1)
    key$lines$lty = c(1, 0)
    bwplot(load ~ grp, sub, scales = list(y = list(log = 10),
        x = list(alternating = 3, cex = c(1, 0.7))), xscale.components = xscale.components.fi,
        yscale.components = yscale.components.log10ticks, ylim = ylim,
        key = key, xlim = c(-5, 105), xlab = xlab, ylab = ylab,
        panel = panel.ld)
}

# the following function is taken from latticeExtra
# this function is not directly exported by latticeExtra
logTicks <-
function (lim, loc = c(1, 5))
{
    ii <- floor(log10(range(lim))) + c(-1, 2)
    main <- 10^(ii[1]:ii[2])
    r <- as.numeric(outer(loc, main, "*"))
    r[lim[1] <= r & r <= lim[2]]
}
panel.loggrid <-
function ()
{
    ylim <- 10^current.panel.limits()$ylim
    panel.abline(v = c(20, 30, 50, 70, 80), lty = 3, col = "gray90")
    panel.abline(h = log10(logTicks(ylim, 2:9)), lty = 3, col = "gray90")
    panel.abline(v = c(0, 10, 40, 60, 90, 100), lty = 3, col = "gray60")
    panel.abline(h = log10(logTicks(ylim, 1)), lty = 3, col = "gray60")
}
prepareflow <-
function (data, pol = NULL, names = NULL, sort = TRUE)
{
    names.default <- list(cfs = "cfs", date = "date")
    names <- modifyList(names.default, names)
    columns <- c(which(colnames(data) == names$date), which(sapply(data,
        is.numeric)))
    flow = data[!is.na(data[, names$cfs]), columns]
    # daily flow averaged as well as daily averages of everything else that might be in data.frame
    day <- as.character(trunc(flow[, names$date], units = "days"))
    data.daily <- aggregate(subset(flow, select = -1), by = list(date = day),
        FUN = "mean")
    # same thing with pollutant data if supplied separately
    if (!is.null(pol)) {
        day <- as.character(trunc(pol[, date], units = "days"))
        columns <- which(sapply(pol, is.numeric))
        pol.daily <- aggregate(subset(pol, select = columns),
            by = list(date = day), FUN = "mean")
        data.daily = merge(data.daily, pol.daily, by = date,
            all.x = TRUE, sort = FALSE)
    }
    # flow ranking
    # can be first instead of random
    data.daily$exc <- 100 * (1 - rank(data.daily[, names$cfs],
        ties.method = "random")/length(data.daily[, names$cfs]))
    if (sort) {                         # sort to plot by exceedance
        o = order(data.daily$exc)
        data.daily = data.daily[o, ]
    }
    data.daily
}

xscale.components.fi <-
function (...)
{
    ans <- xscale.components.default(...)
    ans$bottom$ticks$at <- c(0, 10, 40, 60, 90, 100)
    ans$bottom$labels$labels <- ans$bottom$ticks$at
    ans$top <- ans$bottom
    ans$top$labels$labels <- c("High flows", "Moist conditions",
        "Mid-range flows", "Dry conditions", "Low flows")
    ans$top$ticks$at <- c(5, 25, 50, 75, 95)
    ans$top$ticks$tck <- 0
    ans
}

# the following function is taken from latticeExtra
# this function is not directly exported by latticeExtra
yscale.components.log10ticks <-
function (lim, logsc = FALSE, at = NULL, ...)
{
    ans <- yscale.components.default(lim = lim, logsc = logsc,
        at = at, ...)
    if (is.null(at))
        return(ans)
    if (identical(logsc, FALSE))
        return(ans)
    logbase <- logsc
    if (identical(logbase, TRUE))
        logbase <- 10
    if (identical(logbase, "e"))
        logbase <- exp(1)
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
