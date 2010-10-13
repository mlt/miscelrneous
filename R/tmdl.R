fi <- data.frame(
                 breaks = c(10,40,60,90,100),
                 labels = c("High flows", "Moist conditions", "Mid-range flows", "Dry conditions", "Low flows")
                 )
fi$mids = with(fi, breaks - diff(c(0,breaks)) / 2)
grpfun = with(fi,stepfun(breaks, c(mids, mids[length(mids)])))

fdplot <-
function (data, names = list(), type, scales = list(), xlab = "Flow Duration Interval (%)", values = TRUE,
    ylab, intervals = fi, ...)
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
    xyplot(f, data, type = type, ..., scales = scales,
        xscale.components = xscale.components.fi, intervals = intervals,
        yscale.components = yscale.components.log10ticks, xlab = xlab,
        ylab = ylab, panel = function(x, y, ..., intervals) {
            panel.tmdlgrid(intervals = intervals)
            panel.xyplot(x, y, ...)
            if (values) {
                yy = approx(x, 10^y, c(0, intervals$breaks))
                panel.text(yy$x, log10(yy$y * 1.3), yy$y, pos = 4)
            }
        })
}

ldplot <- function(...)
    UseMethod("ldplot")

# TODO: check if default 1.5 IQR for whiskers is close to 10th and 90th percentiles used in TMDL
# may need to provide custom stats instead of boxplot.stats for that
ldplot.default <-
function (data, names = list(), mult = 2.446576, WQS = 0.1, target = WQS, WLA, intervals = fi,
    xlab = "Flow Duration Interval (%)", ylab = "Load", key = list(), scales = list(), ...)
{
    names.default <- list(cfs = "cfs", exc = "exc", pol = "pol")
    names <- modifyList(names.default, names)
    mydata <- subset(data, select = c(names$exc, names$cfs))
    mydata$target <- mydata[, names$cfs] * target * mult
    mydata$load <- mydata[, names$cfs] * data[, names$pol] * mult
    # for box & whiskers plot
    sub <- subset(mydata, !is.na(load),
                  c(names$cfs, names$exc, 'load'))
    sub$grp <- factor(grpfun(sub[, names$exc]), intervals$mids)
    ylim <- range(mydata$target[mydata$target>0], sub$load[sub$load>0], na.rm = TRUE) * c(0.8, 1.2)
    key.default <- list(lines = list(col = c("darkgreen", NA, "red"), lty = c(1, 0, 1)),
                        points = list(col = "#ff00ff", pch = c(NA, 1, NA)),
                        text = list(lab = c("TMDL", "Measured", expression(sum(WLA)))),
                        columns = 3, rep = TRUE, space = "bottom")
    key <- modifyList(key.default, key)
    scales.default <- list(y = list(log = 10), x = list(alternating = 3, cex = c(1, 0.7)))
    scales <- modifyList(scales.default, scales)
    bwplot(load ~ grp, sub, scales = scales, mydata = mydata, mysub = sub, names = names, WLA = WLA,
        xscale.components = xscale.components.fi, intervals = intervals,
        yscale.components = yscale.components.log10ticks, ylim = ylim,
        key = key, xlim = c(-5, 105), xlab = xlab, ylab = ylab,
        panel = panel.ld)
}

ldplot.tmdl <-
function (tmdl, ...)
{
#    warning("Unimplemented")
    ldplot.default(tmdl$data, tmdl$names, tmdl$mult, WQS = tmdl$WQS, WLA = tmdl$WLA, ...)
}

# monthly variation plot
mvplot <-
function (data, intervals = fi)
{
    scales <- list(x = list(alternating = 3, cex = c(1, 0.7)))
    tmp <- data.frame(month=factor(months(as.POSIXct(data$date),TRUE), rev(month.abb)),
                      exc = data$exc)
    bwplot(month ~ exc, tmp, intervals = intervals,
    horizontal=TRUE, xscale.components = xscale.components.fi, scales=scales,
    panel=function(...) {
        panel.tmdlgrid(FALSE, intervals = intervals)
        panel.bwplot.tmdl(..., box.width = .5)
    })
}

panel.bwplot.tmdl <-
function (x, y, horizontal, ...)
{
    ps.orig <- ps <- trellis.par.get("plot.symbol")
    ps$pch <- 8
    ps$cex <- .5
    trellis.par.set("plot.symbol", ps)
    panel.bwplot(x, y, horizontal, pch="|", cex=0.5, ..., stats = tmdl.stats)
    trellis.par.set("plot.symbol", ps.orig)
    if (horizontal) {
        xx <- tapply(x, y, function(x) mean(x, na.rm = TRUE))
        panel.points(xx, seq(1,along=xx), pch = 18, cex = 1)
    }
    else {
        yy <- tapply(y, x, function(x) mean(x, na.rm = TRUE))
        panel.points(names(yy), yy, pch = 18, cex = 1)
    }
}

panel.ld <-
function(x, ..., mydata, names, WLA, intervals)
{
    panel.tmdlgrid(TRUE, intervals = intervals)
    panel.abline(h = log10(WLA), col = "red", lwd = 1.5)
    panel.lines(mydata[, names$exc], log10(mydata$target), col = "darkgreen")# , col = key$lines$col[1])
    panel.points(mydata[, names$exc], log10(mydata$load), col = "#ff00ff")#, col = key$points$col[1])
    panel.bwplot.tmdl(as.numeric(as.character(x)), box.width = 4, ...)
}

panel.tmdlgrid <-
function (y = TRUE, intervals)
{
    panel.abline(v = c(0, intervals$breaks), lty = 3, col = "gray60")
    panel.abline(v = c(20, 30, 50, 70, 80), lty = 3, col = "gray90")
    if (y) {
        ylim <- 10^current.panel.limits()$ylim
        panel.abline(h = log10(latticeExtra:::logTicks(ylim, 2:9)), lty = 3, col = "gray90")
        panel.abline(h = log10(latticeExtra:::logTicks(ylim, 1)), lty = 3, col = "gray60")
    }
}

# shall geometric mean be calculated for pollutants?
# exp(mean(log(x)))
rankflow <-
function (data, pol, names = list(), sort = TRUE, method = "linear")
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
    if (!missing(pol)) {
        day <- as.character(trunc(pol[, names$date], units = "days"))
        columns <- which(sapply(pol, is.numeric))
        pol.daily <- aggregate(subset(pol, select = columns),
            by = list(date = day), FUN = "mean")
        data.daily = merge(data.daily, pol.daily, by = names$date,
            all.x = TRUE, sort = FALSE)
    }

    # flow ranking
    switch(tolower(method), pr = { # method 1 http://en.wikipedia.org/wiki/Percentile_rank
        cfs=data.daily[, names$cfs]
        data.daily$exc=0
        for(i in 1:length(cfs)) {
            data.daily[i,"exc"] <- 100*(1-(sum(cfs<cfs[i])+0.5*sum(cfs==cfs[i]))/length(cfs))
        }
    },
           ecdf = {  # method 2 fraction of observations less or equal
               ecdf.x <- ecdf(data.daily[, names$cfs])
               data.daily$exc <- 100 * (1 - ecdf.x(data.daily[, names$cfs]))
           },
           linear = {                   # method 3
               data.daily$exc <- 100 * (1 - (rank(data.daily[, names$cfs],
                                                  ties.method = "first")-1)/(length(data.daily[, names$cfs])-1))
           })
    if (sort) {                         # sort to plot by exceedance
        o = order(data.daily$exc)
        data.daily = data.daily[o, ]
    }
    data.daily
}

summary.tmdl <-
function (object)
{
#    print("world")
#    NextMethod("summary")
    out <- as.data.frame(t(sapply(object$stats, function(x) x$stats[5])))
    out <- rbind(out, object$MOS, object$TMDL, object$WLA, object$LA)
#    out <- data.frame('q90' = sapply(object$stats, function(x) x$stats[5]))
    rownames(out) <- c('90th','MOS','TMDL','WLA','LA')
    red <- 1 - (object$TMDL - object$MOS)/out['90th',]
    red[red<0] <- NA
    out <- rbind(out, Reduction = red*100)
    out <- out[c(3:1,4,5,6),]
    format(out, digits=2, scientific = FALSE)
#    str(object$stats$'5'$stats[5])
}

# MOS can be
# TRUE to calculate as difference between median and low flow in each zone
# percentage as between 0 and 1
# FALSE to skip MOS
# FIXME: MS4 WLA???
tmdl <-
function (data, names = list(), mult = 2.446576, WQS = 0.1, target = WQS, WLA = 0, MOS = TRUE, intervals = fi)
{
    names.default <- list(cfs = "cfs", exc = "exc", pol = "pol")
    names <- modifyList(names.default, names)
    mydata <- subset(data, select = c(names$exc, names$cfs, names$pol)) # FIXME: remove names$pol in the future. Used in current ldplot.tmdl only
    mydata$target <- mydata[, names$cfs] * target * mult
    mydata$load <- mydata[, names$cfs] * data[, names$pol] * mult
    mydata$grp <- factor(grpfun(mydata[, names$exc]), intervals$mids)

    targetfun <- approxfun(mydata$exc, mydata$target)
    TMDL.mids <- targetfun(intervals$mids)

    if (is.logical(MOS) & MOS) {
        TMDL.breaks <- targetfun(intervals$breaks)
        MOS <- TMDL.mids - TMDL.breaks
        MOS.len <- length(MOS)
        if (diff(tail(MOS,2))> 0 )
            MOS[MOS.len] <- MOS[MOS.len - 1]    # skewed for low flows
    } else if (is.numeric(MOS)) {
        MOS <- TMDL.mids * MOS
    }

    LA <- TMDL.mids - MOS - WLA

    # for box & whiskers plot
    sub <- subset(mydata, !is.na(load), c(names$cfs, names$exc, 'load'))
    sub$grp <- factor(grpfun(sub[, names$exc]), intervals$mids)
    blist <- tapply(sub$load, sub$grp, tmdl.stats)
    names(blist) <- fi$labels

    # FIXME: need reduction for LA

    out <- list(data = mydata, stats = blist, names = names,
                WQS = target, mult = mult,
                WLA = WLA, MOS = MOS, TMDL = TMDL.mids, LA = LA)
    class(out) <- "tmdl"
    out
}

# original boxplot.stats https://svn.r-project.org/R/trunk/src/library/grDevices/R/calc.R
tmdl.stats <-
function (x, coef = 1.5, do.conf = TRUE, do.out = TRUE)
{
    nna <- !is.na(x)
    n <- sum(nna)                       # including +/- Inf
    stats <- quantile(x, c(.1,.25,.5,.75,.9), na.rm = TRUE, names = FALSE)
    out <- x < stats[1] | x > stats[5]
    iqr <- diff(stats[c(2, 4)])
    conf <- if(do.conf) stats[3L] + c(-1.58, 1.58) * iqr / sqrt(n)
    list(stats = stats, n = n, conf = conf,
	 out = if(do.out) x[out & nna] else numeric(0L))
}

xscale.components.fi <-
function (..., intervals)
{
#        print(intervals)
    ans <- xscale.components.default(...)
    ans$bottom$ticks$at <- seq(0, 100, 10)
    ans$bottom$labels$labels <- ans$bottom$ticks$at
    ans$top <- ans$bottom
    ans$top$labels$labels <- fi$labels #c("High flows", "Moist conditions",
#        "Mid-range flows", "Dry conditions", "Low flows")
    ans$top$ticks$at <- fi$mids #c(5, 25, 50, 75, 95)
    ans$top$ticks$tck <- 0
    ans
}
