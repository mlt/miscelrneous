fi <- data.frame(
                 breaks = c(10,40,60,90,100),
                 labels = c("High flows", "Moist conditions", "Mid-range flows", "Dry conditions", "Low flows")
                 )
fi$mids = with(fi, breaks - diff(c(0,breaks)) / 2)
grpfun = with(fi,stepfun(breaks, c(mids, mids[length(mids)])))

fdplot <-
function (data, names = list(), values = FALSE, type = "l", scales = list(), xlab = "Flow Duration Interval (%)", ylab, intervals = fi, ...)
{
    names.default <- list(Q = "Q", exc = "exc")
    names <- modifyList(names.default, names)
    if (missing(ylab))
        ylab = names$Q
    scales.default <- list(y = list(log = TRUE), x = list(alternating = 3,
        cex = c(1, 0.7)))
    scales <- modifyList(scales.default, scales)
    f <- formula(sprintf("%s~%s", names$Q, names$exc))
    xyplot(f, data, type = type, ..., scales = scales,
        xscale.components = xscale.components.fi, intervals = intervals,
        yscale.components = yscale.components.log10ticks, xlab = xlab,
        ylab = ylab, panel = function(x, y, ..., intervals) {
            panel.tmdlgrid(intervals = intervals)
            panel.xyplot(x, y, ...)
            if (values) {
                yy = approx(x, 10^y, c(0, intervals$breaks))
                panel.text(yy$x, log10(yy$y), format(yy$y), adj=c(0.3,-1),cex=.8)
            }
        })
}

#plot <- function(...)
#    UseMethod("plot")

ldplot <-
function (data, names = list(), mult = 2.446576, WQS = 0.1, target = WQS, WLA = NA, intervals = fi, coef = .1, do.out = FALSE,
    xlab = "Flow Duration Interval (%)", ylab = "Load", key = NULL, legend = NULL, scales = list(), plot = list(), ...)
{
    plot.default <- list(MOS = FALSE, WLA = FALSE)
    plot <- modifyList(plot.default, plot)
    names.default <- list(Q = "Q", exc = "exc", pol = "pol")
    names <- modifyList(names.default, names)
    mydata <- subset(data, select = c(names$exc, names$Q))
    mydata$target <- mydata[, names$Q] * target * mult
    mydata$load <- mydata[, names$Q] * data[, names$pol] * mult
    # for box & whiskers plot
    sub <- subset(mydata, !is.na(load), c(names$Q, names$exc, 'load'))
    sub$grp <- factor(grpfun(sub[, names$exc]), intervals$mids)
    ylim <- range(mydata$target[mydata$target>0], sub$load[sub$load>0], na.rm = TRUE) * c(0.8, 1.2)
    if (missing(legend) & missing(key))
        legend <- list(inside = list(args = list(target = target, WLA = WLA, plot = plot), fun = ldplot.legend, corner = c(1,1)))
    scales.default <- list(y = list(log = 10), x = list(alternating = 3, cex = c(1, 0.7)))
    scales <- modifyList(scales.default, scales)
#    print(do.call(legend$inside$fun,args=legend$inside$args))
    bwplot(load ~ grp, sub, scales = scales, mydata = mydata, mysub = sub, names = names, WLA = WLA, coef = coef, do.out = do.out,
        xscale.components = xscale.components.fi, intervals = intervals, plot = plot, ...,
        yscale.components = yscale.components.log10ticks, ylim = ylim,
        key = key, legend = legend, xlim = c(-5, 105), xlab = xlab, ylab = ylab,
        panel = panel.ld)
}

ldplot.key <-
function(target, WLA, plot)
{
    len <- length(rownames(WLA))
    WLA.names <- rownames(WLA)
    if (len == 0) {
        len <- 1
        WLA.names = "WLA"
    } else if (len>1)
        WLA.names[-1] <- paste('+', WLA.names[-1])
    data <- data.frame(Rows(trellis.par.get("superpose.symbol"), 2), stringsAsFactors = FALSE)
    data[c("lty", "lwd")] <- 0
    data$lab <- "Measured"
    names <- c()
    if (!is.na(target))
        names <- c(names, "LC")
    if (plot$MOS)
        names <- c(names, "MOS")
    if (plot$WLA)
        names <- c(names, WLA.names)
    lines <- as.data.frame(Rows(trellis.par.get("superpose.line"), 2+1:length(names)))
    lines[c("cex", "fill", "font", "pch")] <- NA
    lines$lab <- names
    data <- rbind(data, lines)
    list(lines = list(alpha = data$alpha, col = data$col, lty = data$lty, lwd = data$lwd),
                points = list(alpha = data$alpha, col = data$col, fill = data$fill, font = data$font, pch = data$pch),
                text = list(lab = data$lab),
                corner=c(1,1), background="transparent", padding.text = 3)
}

ldplot.legend <-
function(...)
    draw.key(ldplot.key(...))

plot.tmdl <-
function (x, ...)
{
#    warning("Unimplemented")
    ldplot(x$data, x$names, x$mult, WQS = x$WQS, WLA = x$WLA, MOS.fun = x$MOS.fun, WLA.fun = x$WLA.fun, ...)
}

# monthly variation plot
mvplot <-
function (data, names = list(), intervals = fi, exc = TRUE, scales = list(), xlab, ...)
{
    names.default <- list(date = "date", exc = "exc", Q = "Q")
    names <- modifyList(names.default, names)
    xscale.components = xscale.components.default
    if (exc) {
        scales.default <- list(x = list(alternating = 3, cex = c(1, 0.7)))
        scales <- modifyList(scales.default, scales)
        xscale.components = xscale.components.fi
    }
    if (missing(xlab))
        xlab = ifelse(exc, "Flow Duration Interval (%)", "Flow rate")
    tmp <- data.frame(
                      month=factor(months(as.POSIXct(data[,names$date]),TRUE), rev(month.abb)),
                      value = data[,ifelse(exc,names$exc,names$Q)])
    bwplot(month ~ value, tmp, intervals = intervals, ...,
           horizontal=TRUE, scales=scales, xlab = xlab,
           xscale.components = xscale.components,
           panel=function(...) {
               if (exc)
                   panel.tmdlgrid(FALSE, intervals = intervals)
               panel.bwplot.tmdl(..., box.width = .5)
           })
}

mvstats <-
function (data, names = list(), exc = TRUE, ...)
{
    names.default <- list(date = "date", exc = "exc", Q = "Q")
    names <- modifyList(names.default, names)
    tmp <- data.frame(
                      month=factor(months(as.POSIXct(data[,names$date]),TRUE), rev(month.abb)),
                      value = data[,ifelse(exc, names$exc, names$Q)])
    tapply(tmp$value, tmp$month, tmdl.stats, ...)
}

panel.bwplot.tmdl <-
function (x, y, horizontal, ..., stats = tmdl.stats, coef = .1, do.out = FALSE)
{
    ps.orig <- ps <- trellis.par.get("plot.symbol")
    ps$pch <- 8
    ps$cex <- .5
    trellis.par.set("plot.symbol", ps)
    panel.bwplot(x, y, horizontal, pch="|", cex=0.5, ..., stats = stats, coef = coef, do.out = do.out)
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
function(x, ..., mydata, names, WLA, intervals, plot, MOS.fun, WLA.fun)
{
    panel.tmdlgrid(TRUE, intervals = intervals)
    len <- 4
    if (!missing(WLA.fun))
        len <- len + length(WLA.fun)
    lines <- Rows(trellis.par.get("superpose.line"),1:len) # guaranteed
    line.idx <- 3
    panel.lines(mydata[, names$exc], log10(mydata$target), # bug in lattice alpha having length 1?
                alpha = lines$alpha, col = lines$col[line.idx], lty = lines$lty[line.idx], lwd = lines$lwd[line.idx])
    if (plot$MOS & !missing(MOS.fun)) {
        line.idx <- line.idx + 1
        panel.lines(0:100, log10(MOS.fun(0:100)),
                    alpha = lines$alpha, col = lines$col[line.idx], lty = lines$lty[line.idx], lwd = lines$lwd[line.idx])
    }
    if (plot$WLA) {
        line.idx <- line.idx + 1
        if (is.numeric(WLA) & !is.matrix(WLA))
            panel.abline(h = log10(WLA),
                         alpha = lines$alpha, col = lines$col[line.idx], lty = lines$lty[line.idx], lwd = lines$lwd[line.idx])
        else if (!missing(WLA.fun)) {
            xx <- 0:100
            yy <- sapply(WLA.fun, function(f) f(xx))
            if (length(WLA.fun)>1)
                for (i in 2:length(WLA.fun))    # ugly cumulative sum
                    yy[,i] = yy[,i] + yy[,i-1]
            for (i in 1:length(WLA.fun)) {
                panel.lines(xx, log10(yy[,i]),
                            alpha = lines$alpha, col = lines$col[line.idx], lty = lines$lty[line.idx], lwd = lines$lwd[line.idx])
                line.idx <- line.idx + 1
            }
        }
    }
    symbol <- Rows(trellis.par.get("superpose.symbol"),2)
    panel.points(mydata[, names$exc], log10(mydata$load),
                 alpha = symbol$alpha, cex = symbol$cex, col = symbol$col, fill = symbol$fill, font = symbol$font, pch = symbol$pch)
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
function (data, pol, names = list(date="date"), pol.date, sort = TRUE, method = "linear", mean = base::mean)
{
    names.default <- list(Q = "Q", date = "date")
    names <- modifyList(names.default, names)
    columns <- c(which(colnames(data) == names$date), which(sapply(data, is.numeric)))
    flow = data[!is.na(data[, names$Q]), columns]

    # daily flow averaged as well as daily averages of everything else that might be in data.frame
    day <- as.character(trunc(flow[, names$date], units = "days"))
    data.daily <- aggregate(subset(flow, select = names$Q), by = list(date = day), FUN = base::mean)
    pol.daily <- NULL

    # same thing with pollutant data if supplied separately
    if (!missing(pol)) {
        if (missing(pol.date))
            pol.date <- names$date
        day <- as.character(trunc(pol[, pol.date], units = "days"))
        columns <- which(sapply(pol, is.numeric))
        pol.daily <- aggregate(subset(pol, select = columns),
                               by = list(date = day), FUN = mean)
    } else
        pol.daily <- aggregate(subset(flow, select = -c(1, which(names(flow)==names$Q))), by = list(date = day), FUN = mean)
    data.daily = merge(data.daily, pol.daily, by = "date", all.x = TRUE, sort = FALSE)

    # flow ranking
    switch(tolower(method), pr = { # method 1 http://en.wikipedia.org/wiki/Percentile_rank
        Q=data.daily[, names$Q]
        data.daily$exc=0
        for(i in 1:length(Q)) {
            data.daily[i,"exc"] <- 100*(1-(sum(Q<Q[i])+0.5*sum(Q==Q[i]))/length(Q))
        }
    },
           ecdf = {  # method 2 fraction of observations less or equal
               ecdf.x <- ecdf(data.daily[, names$Q])
               data.daily$exc <- 100 * (1 - ecdf.x(data.daily[, names$Q]))
           },
           linear = {                   # method 3
               data.daily$exc <- 100 * (1 - (rank(data.daily[, names$Q],
                                                  ties.method = "first")-1)/(length(data.daily[, names$Q])-1))
           })
    if (sort) {                         # sort to plot by exceedance
        o = order(data.daily$exc)
        data.daily = data.daily[o, ]
    }
    data.daily
}

# Smooth Catmull-Rom spline passing through control knots
splineCR <-
function(x,y)
{
    xd <- diff(x,lag=2)
    yd <- diff(y,lag=2)
    m <- c(0,yd/xd,0)
    splinefunH(x,y,m)
}

summary.tmdl <-
function (object,...)
{
    out <- as.data.frame(t(sapply(object$stats, function(x) ifelse(is.null(x),NA, x$stats[5]))))
    WLA <- object$WLA
    WLA.names <- "WLA"
    if (is.matrix(WLA) | is.data.frame(WLA)) {
        WLA <- as.data.frame(WLA)
        colnames(WLA) <- colnames(out)
        WLA.names <- rownames(WLA)
    }
    out <- rbind(out, object$MOS, object$LC, WLA, object$LA)
    rownames(out) <- c('90th','MOS','LC',WLA.names,'LA')
    red <- 1 - (object$LC - object$MOS)/out['90th',]
    red[red<0] <- NA
    out <- rbind(out, Reduction = red*100)
    out <- out[c(3:1,4:(length(WLA.names)+5)),]
    out
}

# MOS can be
# TRUE to calculate as difference between median and low flow in each zone
# percentage as between 0 and 1. Can be given for each zone individually.
# FALSE to skip MOS
# FIXME: MS4 WLA???
tmdl <-
function (data, names = list(), mult = 2.446576, WQS = 0.1, target = WQS, WLA = NA, MOS = TRUE, intervals = fi, interp = "spline")
{
    names.default <- list(Q = "Q", exc = "exc", pol = "pol")
    names <- modifyList(names.default, names)
    mydata <- subset(data, select = c(names$exc, names$Q, names$pol)) # FIXME: remove names$pol in the future. Used in current ldplot.tmdl only
    mydata$load <- mydata[, names$Q] * data[, names$pol] * mult
    mydata$grp <- factor(grpfun(mydata[, names$exc]), intervals$mids)
    TMDL.mids <- NULL
    LA <- NULL
    MOS.fun <- NULL
    WLA.fun <- NULL
    if (!is.na(target)) {               # without WQS all other calculations are meaningless
        mydata$target <- mydata[, names$Q] * target * mult

        target.fun <- approxfun(mydata$exc, mydata$target)
        TMDL.mids <- target.fun(intervals$mids)

        if (is.logical(MOS) & MOS[1]) {     # we check only first (the only?) element
            TMDL.breaks <- target.fun(intervals$breaks)
            MOS <- TMDL.mids - TMDL.breaks
            MOS.len <- length(MOS)
            MOS[MOS.len] <- TMDL.mids[MOS.len] * MOS[MOS.len - 1] / TMDL.mids[MOS.len - 1]    # skewed for low flows, use same percentage as previous
        } else if (is.numeric(MOS)) {
            MOS <- TMDL.mids * MOS          # should work fine for vector of same length
        }

        MOSpct.fun <- splineCR(intervals$mids, MOS/TMDL.mids)            # Catmull-Rom spline
                                        #    MOSpct.fun <- approxfun(intervals$mids, MOS/TMDL.mids, rule = 2) # linear % of LC, extrapolate as end values
        MOS.fun <- function(x) target.fun(x) * MOSpct.fun(x)

        env <- environment()  # neccesary to adjust available LA for each WLA
        LA <- TMDL.mids - MOS               # LA available for distribution at the moment
                                        # TODO: add case for WLA as a list(?) of user functions
        WLA.fun <- NULL                     # otherwise WLA.fun would be defined within `if` block only
        if (is.matrix(WLA) | is.data.frame(WLA) | is.numeric(WLA)) {
            WLA.mtx <- NULL
            if (is.numeric(WLA))
                WLA.mtx <- matrix(WLA, ncol = 5, dimnames = list("WLA"))
            else
                WLA.mtx <- as.matrix(WLA)#, dimnames=list(WLA=rownames(WLA)))
            if (is.null(rownames(WLA.mtx))) # shouldn't get several unnamed rows
                rownames(WLA.mtx) = "WLA"
            WLA.fun <- sapply(rownames(WLA.mtx),
                              function(name) {
                                  assign("LA", LA - WLA.mtx[name,], env) # this effectively reduces available LA in parent environment
                                  switch(interp,
                                         spline = {
                                        #pct.fun <- approxfun(intervals$mids, WLA.mtx[name,]/TMDL.mids, rule = 2) # % of LC
                                             pct.fun <- splineCR(intervals$mids, WLA.mtx[name,]/TMDL.mids)
                                             function(x) pct.fun(x) * target.fun(x)
                                         },
                                         linear = function(x) 10^approx(intervals$mids, log10(WLA.mtx[name,]), x, rule = 2)$y
                                         )
                              })
        } else if (is.list(WLA)) {      # should be list of functions for now
            WLA.fun <- sapply(WLA, function(f) {
                ff <- function(x) f(exc = x, LC = target.fun(x), MOS = MOS.fun(x), LC.fun = target.fun, MOS.fun = MOS.fun)
                invisible(ff(2))       # without it function is being replaced TODO: ask on the mailing list
                ff
            })
            WLA <- t(sapply(WLA.fun, function(f) f(intervals$mids)))
        }
    }
    # for box & whiskers plot
    sub <- subset(mydata, !is.na(load), c(names$Q, names$exc, 'load'))
    sub$grp <- factor(grpfun(sub[, names$exc]), intervals$mids)
    blist <- tapply(sub$load, sub$grp, tmdl.stats)
    names(blist) <- fi$labels

    obj <- list(data = mydata, stats = blist, names = names,
                WQS = target, mult = mult,
                WLA = WLA, MOS = MOS, LC = TMDL.mids, LA = LA, MOS.fun = MOS.fun, WLA.fun = WLA.fun)
    class(obj) <- c("tmdl", class(obj))
    obj
}

# original boxplot.stats https://svn.r-project.org/R/trunk/src/library/grDevices/R/calc.R
tmdl.stats <-
function (x, coef = .1, do.conf = TRUE, do.out = TRUE)
{
    nna <- !is.na(x)
    n <- sum(nna)                       # including +/- Inf
    stats <- quantile(x, c(coef,.25,.5,.75,1-coef), na.rm = TRUE, names = FALSE)
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
