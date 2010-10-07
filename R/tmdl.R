fdplot <-
function (data, names = list(), type, scales = list(), xlab = "Flow Duration Interval (%)",
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
    xyplot(f, data, type = type, ..., scales = scales, xscale.components = xscale.components.fi,
        yscale.components = yscale.components.log10ticks, xlab = xlab,
        ylab = ylab, panel = function(...) {
            panel.loggrid()
            panel.xyplot(...)
        })
}

ldplot <-
function (data, names = list(), mult = 2.446576, target = 0.1,
    xlab = "Flow Duration Interval (%)", ylab = "Load", key = list(), scales = list(), ...)
{
    names.default <- list(cfs = "cfs", exc = "exc", pol = "pol")
    names <- modifyList(names.default, names)
    # for box & whiskers plot
    sub <- subset(data, !is.na(names$pol), c(names$cfs, names$exc,
        names$pol))
    sub$load <- sub[, names$cfs] * sub[, names$pol] * mult
    sub$grp <- factor(5, c(5, 25, 50, 75, 95))
    sub[10 <= sub[, names$exc] & sub[, names$exc] < 40, "grp"] = 25
    sub[40 <= sub[, names$exc] & sub[, names$exc] < 60, "grp"] = 50
    sub[60 <= sub[, names$exc] & sub[, names$exc] < 90, "grp"] = 75
    sub[90 <= sub[,names$exc], "grp"] = 95
    target.load <- data[, names$cfs] * target * mult
    panel.ld <- function(x, ...) {
        panel.loggrid()
        panel.lines(data[, names$exc], log10(target.load), col = key$lines$col[1])
        panel.points(sub[, names$exc], log10(sub$load), col = key$points$col[1])
        panel.bwplot(as.numeric(as.character(sub$grp)), box.width = 4,
            pch = "|", ...)
    }
    ylim <- range(target.load[target.load>0], sub$load[sub$load>0], na.rm = TRUE) * c(0.8, 1.2)
    key.default <- list(lines = list(col = "darkgreen", lty = c(1, 0)),
                        points = list(col = "#ff00ff", pch = c(NA, 1)),
                        text = list(lab = c("Target", "Measured")),
                        columns = 2, rep = TRUE, space = "bottom")
    key <- modifyList(key.default, key)
    scales.default <- list(y = list(log = 10), x = list(alternating = 3, cex = c(1, 0.7)))
    scales <- modifyList(scales.default, scales)
    bwplot(load ~ grp, sub, scales = scales, xscale.components = xscale.components.fi,
        yscale.components = yscale.components.log10ticks, ylim = ylim,
        key = key, xlim = c(-5, 105), xlab = xlab, ylab = ylab,
        panel = panel.ld)
}

panel.loggrid <-
function ()
{
    ylim <- 10^current.panel.limits()$ylim
    panel.abline(v = c(20, 30, 50, 70, 80), lty = 3, col = "gray90")
    panel.abline(h = log10(latticeExtra:::logTicks(ylim, 2:9)), lty = 3, col = "gray90")
    panel.abline(v = c(0, 10, 40, 60, 90, 100), lty = 3, col = "gray60")
    panel.abline(h = log10(latticeExtra:::logTicks(ylim, 1)), lty = 3, col = "gray60")
}

rankflow <-
function (data, pol, names = list(), sort = TRUE)
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
# method 1
#    cfs=data.daily[, names$cfs]
#    data.daily$exc=0
#    for(i in 1:length(cfs)) {
#	   	  data.daily[i,"exc"] <- 100*(1-(sum(cfs<cfs[i])+0.5*sum(cfs==cfs[i]))/length(cfs))
#	  }
# method 2
#    ecdf.x <- ecdf(data.daily[, names$cfs])
#	   data.daily$exc <- 100 * (1 - ecdf.x(data.daily[, names$cfs]))
# method 3
    data.daily$exc <- 100 * (1 - (rank(data.daily[, names$cfs],
        ties.method = "first")-1)/(length(data.daily[, names$cfs])-1))
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
