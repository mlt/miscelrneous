# data from USGS gage 
#require(tmdl)
library(latticeExtra)
source('../R/tmdl.R')
#file <- file.path(.path.package('tmdl'), 'data', 'USGS_04015330_Knife_River.txt')
x <- read.table('c:/workspace/tmdl/pkg/data/USGS_04015330_Knife_River.txt', skip=29, as.is=TRUE)
names(x)  <-  c("agency", "site", "date", "cfs", "status")
x$agency <- NULL
x$site <- NULL
x$status <- NULL
x$cfs[x$cfs=='Ice'] <- NA
x$cfs <- as.numeric(x$cfs)
x$date <- as.POSIXct(x$date, format="%Y-%m-%d")
z <- rankflow(x)
fig <- fdplot(z, ylab=list("Flow (cfs)", cex=1.5, col='red'), main="USGS gage 04015330")
plot(fig)
