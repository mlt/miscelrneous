# data from USGS gage
#require(tmdl)
library(latticeExtra)
source('../R/tmdl.R')
pol=read.csv('c:/workspace/tmdl/pkg/data/water_chemistry_data_02026001_1974-07-01_2010-10-07.csv')
pol$date = as.POSIXct(pol$Timestamp)
pol$Timestamp = NULL
#file <- file.path(.path.package('tmdl'), 'data', 'USGS_04015330_Knife_River.txt')
x <- read.table('c:/workspace/tmdl/pkg/data/USGS_04015330_Knife_River.txt', skip=29, as.is=TRUE)
names(x)  <-  c("agency", "site", "date", "cfs", "status")
x$agency <- NULL
x$site <- NULL
x$status <- NULL
x$cfs[x$cfs=='Ice'] <- NA
x$cfs <- as.numeric(x$cfs)
x$date <- as.POSIXct(x$date, format="%Y-%m-%d")
z <- rankflow(x,pol)
turb2tss <- function(x) 10^(0.920*log10(x)-0.230)
tmdl1 <- tmdl(z, names=list(pol="TSS..mg.L."), WQS = turb2tss(10))
#fig <- ldplot(tmdl1)
print(summary(tmdl1))
#fig <- ldplot(z, names=list(pol="TSS..mg.L."), target = turb2tss(10))
fig <- ldplot(tmdl1)
plot(fig)
