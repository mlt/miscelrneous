\name{fdplot}
%\alias{load}
\title{Plots Flow Duration Curve}
\description{
This function produces grob with FDC given
prepared flow and pollutant data.
%Reload  the  datasets  written  to  a  file  with  the  function
%\code{save}.
}
\usage{
fdplot (data, names = list(), type, scales = list(), xlab = "Flow Duration Interval (%)",
    ylab, ...)
}
\arguments{
\item{data}{a  data frame typically with columns date, cfs, exc, pol.
 Those can be overriden using \code{names}}
\item{names}{List that can specify custom names for date, cfs, and pol}
\item{...}{Other parameters are passed to \code{lattice::xyplot}}
}
\seealso{
\code{\link{rankflow}}.
}
\examples{
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
}
\keyword{data}
