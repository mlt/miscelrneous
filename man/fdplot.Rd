\name{fdplot}
\alias{fdplot}
\title{Plots Flow Duration Curve}
\description{
This function produces "trellis" object with FDC given
prepared flow and pollutant data.
%Reload  the  datasets  written  to  a  file  with  the  function
%\code{save}.
}
\usage{
fdplot (data, names =list(), values = FALSE, type = "l", scales = list(),
xlab = "Flow Duration Interval (\%)", ylab, intervals = fi, ...)
}
\arguments{
\item{data}{Typically result of \code{\link{rankflow}}, a \code{\link{data.frame}} with columns "Q" & "exc".
 Those can be overriden using \var{names}}
\item{names}{List that can specify custom column names in
  data frame \var{data} for \var{Q} and \var{exc}}
\item{values}{Show flow rate at intervals boundaries}

See \code{\link[lattice]{xyplot}} for these
\item{type}{}
\item{scales}{}
\item{xlab}{}
\item{ylab}{}
\item{...}{}
\item{intervals}{Non-standard flow intervals definitions (currently broken)}
}
\seealso{
\code{\link{rankflow}}.
}
\examples{
data(flow)
data(pol)
ranked <- rankflow(flow, pol, names = list(date="Date", Q="cfs"))
options(digits=4) # can limit number of digits on plot value labels
fdplot(ranked, names=list(Q="cfs"), values=TRUE)
}

