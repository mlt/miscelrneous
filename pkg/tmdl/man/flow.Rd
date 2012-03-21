\name{flow}
\alias{flow}
\docType{data}
\title{Flow data 1974-2006 for USGS gage 04015330
}
\description{
Flow data for Knife River
}
\usage{data(flow)}
\format{
  A data frame with 30259 observations on the following 2 variables.
  \describe{
    \item{\code{Date}}{a POSIXt date of observation}
    \item{\code{cfs}}{flow rate in \emph{cfs}, \emph{Ice} was replaced to \code{NA}}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
  \url{http://waterdata.usgs.gov/mn/nwis/uv?04015330}
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(flow)
plot(flow, type='l')
}
\keyword{datasets}
