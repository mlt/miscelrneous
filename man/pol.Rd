\name{pol}
\alias{pol}
\docType{data}
\title{Turbidity data 2003-2006 for STORET ID S003-642
}
\description{
Turbidity data for Knife River, MN
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(pol)}
\format{
  A data frame with 68 observations on the following 3 variables.
  \describe{
    \item{\code{Date}}{a POSIXt}
    \item{\code{Turb}}{Turbidity, \emph{NTU}}
    \item{\code{TSS}}{Total Suspended Sediments, \emph{mg/L}}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
  \url{http://www.pca.state.mn.us/data/eda/STresults.cfm?stID=S003-642&stOR=MNPCA1}
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(pol)
plot(TSS~Date, pol)

plot(log10(TSS)~log10(Turb),pol)
pol.lm <- lm(log10(TSS)~log10(Turb), pol)
abline(pol.lm)
}
\keyword{datasets}
