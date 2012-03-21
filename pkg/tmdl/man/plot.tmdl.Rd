\name{plot.tmdl}
\alias{plot.tmdl}
\title{Plots Load Duration Curve}
\description{
Plots Load Duration Curve
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{plot}{tmdl}(x, ...)
}
\arguments{
  \item{x}{tmdl object returned by \code{\link{tmdl}}
}
  \item{\dots}{other parameters for bwplot
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Trellis object with LDC
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{tmdl}}, \code{\link{ldplot}}
}
\examples{
data(flow)
data(pol)
flow.ranked <- rankflow(flow,pol,names=list(date="Date",Q="cfs"))
WQS <- 5 # mg/L => 10 NTU

# simple WLA definition
kr.tmdl <- tmdl(flow.ranked,names=list(Q="cfs",pol="TSS"),WQS=WQS, WLA=1)
plot(kr.tmdl, plot=list(WLA=TRUE))

# WLA as a list of functions
WLA.construction <- function(exc,LC,MOS,LC.fun,MOS.fun) {
  LA = ifelse(exc>75, LC.fun(75)-MOS.fun(75), LC - MOS)
  .01 * LA
}
MS4.Duluth <- function(exc,LC,MOS,LC.fun,MOS.fun) {
  .16 * (LC-MOS-WLA.construction(exc,LC,MOS,LC.fun,MOS.fun))
}
WLA <- list("WLA (constr.)"=WLA.construction, MS4=MS4.Duluth)
kr.tmdl <- tmdl(flow.ranked,names=list(Q="cfs",pol="TSS"),WQS=WQS, WLA=WLA)
fig <- plot(kr.tmdl, plot=list(MOS=TRUE, WLA=TRUE))
plot(fig)

# apply custom theme
update(fig, par.settings = theEconomist.theme())
}
