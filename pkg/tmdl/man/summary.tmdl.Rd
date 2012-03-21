\name{summary.tmdl}
\alias{summary.tmdl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary for TMDL
}
\description{
Summary for TMDL
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{summary}{tmdl}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{tmdl object as returned by \code{\link{tmdl}}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A data frame with...
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
  \code{\link{tmdl}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(flow)
data(pol)
flow.ranked <- rankflow(flow,pol,names=list(date="Date",Q="cfs"))
fd.fig <- fdplot(flow.ranked,names=list(Q="cfs"),values=TRUE)
WQS <- 5 # mg/L => 10 NTU
WLA.construction = function(exc,LC,MOS,LC.fun,MOS.fun) {
  LA = ifelse(exc>75, LC.fun(75)-MOS.fun(75), LC - MOS)
  .01 * LA
}
MS4.Duluth = function(exc,LC,MOS,LC.fun,MOS.fun) {
  .16 * (LC-MOS-WLA.construction(exc,LC,MOS,LC.fun,MOS.fun))
}
WLA=list(WLA=WLA.construction, MS4=MS4.Duluth)
kr.tmdl <- tmdl(flow.ranked,names=list(Q="cfs",pol="TSS"),WQS=WQS, WLA=WLA)
summary(kr.tmdl)
}
