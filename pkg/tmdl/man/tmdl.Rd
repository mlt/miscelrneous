\name{tmdl}
\alias{tmdl}
\title{Calculates TMDL
}
\description{
Calculates TMDL
}
\usage{
tmdl(data, names = list(), mult = 2.446576, WQS = 0.1, target = WQS,
 WLA = NA, MOS = TRUE, intervals = fi, interp = "spline")
}
\arguments{
  \item{data}{Typically result of \code{\link{rankflow}}, a \code{\link{data.frame}} with columns "Q" & "exc".
 Those can be overriden using \var{names}
}
  \item{names}{List that can specify custom column names in
  data frame \var{data} for \var{Q} and \var{exc}
}
  \item{mult}{ Multiplier to convert flow rate times pollutant
    concentration to resulting load units.
    If flow rate is given in \emph{cfs} and pollutant concentration in
    \emph{ppm}, default \var{mult} yields load in \emph{kg/day}
}
  \item{WQS}{Water quality standard
}
  \item{target}{Same as \var{WQS}. Has precedence over \var{WQS}.
}
\item{WLA}{Wate Load Allocation
}
  \item{MOS}{Margin Of Safety. FALSE for implicit, 0-1 to use \% of
    Load Capacity, or TRUE to calculate MOS for flow interval as a
    difference between LC for median and low flows for a given flow interval
}
  \item{intervals}{Currently broken
}
  \item{interp}{How to interpolate WLA for different exceedance
    level. Used in plotting.
}
}
\details{
  \var{WLA} can be one of the following
  \itemize{
    \item{numeric}{ length of 1 or 5}
    \item{matrix of data.frame}{ with 5 columns}
    \item{list}{ of functions}
  }
}
\value{
  Object of class "tmdl", essentially a list with the following components...
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{TMDL}
