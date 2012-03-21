\name{ldplot}
\alias{ldplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots Load Duration Curve}
\description{
  It is better to call \code{\link{tmdl}} first and then use S3 method \code{\link{plot.tmdl}}.
}
\usage{
ldplot(data, names = list(), mult = 2.446576, WQS = 0.1, target = WQS,
WLA = NA, intervals = fi, coef = 0.1, do.out = FALSE,
 xlab = "Flow Duration Interval (\%)", ylab = "Load",
key = NULL, legend = NULL, scales = list(), plot = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Result of \code{\link{rankflow}}
}
  \item{names}{Non-default column names in data
}
  \item{mult}{Multiplier
}
  \item{WQS}{Water Quality Standard
}
  \item{target}{Same as WQS. Has precedance over WQS
}
  \item{WLA}{Waste Load Allocation
}
  \item{intervals}{Currently broken
  }
  \item{coef}{Influences whiskers calculation
}
  \item{do.out}{Whether to plot outliers
  }
  \item{plot}{What should be plotted list. Defaults to list(MOS=FALSE, WLA=FALSE)
%%     ~~Describe \code{plot} here~~
}

  See lattice::xyplot for the following
  
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{scales}{
%%     ~~Describe \code{scales} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
}
