\name{mvplot}
\alias{mvplot}
\title{Monthly Variation plot
}
\description{
  Plots variability of the flow rate or \% exceedance by month.
}
\usage{
mvplot(data, names = list(), intervals = fi, exc = TRUE, scales = list(), xlab, ...)
}
\arguments{
  \item{data}{Result from \code{\link{rankflow}}
}
  \item{names}{Overrides default column names
%%     ~~Describe \code{names} here~~
}
  \item{intervals}{Currently broken
%%     ~~Describe \code{intervals} here~~
}
  \item{exc}{Plot variability of \% exceedance by month, or flow rate if
    \code{exc = FALSE}
}

See lattice::bwplot for details
\item{scales}{
%%     ~~Describe \code{scales} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
\item{\dots}{
  \itemize{
    \item{stats}{defaults to \code{\link{tmdl.stats}}, can be
      boxplot.stats as well}
    \item{coef}{Length of whiskers. Defaults to 10th and 90th
      percentiles if \code{stats=tmdl.stats}}
    \item{do.out}{Plots outliers if \emph{TRUE}}
    \item{...}{other parameters to lattice::bwplot}
  }
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Trellis object
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
