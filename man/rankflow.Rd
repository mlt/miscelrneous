\name{rankflow}
\alias{rankflow}
\title{Calculates daily averages and ranks flow
}
\description{
Calculates daily averages and ranks flow
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rankflow(data, pol, names = list(date = "date"), pol.date, sort
                 = TRUE, method = "linear", mean = base::mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Data frame with flow (and optionally with pollutant) data
}
  \item{pol}{Data frame with pollutant data if not a part of data
%%     ~~Describe \code{pol} here~~
}
  \item{names}{List with column names in \var{data} having date and
    Q. Defaults to \code{list(Q = "Q", date="date")}}
  \item{pol.date}{Column name in pol with dates. Default to
    names$date. Ignored if pol is missing}
  \item{sort}{Sorts output by exceedance (necessary for correct plot)}
  \item{method}{How to rank data. Choices are : "linear", "pr",
    "ecdf". "linear" uses rank. For "pr" see
    \url{http://en.wikipedia.org/wiki/Percentile_rank}}
    \item{mean}{How to average pollutant data. Defaults to arithmetic
      average. Can be \code{function(x) exp(mean(log(x)))} for geometric
    mean}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  List with the following components
  \item{exc}{Exceedance level 0-100}
  \item{names$Q}{Daily flow data}
  \item{...}{Daily averages of everything else passed in data or pol}
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
  \code{\link{fdplot}}, \code{\link{tmdl}}
}
\examples{
data(flow)
data(pol)
ranked <- rankflow(flow, pol, names = list(date="Date", Q="cfs"))
names(ranked)

ldplot(ranked, names=list(pol="TSS", Q="cfs"), WQS = 5)
}
