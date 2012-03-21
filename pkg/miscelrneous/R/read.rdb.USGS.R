##' Read RDB database format from USGS
##'
##' .. content for \details{} ..
##' @title read.rdb.USGS
##' @param f file or connection
##' @return \code{data.frame} with time series
##' @author Mikhail Titov
##' @examples
##' begin_date <- ISOdate(2011,03,21)
##' end_date <- ISOdate(2012,03,19)
##' l <- list(
##'           site_no="05355200",
##' #          multiple_site_no="05355200",
##'           cb_00060="on",
##'           format="rdb",
##'           begin_date=format(begin_date,"%Y-%m-%d"),
##'           end_date=format(end_date,"%Y-%m-%d")
##'           )
##' myurl <- url.make.USGS(list(path="nwis/dv"), l)
##' #f <- file("USGS_05355200.rdb")
##' f <- url(myurl)
##' x <- read.rdb.USGS(f)
##' plot(X04_00060_00003~datetime,x,"o",col=x$X04_00060_00003_cd)
##' @export
read.rdb.USGS <- function(f) {
    open(f)
    repeat {
        l <- readLines(f,1)
        if (grepl("^[^#]",l)) break
    }
    cols <- unlist(strsplit(l,"\t"))
    dummy <- readLines(f,1)
    x <- read.table(f, sep="\t", col.names=cols,
                    colClasses=c(datetime="POSIXct"),
                    na.strings=c("Ice"))
    close(f)
    x
}
