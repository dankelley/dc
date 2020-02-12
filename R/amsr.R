#' Download and Cache an AMSR File
#'
#' @param year,month,day numerical values of the year, month, and day
#' of the desired dataset. Note that one file is archived per day,
#' so these three values uniquely identify a dataset.
#' If `day` and `month` are not provided but `day` is,
#' then the time is provided in a relative sense, based on the present
#' date, with `day` indicating the number of days in the past.
#' Owing to issues with timezones and the time when the data
#' are uploaded to the server, `day=3` may yield the
#' most recent available data. For this reason, there is a
#' third option, which is to leave `day` unspecified, which
#' works as though `day=3` had been given.
#' @template server
#' @param version character string indicating the 'version' name in
#' the files.
#' @template destdir
#' @template destfile
#' @template force
#' @template dryrun
#' @template debug
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @examples
#'\dontrun{
#' ## The download takes several seconds.
#' f <- dc.amsr(2017, 1, 14, destdir="~/data/amsr") # Jan 14, 2017
#' library(oce)
#' d <- read.amsr(f)
#' plot(d)
#' mtext(d[["filename"]], side=3, line=0, adj=0)
#'}
#' @references
#' \url{http://images.remss.com/amsr/amsr2_data_daily.html}
#' provides daily images going back to 2012. Three-day,
#' monthly, and monthly composites are also provided on that site.
#'
#' @family functions related to amsr data
#' @export
dc.amsr <- function(year, month, day,
                    server="http://data.remss.com/amsr2/bmaps",
                    version="v08",
                    destdir=".", destfile, force=FALSE, dryrun=FALSE,
                    debug=getOption("dcDebug", 0))
{
    ## http://data.remss.com/amsr2/bmaps_v08/y2016/m08/f34_20160801v8_d3d.gz
    if (missing(year) && missing(month)) {
        if (missing(day))
            day <- 3
        day <- abs(day)
        today <- as.POSIXlt(Sys.Date() - day)
        year <- 1900 + today$year
        month <- 1 + today$mon
        day <- today$mday
    }
    year <- as.integer(year)
    month <- as.integer(month)
    day <- as.integer(day)
    destfile <- sprintf("f34_%4d%02d%02d%s.gz", year, month, day, gsub("0", "", version))
    ## examples (with dates when tested)
    ## http://data.remss.com/amsr2/bmaps_v08/y2017/m01/f34_20170114v8.gz # 20180414
    ## http://data.remss.com/amsr2/bmaps_v08/y2015/m11/f34_20151101v8.gz # 20180414
    ## http://data.remss.com/amsr2/bmaps_v07.2/y2015/m11/f34_20151101v7.2.gz # unknown date
    url <- sprintf("%s_%s/y%4d/m%02d/%s", server, version, year, month, destfile)
    rval <- dc(url=url, destdir=destdir, destfile=destfile,
               dryrun=dryrun, force=force, debug=debug-1)
    dcDebug(debug, "} # dc.amsr() ", sep="", "\n", unindent=1)
    rval
}

