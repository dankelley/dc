#' Download and Cache an AMSR File
#' @name dc.amsr
#'
#' @description
#'
#' @template intro
#'
#' @details
#' \code{dc.amsr} downloads data from an AMSR satellite.
#'
#' @param year,month,day Numerical values of the year, month, and day
#' of the desired dataset. Note that one file is archived per day,
#' so these three values uniquely identify a dataset.
#' If \code{day} and \code{month} are not provided but \code{day} is,
#' then the time is provided in a relative sense, based on the present
#' date, with \code{day} indicating the number of days in the past.
#' Owing to issues with timezones and the time when the data
#' are uploaded to the server, \code{day=3} may yield the
#' most recent available data. For this reason, there is a
#' third option, which is to leave \code{day} unspecified, which
#' works as though \code{day=3} had been given.
#'
#' @param destdir A string naming the directory in which to cache the downloaded file.
#' The default is to store in the present directory, but many users find it more
#' helpful to use something like \code{"~/data/amsr"} for this, to collect all
#' downloaded amsr files in one place.
#'
#' @param server A string naming the server from which data
#' are to be acquired, sans the final underline and characters
#' after it. For example, the website
#' \code{http://data.remss.com/amsr2/bmaps_v08},
#' is indicated with \code{server="http://data.remss.com/amsr2/bmaps_v08"}.
#'
#' @param version Character string indicating the 'version' name in
#' the files.
#'
#' @template filenames
#'
#' @template debug
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @section History:
#' Experience with \code{download.amsr} in the \code{oce} package
#' suggests that server names may change over time.
#'
#' @examples
#'\dontrun{
#' ## The download takes several seconds.
#' f <- dc.amsr(2017, 1, 14) # Jan 14, 2017
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
#' @family functions that download ocean-related data
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
    destfile <- sprintf("f34_%4d%02d%02dv%s.gz", year, month, day, server)
    ## example
    ## http://data.remss.com/amsr2/bmaps_v07.2/y2015/m11/f34_20151101v7.2.gz
    ## http://data.remss.com/amsr2/bmaps_v08/y2015/m11/f34_20151101v8.gz
    url <- sprintf("%s_%s/y%4d/m%02d/%s", server, version, year, month, destfile)
    rval <- dc(url=url, destdir=destdir, destfile=destfile,
               dryrun=dryrun, force=force, debug=debug-1)
    dcDebug(debug, "} # dc.argoSearch", sep="", "\n", unindent=1)
    rval
}

