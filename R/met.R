#' Download and Cache a Meteorology File
#'
#' The data are downloaded with [utils::download.file()]
#' pointed to the Environment Canada website (ref 1)
#' using queries that had to be devised by reverse-engineering, since the agency
#' does not provide documentation about how to construct queries. Caution: the
#' query format changes from time to time, so [dc.met()] may work one
#' day, and fail the next.
#'
#' The constructed query contains Station ID, as provided in the `id` argument.
#' Note that this seems to be a creation of Environment Canada, alone;
#' it is distinct from the more standard "Climate ID" and "WMO ID".
#' To make things more difficult, Environment Canada states that the
#' Station ID is subject to change over time. (Whether this applies to existing
#' data is unclear.)
#'
#' Given these difficulties with Station ID, users are advised to consult
#' the Environment Canada website (ref 1) before downloading any data,
#' and to check it from time to time
#' during the course of a research project, to see if the Station ID has changed.
#' It can be very helpful to use Gavin Simpson's
#' `canadaHCD` package (ref 2) to look up Station IDs. This package maintains
#' a copy of the Environment Canada listing of stations, and its
#' `find_station` function provides an easy way to determine Station IDs.
#' After that, its `hcd_hourly` function (and related functions) make
#' it easy to read data.
#'
#' @param id A number giving the "Station ID" of the station of interest.
#' @param year A number giving the year of interest. Ignored unless `deltat`
#' is `"hour"`. If `year` is not given, it defaults to the present year.
#' @param month A number giving the month of interest. Ignored unless `deltat`
#' is `"hour"`. If `month` is not given, it defaults to the present
#' month.
#' @param deltat Optional character string indicating the time step of the
#' desired dataset. Only `"hour"` or `"month"` are permitted.
#' If `deltat` is not given, it defaults to `"hour"`.
#' @template destdir
#' @template destfile
#' @template force
#' @template dryrun
#' @template debug
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' ## Download data for Halifax International Airport, in September
#' ## of 2003. (This dataset is used for data(met) in the oce package.)
#' library(dc)
#' metFile <- dc.met(6358, 2003, 9, destdir="~/data/met")
#' library(oce)
#' met <- read.met(metFile)
#' plot(met)
#'}
#'
#' @seealso The work is done with [dc()].
#'
#' @references
#' 1. Environment Canada website for Historical Climate Data
#' \url{http://climate.weather.gc.ca/index_e.html}
#'
#' 2. Gavin Simpson's `canadaHCD` package on GitHub
#' \url{https://github.com/gavinsimpson/canadaHCD}
#'
#' @author Dan Kelley 2017-09-16
#'
#' @family functions that download ocean-related data
#' @export
dc.met <- function(id, year, month, deltat="hour",
                   destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                   debug=getOption("dcDebug", 0))
{
    if (missing(id))
        stop("must provide a station identifier, as the 'id' argument")
    id <- as.integer(id)
    deltatChoices <- c("hour", "month") # FIXME: add "day"
    deltatIndex <- pmatch(deltat, deltatChoices)
    if (is.na(deltatIndex))
        stop("deltat=\"", deltat, "\" is not supported; try \"hour\" or \"month\"")
    deltat <- deltatChoices[deltatIndex]
    if (deltat == "hour") {
        today <- as.POSIXlt(Sys.time())
        if (missing(year))
            year <- today$year + 1900
        if (missing(month)) {
            month <- today$mon + 1         # so 1=jan etc
            month <- month - 1             # we want *previous* month, which should have data
            if (month == 1) {
                year <- year - 1
                month <- 12
            }
        }
        ## Next line is an example that worked as of Feb 2, 2017
        ## http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=6358&Year=2003&Month=9&timeframe=1&submit=Download+Data
        url <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=",
                     id, "&Year=", year, "&Month=", month, "&timeframe=1&submit=Download+Data", sep="")
        if (missing(destfile))
            destfile <- sprintf("met_%d_hourly_%04d_%02d_%02d.csv", id, year, month, 1)
    } else if (deltat == "month") {
        ## Next line reverse engineered from monthly data at Resolute. I don't imagine we
        ## need Year and Month and Day.
        url <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?stationID=",
                     id, "&format=csv&timeframe=3&submit=Download+Data", sep="")
                     ##id, "&Year=2000&Month=1&Day=14&format=csv&timeframe=3&submit=%20Download+Data", sep="")
        if (missing(destfile))
            destfile <- sprintf("met_%d_monthly.csv", id)
    } else {
        stop("deltat must be \"hour\" or \"month\"")
    }
    rval <- dc(url=url, destdir=destdir, destfile=destfile, dryrun=dryrun, force=force, debug=debug-1)
    dcDebug(debug, "} # dc.met", sep="", "\n", unindent=1)
    rval
}

