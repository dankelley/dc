#' Download and Cache a G1SST (SST data/model) File
#'
#' @name dc.g1sst
#'
#' @description
#'
#' @template intro
#'
#' @details
#' \code{dc.g1sst} downloads data from a G1SST server.
#'
#' @param year,month,day Numerical values of the year, month, and day
#' of the desired dataset.
#'
#' @param lonW, lonE Numerical values, in range -180 to 180, specifying
#' the longitudes of the western and eastern boundaries of the focus region.
#'
#' @param latS, latN Numerical values, in range -90 to 90, specifying
#' the latitudes of the southern and northern boundaries of the focus region.
#'
#' @param destdir A string naming the directory in which to cache the downloaded file.
#' The default is to store in the present directory, but many users find it more
#' helpful to use something like \code{"~/data/g1sst"} for this, to collect all
#' downloaded g1sst files in one place.
#'
#' @param server A string naming the server from which data
#' are to be acquired.
#'
#' @template filenames
#'
#' @template debug
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @section History:
#' Download URLs working as of 2018-02-07.
#'
#' @examples
#'\dontrun{
#' ## Scotian Shelf on Oct 14, 2015.
#' f <- dc.g1sst(2015, 10, 14, -66, -60, 41, 46) # October 14, 2015
#' library(oce)
#' d <- read.g1sst(f)
#' plot(d, "SST")
#' mtext(d[["time"]], side=3, line=0, adj=0)
#'}
#' @references
#' \enumerate{
#' \item source URL-construction tutorial \url{https://coastwatch.pfeg.noaa.gov/erddapinfo/index.html}
#' \item status page \url{https://coastwatch.pfeg.noaa.gov/erddap/status.html}
#'}
#'
#' @family functions that download ocean-related data
dc.g1sst <- function(year, month, day, lonW, lonE, latS, latN,
                     server="https://coastwatch.pfeg.noaa.gov/erddap/griddap",
                     destdir=".", destfile, force=FALSE, dryrun=FALSE,
                     debug=getOption("dcDebug", 0))
{
    if (missing(year) || missing(month) || missing(day))
        stop("Must give year, month, day")
    if (missing(lonW) || missing(lonE))
        stop("must give lonW and lonE")
    if (missing(latS) || missing(latN))
        stop("must give latS and latN")
    date <- sprintf("%4d-%02d-%02d", year, month, day)
    destfile <- paste("g1sst_", date, ".nc", sep="")
    url<- paste(server, "/jplG1SST.nc?SST",
                "%5B(", date, "T12:00:00Z)%5D",
                "%5B(", latS, "):(", latN, ")%5D",
                "%5B(", lonW, "):(", lonE, ")%5D", sep="")
    rval <- dc(url=url, destdir=destdir, destfile=destfile,
               dryrun=dryrun, force=force, debug=debug-1)
    dcDebug(debug, "} # dc.g1sst", sep="", "\n", unindent=1)
    rval
}

