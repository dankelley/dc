#' Download and Cache a Coastline File
#'
#' Constructs a query to the NaturalEarth server (ref 1) to download coastline
#' data (or lake data, river data, etc) in any of three resolutions,
#' and caches the resultant file.
#'
#' @param resolution A character value specifying the desired resolution. The permitted
#' choices are `"10m"` (for 1:10M resolution, the most detailed),
#' `"50m"` (for 1:50M resolution)
#' and `"110m"` (for 1:110M resolution). If `resolution` is not supplied,
#' `"50m"` will be used.
#' @param item A character value indicating the quantity to be downloaded.
#' This is normally one of `"coastline"`, `"land"`, `"ocean"`,
#' `"rivers_lakes_centerlines"`, or `"lakes"`, but the NaturalEarth
#' server has other types, and advanced users can discover their names by inspecting
#' the URLs of links on the NaturalEarth site, and use them for `item`.
#' If `item` is not supplied, it defaults to `"coastline"`.
#' @template server
#' @template destdir
#' @template destfile
#' @template force
#' @template dryrun
#' @template debug
#'
#' @return A character value indicating the filename of the result; if
#' there is a problem of any kind, the result will be the empty
#' string.
#'
#' @seealso The work is done with [utils::download.file()].
#'
#' @examples
#'\dontrun{
#' library(dc)
#' library(oce)
#' # User must create directory ~/data/coastline first.
#' # As of September 2016, the downloaded file, named
#' # "ne_50m_coastline.zip", occupies 443K bytes.
#' filename <- dc.coastline(destdir="~/data/coastline")
#' coastline <- read.coastline(filename)
#' plot(coastline)
#'}
#'
#' @seealso The work is done with [dc()].
#'
#' @references
#' 1. The NaturalEarth server is at \url{http://www.naturalearthdata.com}
#'
#' @family functions that download ocean-related data
#' @export
dc.coastline <- function(resolution, item="coastline",
                         server="naturalearth",
                         destdir=".", destfile, force=FALSE, dryrun=FALSE,
                         debug=getOption("oceDebug"))
{
    if (missing(resolution))
        resolution <- "50m"
    resolutionChoices <- c("10m", "50m", "110m")
    if (!(resolution %in% resolutionChoices))
        stop("'resolution' must be one of: '", paste(resolutionChoices, collapse="' '"), "'")
    if (server == "naturalearth")
        urlBase <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download"
    else
        stop("the only server that works is naturalearth")
    filename <- paste("ne_", resolution, "_", item, ".zip", sep="")
    if (missing(destfile))
        destfile <- filename
    url <- paste(urlBase, "/", resolution, "/physical/", filename, sep="")
    rval <- dc(url=url, destdir=destdir, destfile=destfile, dryrun=FALSE, force=FALSE, debug=debug-1)
    dcDebug(debug, "} # dc.coastline", sep="", "\n", unindent=1)
    rval
}

