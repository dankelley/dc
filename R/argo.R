#' Download and Cache an Argo Dataset
#' @name dc.argo
#'
#' @description
#'
#' @template intro
#'
#' @details
#' \code{dc.argoId} downloads all data for the float with
#' a specified identifier, \code{dc.argoSearch} downloads the
#' most recent profile for all floats in a specified longitude,
#' latitude, and time box, and \code{dc.argo} is a wrapper that
#' calls either of the first two functions, depending on
#' whether the \code{id} argument is provided.
#'
#' In any case, the downloads are made from the USGODAE server [1]
#' by default (or from any other server that obeys the same directory
#' structure). Since the servers do not provide an API for such
#' downloads, the \code{dc.argo*} functions are forced to work by
#' constructing URLs that are devised based on inspection of queries
#' constructed from a GUI-style webpage [2]. This leads to a
#' brittleness that is discussed in \dQuote{Caution}.
#'
#' @section Caution:
#' The queries used by the \code{dc.argo} functions will fail
#' if USGODAE changes their system. For an example, USGODAE presently looks
#' for the substring \code{".submit=++Go++"} in the query, but if this
#' were to be switched to \code{".submit=Go"}, a seemingly trivial
#' change, then \code{dc.argo} would fail entirely. For this reason,
#' the \code{read.argo*} functions may fail at any time. Users who
#' encounter this problem should contact the author, who may be able
#' to find a way to reverse-engineer the updated Argo system.
#'
#' @param id A character value indicating the ID of a particular
#' argo float. If this is provided, then \code{longitude}
#' \code{latitude} and \code{time} are ignored, and \code{dc.argo}
#' downloads a file that contains all the profiles for the
#' named float.
#'
#' @param longitude Two-element numerical vector holding the limits
#' of longitude (degrees East) to search for Argo profiles.
#'
#' @param latitude Two-element numerical vector holding the
#' latitude limits (degrees North).
#'
#' @param time Two-element character vector holding the
#' time limits in YYYY-MM-DD format.
#'
#' @param server String indicating the server. The default is
#' \code{"www.usgodae.org"}, and it is unlikely that other values
#' will work correctly, unless they are mirrors of this site, using
#' the same URL structure and file formats.
#'
#' @template filenames
#'
#' @template debug
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @examples
#' library(dc)
#'\dontrun{
#' library(oce)
#' ## 1. Get float profiles near Nova Scotia on Remembrance Day, 2017,
#' ##    and plot the first one as a CTD.
#' n <- dc.argo(longitude=c(-65,-55), latitude=c(40,50),
#'              time=c("2017-11-11","2017-11-11"))
#' a <- read.argo(n[1])
#' plotTS(as.ctd(a))
#'
#' ## 2. All profiles for a particular float.
#' N <- dc.argo(id="4902912")
#' A <- read.argo(N)
#' plot(A, type="l") # trajectory
#'}
#'
#' @references
#' 1. \url{http://www.usgodae.org}
#'
#' 2. \url{http://www.usgodae.org/cgi-bin/argo_select.pl}
#'
#' @seealso The work is done with \code{\link{dc}}.
#'
#' @author Dan Kelley (2017-11-26)
#'
#' @family functions that download ocean-related data
dc.argo <- function(id,
                    longitude, latitude, time, server="www.usgodae.org",
                    destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                    debug=getOption("dcDebug", 0))
{
    dcDebug(debug, "dc.argo(...) {", sep="", "\n", unindent=1)
    idGiven <- !missing(id)
    longitudeGiven <- !missing(longitude)
    latitudeGiven <- !missing(latitude)
    timeGiven <- !missing(time)
    rval <- NULL
    if (idGiven) {
        if (longitudeGiven)
            warning("longitude must not be provided, if id was provided\n")
        if (latitudeGiven)
            warning("latitude must not be provided, if id was provided\n")
        if (timeGiven)
            warning("time must not be provided, if id was provided\n")
        rval <- dc.argoID(id=id,
                          server=server, destdir=destdir, destfile=destfile,
                          force=force, dryrun=dryrun, debug=debug-1)
    } else {
        rval <- dc.argoSearch(longitude=longitude, latitude=latitude, time=time,
                              server=server, destdir=destdir, destfile=destfile,
                              force=force, dryrun=dryrun, debug=debug-1)
    }
    dcDebug(debug, "} # dc.argo", sep="", "\n", unindent=1)
    rval
}

#' @rdname dc.argo
dc.argoID<- function(id=NULL,
                     server="www.usgodae.org",
                     destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                     debug=getOption("dcDebug", 0))
{
    dcDebug(debug, "dc.argoID(id=\"", id, "\", ...) {", sep="", "\n", unindent=1)
    ## http://www.usgodae.org/ftp/outgoing/argo/ar_index_global_meta.txt.gz
    indexURL <- paste("http://", server, "/ftp/outgoing/argo/ar_index_global_meta.txt.gz", sep="")
    dcDebug(debug, "indexURL", indexURL, "\n")
    con <- gzcon(url(indexURL))
    index <- readLines(con)
    ## An example file starts as follows (downloaded 2017-12-01)
    ## # Title : Metadata directory file of the Argo Global Data Assembly Center
    ## # Description : The directory file describes all metadata files of the argo GDAC ftp site.
    ## # Project : ARGO
    ## # Format version : 2.0
    ## # Date of update : 20171202121617
    ## # FTP root number 1 : ftp://ftp.ifremer.fr/ifremer/argo/dac
    ## # FTP root number 2 : ftp://usgodae.org/pub/outgoing/argo/dac
    ## # GDAC node : NRL-MRY
    ## file,profiler_type,institution,date_update
    ## aoml/13857/13857_meta.nc,845,AO,20120521144513
    ## aoml/13858/13858_meta.nc,845,AO,20130222100319
    dcDebug(debug, "downloaded index file containing", length(index), "lines\n")
    close(con)
    header <- grep("^#.*$", index)
    if (length(header))
        index <- index[-header]
    i <- read.csv(text=index, header=TRUE, stringsAsFactors=FALSE)
    lines <- grep(id, i$file)
    if (0 == length(lines))
        stop("No id '", id, "' found in index file ", indexURL)
    lines <- lines[1]
    dcDebug(debug, "line ", lines, " of index file refers to id \"", id, "\"\n", sep="")
    ## e.g. aoml/4902912/4902912_meta.nc
    dac <- gsub("^([^/]*)/.*$", "\\1", i$file[lines])
    dcDebug(debug, "infer dac \"", dac, "\"\n", sep="")
    ## e.g. http://www.usgodae.org/ftp/outgoing/argo/dac/aoml/4902912/4902912_prof.nc
    destfile <- paste(id, "_prof.nc", sep="")
    url <- paste("http://", server, "/ftp/outgoing/argo/dac/", dac, "/", id, "/", destfile, sep="")
    rval <- dc(url=url, destdir=destdir, destfile=destfile, dryrun=dryrun, force=force, debug=debug-1)
    dcDebug(debug, "} # dc.argoID", sep="", "\n", unindent=1)
    rval
}

#' @rdname dc.argo
dc.argoSearch <- function(id=NULL, longitude, latitude, time,
                          server="www.usgodae.org",
                          destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                          debug=getOption("dcDebug", 0))
{
    if (missing(longitude))
        stop("longitude must be provided, unless id is provided")
    if (length(longitude) != 2)
        stop("longitude must be a two-element vector")
    if (missing(latitude))
        stop("latitude must be provided, unless id is provided")
    if (length(latitude) != 2)
        stop("latitude must be a two-element vector")
    if (missing(time))
        stop("time must be provided, unless id is provided")
    if (length(time) != 2)
        stop("time must be a two-element vector")
    if (!is.character(time))
        stop("time must be a character vector")
    if (nchar(time[1]) != 10)
        stop("time[1] must be a 10-character string")
    if (nchar(time[2]) != 10)
        stop("time[2] must be a 10-character string")
    startyear <- substr(time[1], 1, 4)
    startmonth <- substr(time[1], 6, 7)
    startday <- substr(time[1], 9, 10)
    endyear <- substr(time[2], 1, 4)
    endmonth <- substr(time[2], 6, 7)
    endday <- substr(time[2], 9, 10)
    ## http://www.usgodae.org/cgi-bin/argo_select.pl?startyear=2017&startmonth=11&startday=26&endyear=2017&endmonth=11&endday=26&Nlat=60&Wlon=-70&Elon=-30&Slat=30&dac=ALL&floatid=ALL&gentype=plt&.submit=++Go++&.cgifields=endyear&.cgifields=dac&.cgifields=delayed&.cgifields=startyear&.cgifields=endmonth&.cgifields=endday&.cgifields=startday&.cgifields=startmonth&.cgifields=gentype

    ## http://www.usgodae.org/cgi-bin/argo_select.pl?startyear=2017&startmonth=11&startday=26&endyear=2017&endmonth=11&endday=26&Nlat=60&Wlon=-70&Elon=-30&Slat=30&dac=ALL&floatid=ALL&gentype=txt&.submit=++Go++&.cgifields=endyear&.cgifields=dac&.cgifields=delayed&.cgifields=startyear&.cgifields=endmonth&.cgifields=endday&.cgifields=startday&.cgifields=startmonth&.cgifields=gentype

    ## http://www.usgodae.org/cgi-bin/argo_select.pl?startyear=2017&startmonth=11&startday=26&endyear=2017&endmonth=11&endday=26&Nlat=60&Wlon=-70&Elon=-30&Slat=30&dac=ALL&floatid=ALL&gentype=txt&.submit=++Go++

    baseURL <- paste("http://", server, "/cgi-bin/argo_select.pl?", sep="")
    dcDebug(debug, "baseURL: ", baseURL, "\n")
    queryTime <- sprintf("startyear=%s&startmonth=%s&startday=%s&endyear=%s&endmonth=%s&endday=%s",
                         startyear, startmonth, startday, endyear, endmonth, endday)
    dcDebug(debug, "queryTime: ", queryTime, "\n")
    queryLocation <- sprintf("&Nlat=%s&Wlon=%s&Elon=%s&Slat=%s",
                             latitude[2], longitude[1], longitude[2], latitude[1])
    dcDebug(debug, "queryLocation: ", queryLocation, "\n")
    queryOutput <- "&dac=ALL&floatid=ALL&gentype=txt&.submit=++Go++"
    dcDebug(debug, "queryOutput: ", queryOutput, "\n")
    query <- paste(baseURL, queryTime, queryLocation, queryOutput, sep="")
    dcDebug(debug, "query: ", query, "\n")
    con <- url(query, open="r")
    ## Turn off warnings before reading to avoid warnings about a missing
    ## newline at the end.
    owarn <- options("warn")$warn
    options(warn=-1)
    lines <- readLines(con)
    nlines <- length(lines)
    options(warn=owarn)
    close(con)
    pattern <- "select name=\"float_list\""
    start <- grep(pattern, lines)
    if (0 == length(start))
        stop("cannot find a line matching '", pattern, "' in the HTML generated by ", query)
    focus <- lines[start[1]:nlines]
    pattern <- "<option value="
    profileLines <- grep(pattern, focus)
    if (0 == length(profileLines))
        stop("cannot find a line matching '", pattern, "' in the HTML (starting at line ", start[1], " generated by ", query)
    dcDebug(debug, "profile info found on lines ", paste(profileLines + start[1] - 1, collapse=" "), "\n")

    ## <option value=\"meds/4901765/profiles/R4901765_132.nc\">20171126 | 0600 | 40.398N | -66.487E | 4901765 | meds | R4901765_132.nc</option>
    url <- gsub('<option value="([^"]*)".*', "\\1", focus[profileLines])
    prefix <- "http://www.usgodae.org/ftp/outgoing/argo/dac/"
    url <- paste(prefix, url, sep="")
    destfile <- gsub("^.*/", "", url)
    ## Below is standard code that should be mimicked at the end of every dc.x() function.
    rval <- dc(url=url, destdir=destdir, destfile=destfile, dryrun=dryrun, force=force, debug=debug-1)
    dcDebug(debug, "} # dc.argoSearch", sep="", "\n", unindent=1)
    rval
}

