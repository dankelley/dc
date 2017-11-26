#' Download and Cache an Argo Dataset
#'
#' @description
#'
#' @template intro
#'
#' @details
#' \code{dc.argo} downloads Argo profiles from a USGODAE server [1], by
#' constructing queries that are devised based on inspection of queries
#' constructed from the GUI-style webpage [2]. Profiles are specified
#' in a location-time window.  See \dQuote{Caution} for notes on the
#' brittleness of the procedure used here.
#'
#' @section Caution:
#' USGODAE does not provide an API for downloading data, and so the
#' queries used by \code{dc.argo} will not work properly if USGODAE
#' changes their system. For an example, USGODAE presently looks for
#' the substring \code{".submit=++Go++"} in the query, but if this
#' gets switched to \code{".submit=Go"}, then \code{dc.argo} will
#' fail.  It would certainly be preferable to use
#' a more formal application-programmer-interface, but until USGODAE
#' provide such an API, \code{dc.argo} provides a stopgap measure
#' that avoids the requirement to interact with a web interface using
#' the mouse and keyboard.
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
#' \code{"cchdo.ucsd.edu"}, and it is unlikely that other values
#' will work correctly, unless they are mirrors of this site, using
#' the same URL structure.
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
#' ## 1. 
#' a <- dc.argo(lon=c(-70,-30), lat=c(30,60),time=c("2017-11-26","2017-11-26"))
#'}
#'
#' @references
#' 1. \url{https://www.usgodae.org}
#'
#' 2. \url{http://www.usgodae.org/cgi-bin/argo_select.pl}
#'
#' @author Dan Kelley (2017-11-26)
dc.argo <- function(longitude, latitude, time, server="www.usgodae.org",
                    destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                    debug=getOption("dcDebug", 0))
{
    dcDebug(debug, "dc.argo(...) {", sep="", "\n", unindent=1)
    if (missing(longitude))
        stop("longitude must be provided")
    if (length(longitude) != 2)
        stop("longitude must be a two-element vector")
    if (missing(latitude))
        stop("latitude must be provided")
    if (length(latitude) != 2)
        stop("latitude must be a two-element vector")
    if (missing(time))
        stop("time must be provided")
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


    ## below is hand-cleaned
    eg <- "http://www.usgodae.org/cgi-bin/argo_select.pl?startyear=2017&startmonth=11&startday=26&endyear=2017&endmonth=11&endday=26&Nlat=60&Wlon=-70&Elon=-30&Slat=30&dac=ALL&floatid=ALL&gentype=txt&.submit=++Go++"

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
    options(warn=owarn)
    close(con)
    pattern <- "select name=\"float_list\""
    start <- grep(pattern, lines)
    if (0 == length(start))
        stop("cannot find a line matching '", pattern, "' in the HTML generated by ", query)
    focus <- lines[start[1] + 0:10]
    pattern <- "<option value="
    profileLines <- grep(pattern, focus)
    if (0 == length(profileLines))
        stop("cannot find a line matching '", pattern, "' in the HTML (starting at line ", start[1], " generated by ", query)
    dcDebug(debug, "profile info found on lines ", paste(profileLines + start[1] - 1, collapse=" "), "\n")

    ## <option value=\"meds/4901765/profiles/R4901765_132.nc\">20171126 | 0600 | 40.398N | -66.487E | 4901765 | meds | R4901765_132.nc</option>
    urls <- gsub('<option value="([^"]*)".*', "\\1", focus[profileLines])
    prefix <- "http://www.usgodae.org/ftp/outgoing/argo/dac/"
    urls <- paste(prefix, urls, sep="")
    warning("dc.argo() is NOT downloading the data yet, just finding URLs for the data\n")
    warning("try e.g.\n\tu<-dc.argo(etc)\n\tdownload.file(u[1],\"test.nc\")\n\ta<-read.oce(\"test.nc\")\n\tplotTS(a)\n")
    return(urls)
    dcDebug(debug, "urls: ", paste(urls, collapse=" "), "\n")
    browser()
    ## Below is standard code that should be used at the end of every dc.x() function.
    destination <- paste(destdir, destfile, sep="/")
    dcDebug(debug, "url:", url, "\n")
    if (dryrun) {
        cat(url, "\n")
    } else {
        isZipfile <- 1 == length(grep(".zip$", destfile))
        destfileClean <- if (isZipfile) gsub(".zip$", "", destfile) else destfile
        destinationClean <- gsub(".zip$", "", destination)
        if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfileClean, "$", sep="")))) {
            dcDebug(debug, "Not downloading \"", destfileClean, "\" because it is already present in the \"", destdir, "\" directory\n", sep="")
            destination <- destinationClean
        } else {
            download.file(url, destination)
            dcDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
            if (isZipfile) {
                unzip(destination, exdir=destinationClean)
                dcDebug(debug, "unzipped data into '", destinationClean, "'\n", sep="")
                destination <- destinationClean
            }
        }
    }
    dcDebug(debug, "} # dc.argo", sep="", "\n", unindent=1)
    destination
}

