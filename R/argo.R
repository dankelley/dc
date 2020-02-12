##JUNK # this was VERY slow; we an do this with as.POSIXct() anyway.
##JUNK #' Convert string date to POSIXct time
##JUNK #'
##JUNK #' @param t character value giving the time in yyyymmddHHMMSS format,
##JUNK #' as illustrated in \dQuote{Examples}.
##JUNK #' @param tz character value holding the timezone.  It seems unlikely
##JUNK #' that a value other than `"UTC"` will be appropriate, so that
##JUNK #' is the default.
##JUNK #'
##JUNK #' @param tz character string
##JUNK #' @return POSIXct time
##JUNK #'
##JUNK #' @examples
##JUNK #' decodeTextDate("19970729200300") ## 1997 07 29 20 03 00
##JUNK #'
##JUNK #' @export
##JUNK decodeTextDate <- function(t, tz="UTC")
##JUNK {
##JUNK     t <- as.character(t)
##JUNK     ISOdatetime(substr(t,1,4), substr(t,5,6), substr(t,7,8),
##JUNK                 substr(t,9,10), substr(t,11,12), substr(t,13,14))
##JUNK }

#' Download and Cache an Argo Dataset
#'
#' Download and cache data from an argo profiling float.
#'
#' [dc.argoById()] downloads data for the float with
#' a specified identifier, [dc.argoSearch()] downloads the
#' most recent profile for all floats in a specified longitude,
#' latitude, and time box, and `dc.argo` is a wrapper that
#' calls either of the first two functions, depending on
#' whether the `id` argument is provided.
#'
#' In any case, the downloads are made from the USGODAE server (ref 1)
#' by default (or from any other server that obeys the same directory
#' structure). Since the servers do not provide an API for such
#' downloads, the `dc.argo*` functions are forced to work by
#' constructing URLs that are devised based on inspection of queries
#' constructed from a GUI-style webpage (ref 2). This leads to a
#' brittleness that is discussed in \dQuote{Caution}.
#'
#' @section Caution:
#' The queries used by the `dc.argo` functions will fail
#' if USGODAE changes their system. For an example, USGODAE presently looks
#' for the substring `".submit=++Go++"` in the query, but if this
#' were to be switched to `".submit=Go"`, a seemingly trivial
#' change, then `dc.argo` would fail entirely. For this reason,
#' the `read.argo*` functions may fail at any time. Users who
#' encounter this problem should contact the author, who may be able
#' to find a way to reverse-engineer the updated Argo system.
#'
#' @param id a character string giving the
#' float ID to be queried.
#' argo float. If this is provided, then `longitude`,
#' `latitude` and `time` are ignored, and [dc.argo()]
#' downloads a file that contains all the profiles for the
#' named float. **FIXME/dk: I think this text is garbled and out of date.**
#' @param longitude Two-element numerical vector holding the limits
#' of longitude (degrees East) to search for Argo profiles.
#' @param latitude Two-element numerical vector holding the
#' latitude limits (degrees North).
#' @param time Two-element character vector holding the
#' time limits in YYYY-MM-DD format.
#' @template server
#' @template destdir
#' @template destfile
#' @template force
#' @template dryrun
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
#'              time=c("2017-11-11","2017-11-11"), destdir="~/data/argo")
#' a <- read.argo(n[1])
#' plotTS(as.ctd(a))
#'
#' ## 2. All profiles for a particular float.
#' N <- dc.argo(id="4902912", destdir="~/data/argo")
#' A <- read.argo(N)
#' plot(A, type="l") # trajectory
#'}
#'
#' @references
#' 1. \url{http://.usgodae.org}
#'
#' 2. \url{http://www.usgodae.org/cgi-bin/argo_select.pl}
#'
#' @author Dan Kelley
#'
#' @family functions related to argo data
## @export
dc.argo <- function(id,
                    longitude, latitude, time, server="ftp://usgodae.org",
                    destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                    debug=getOption("dcDebug", 0))
{
    dcDebug(debug, "dc.argo(...) {", sep="", "\n", style="bold", unindent=1)
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
        rval <- dc.argoById(id=id,
                            server=server, destdir=destdir, destfile=destfile,
                            force=force, dryrun=dryrun, debug=debug-1)
    } else {
        rval <- dc.argoSearch(longitude=longitude, latitude=latitude, time=time,
                              server=server, destdir=destdir, destfile=destfile,
                              force=force, dryrun=dryrun, debug=debug-1)
    }
    dcDebug(debug, "} # dc.argo", sep="", "\n", style="bold", unindent=1)
    rval
}

#' Download and cache an argo file specified by id
#'
#' @param id character value giving the Argo float id. In this version
#' of [dc.argoById()], the value of `id` must be a full URL for the netcdf
#' file, and an error is reported if `id` does not start with `ftp://`.
#' @param server ignored
#' @template destdir
#' @param destfile optional character value that specifies the name to be used
#' for the downloaded file. If this is not specified, then a name is determined
#' from the value of `id`.
#' @template force
#' @template dryrun
#' @template quiet
#' @template debug
#' @family functions related to argo data
#' @export
#' @examples
#' # These examples assume that the ~/data/argo directory exists and is readable.
#'\dontrun{
#' library(oce)
#' # Case 1: id is a URL; server is ignored.
#' id <- "ftp://ftp.ifremer.fr/ifremer/argo/dac/nmdis/2901633/profiles/R2901633_071.nc"
#' f <- dc.argoById(id, destdir="~/data/argo")
#' a <- read.oce(f)
#' summary(a) # an oce object of class 'argo'
#' par(mfrow=c(2, 2))
#' plot(a, which="map")
#' mtext(a[["time"]], cex=par("cex"))
#' mtext(gsub(".*/", "", f), cex=par("cex"), line=-1)
#' plot(a, which="TS")
#' plot(a, which="temperature profile")
#' plot(a, which="salinity profile")
#'}
#' @importFrom curl curl_download
#' @export
dc.argoById <- function(id=NULL,
                        server,
                        destdir=".", destfile,
                        force=FALSE, dryrun=FALSE,
                        quiet=FALSE, debug=getOption("dcDebug", 0))
{
    dcDebug(debug, "dc.argoById(id=\"", id, "\", destdir=\"", destdir, "\", ...) {", sep="", "\n", style="bold", unindent=1)
    ## If the ID starts with ftp://, thn we just download the file directly, ignoring server
    if (grepl("^ftp://", id)) {
        if (missing(destfile))
            destfile <- gsub(".*/(.*).nc", "\\1.nc", id)
        dcDebug(debug, "originally, destfile='", destfile, "'\n", sep="")
        destfile <- paste0(destdir, "/", destfile)
        dcDebug(debug, "after resolving destdir, destfile='", destfile, "'\n", sep="")
        if (!dryrun && (force || !file.exists(destfile)))
            curl::curl_download(url=id, destfile=destfile, quiet=quiet, mode="wb")
        dcDebug(debug, "} # dc.argoById()", sep="", "\n", style="bold", unindent=1)
        return(destfile)
    } else {
        stop("the id must start with \"ftp://\" -- contact author if you need this limitation to be lifted")
    }
    ##> ##
    ##> ## FIXME(dk): see how much of the following, which is a remnant from an older
    ##> ## FIXME(dk): version, is useful. My current thinking is that it is not, i.e.
    ##> ## FIXME(dk): I think users ought to be working from full indices.
    ##> ##
    ##> ##> http://www.usgodae.org/ftp/outgoing/argo/ar_index_global_meta.txt.gz
    ##> ##> indexURL <- paste("http://", server, "/ftp/outgoing/argo/ar_index_global_meta.txt.gz", sep="")
    ##> ##
    ##> ## ftp://usgodae.org/pub/outgoing/argo/ar_index_global_meta.txt
    ##> indexURL <- paste0(server, "/pub/outgoing/argo/ar_index_global_meta.txt.gz")
    ##> dcDebug(debug, "indexURL", indexURL, "\n")
    ##> con <- gzcon(url(indexURL))
    ##> index <- readLines(con)
    ##> ## An example file starts as follows (downloaded 2017-12-01)
    ##> ## # Title : Metadata directory file of the Argo Global Data Assembly Center
    ##> ## # Description : The directory file describes all metadata files of the argo GDAC ftp site.
    ##> ## # Project : ARGO
    ##> ## # Format version : 2.0
    ##> ## # Date of update : 20171202121617
    ##> ## # FTP root number 1 : ftp://ftp.ifremer.fr/ifremer/argo/dac
    ##> ## # FTP root number 2 : ftp://usgodae.org/pub/outgoing/argo/dac
    ##> ## # GDAC node : NRL-MRY
    ##> ## file,profiler_type,institution,date_update
    ##> ## aoml/13857/13857_meta.nc,845,AO,20120521144513
    ##> ## aoml/13858/13858_meta.nc,845,AO,20130222100319
    ##> dcDebug(debug, "downloaded index file containing", length(index), "lines\n")
    ##> close(con)
    ##> header <- which(grepl("^#.*$", index))
    ##> if (length(header))
    ##>     index <- index[-header]
    ##> i <- read.csv(text=index, header=TRUE, stringsAsFactors=FALSE)
    ##> if (is.null(id)) {
    ##> } else {
    ##>     lines <- grep(id, i$file)
    ##>     if (0 == length(lines))
    ##>         stop("No id '", id, "' found in index file ", indexURL)
    ##>     lines <- lines[1]
    ##>     dcDebug(debug, "line ", lines, " of index file refers to id \"", id, "\"\n", sep="")
    ##>     ## e.g. aoml/4902912/4902912_meta.nc
    ##>     dac <- gsub("^([^/]*)/.*$", "\\1", i$file[lines])
    ##>     dcDebug(debug, "infer dac \"", dac, "\"\n", sep="")
    ##>     ## e.g. http://www.usgodae.org/ftp/outgoing/argo/dac/aoml/4902912/4902912_prof.nc
    ##>     destfile <- paste(id, "_prof.nc", sep="")
    ##>     url <- paste("http://", server, "/ftp/outgoing/argo/dac/", dac, "/", id, "/", destfile, sep="")
    ##>     rval <- dc(url=url, destdir=destdir, destfile=destfile, dryrun=dryrun, force=force, debug=debug-1)
    ##> }
    ##> dcDebug(debug, "} # dc.argoID", sep="", "\n", style="bold", unindent=1)
    ##> rval
}

#' FIXME: this function is in flux: do not use
#'
#' @template id
#' @template longitude
#' @template latitude
#' @template time
#' @template time
#' @template server
#' @template destdir
#' @template destfile
#' @template force
#' @template dryrun
#' @template debug
#' @family functions related to argo data
#' @export
dc.argoSearch <- function(id=NULL,
                          longitude=c(-180,180),
                          latitude=c(-90,90),
                          time=Sys.Date() + c(-7*86400, 0),
                          server="ftp://usgodae.org",
                          destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                          debug=getOption("dcDebug", 0))
{
    if (length(time) != 2)
        stop("time must contain two elements")
    time <- as.POSIXlt(time)
    startyear <- time[1]$year + 1900
    startmonth <- time[1]$mon + 1
    startday <- time[1]$mday + 1
    endyear <- time[2]$year + 1900
    endmonth <- time[2]$mon + 1
    endday <- time[2]$mday + 1

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
    dcDebug(debug, "} # dc.argo", sep="", "\n", unindent=1)
    rval
}

#' Get an index of available floats
#'
#' Downloads a file (or uses a cached file) and then reads it to create a
#' local `.rda` file that stores information about available float files,
#' as explained in \dQuote{Details}.
#' The download takes several of order 1 to 60 minutes, so this function
#' has an `age` argument that lets the user avoid new downloads of
#' data that were downloaded recently.
#'
#' The first step is to construct a URL for downloading, based on the
#' `url` and `file` arguments. That URL will be a string ending in `.gz`,
#' and from this the name of a local file is constructed by changing the
#' suffix to `.rda`. If that rda file is less than the age (in days)
#' specified by the `age` argument, then no downloading takes place,
#' and [dc.argoIndex()] returns the name of that rda file.
#'
#' However, if the local rda file is older than `age` days, a download
#' is started, using [curl::curl_download()] from the \CRANpkg{curl}
#' package.  The data file is downloaded to a local temporary file,
#' and then the contents of that file are analysed, with the results
#' of the analysis being stored in the local rda file.
#'
#' The resultant `.rda` file holds a list named `argoIndex`
#' that holds following elements:
#' * `ftpRoot`, the FTP root  stored in the header of the source `file`.
#' * `server`, the argument  provided here.
#' * `file`, the argument provided here.
#' * `header`, the preliminary lines in the source file that start with the `#` character.
#' * `data`, a data frame containing the items in the source file. As of
#'    files downloaded in February 2020, this has columns named
#'    `file`, `date`, `longitude`, `latitude`, `ocean`, `profiler_type`,
#'    `institution`, and `date_update`.
#'
#' Note that `paste0(argoIndex$ftpRoot, argoIndex$data$file)` will
#' form a vector of names of files that can be downloaded as local
#' Netcdf files (ending in suffix `.nc`) that can be read with
#' [oce::read.argo()] in the \CRANpkg{oce} packag, creating an
#' `argo` object.
#'
#' @template server
#' @param file character value indicating the file on the server, also
#' used as a pattern for the name of a constructed `.rda` file that
#' is placed in the `destdir` directory.
#' For the `ftp://usgodae.org/pub/outgoing/argo` server,
#' two of multiple choices for `file` are
#' `ar_index_global_prof.txt.gz`
#' and
#' `argo_bio-profile_index.txt.gz`
#' but examination of the server will reveal other possibilities
#' that might be worth exploring.
#' @template destdir
#' @param age numeric value indicating how old a downloaded file
#' must be (in days), for it to be considered out-of-date.  The
#' default, `age=2`, limits downloads to once per day, as a way
#' to avoid slowing down a workflow with a download that might take
#' a sizeable fraction of an hour. Set `age=0` to force a download
#' at any time.
#' @param quiet silence some progress indicators.  The default
#' is to show such indicators.
#' @template debug
#'
#' @return A character value holding the name of the `.rda` file,
#' which is typically loaded with [load()]; see \dQuote{Examples}.
#'
#' @examples
#'\dontrun{
#' # Download whole index
#' ai <- dc.argoIndex(destdir="~/data/argo")
#' load(ai) # defines argoIndex
#' # Plot histograms of 'date' and 'date_update'
#' par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
#' hist(argoIndex$data$date, breaks="years",
#'      main="", xlab="Time", freq=TRUE)
#' hist(argoIndex$data$date_update, breaks="years",
#'      main="", xlab="Last Update Time", freq=TRUE)
#' # Download and plot the last file in the index
#' id <- paste0(argoIndex$ftpRoot, "/", tail(argoIndex$data$file, 1))
#' f <- dc.argoById(id)
#' library(oce)
#' a <- read.oce(f)
#' summary(a)
#' par(mfrow=c(2, 2))
#' plot(a, which="map")
#' mtext(a[["time"]], cex=par("cex"))
#' plot(a, which="TS")
#' plot(a, which="temperature profile")
#' plot(a, which="salinity profile")
#'}
#'
#' @author
#' Dan Kelley
#'
#' @family functions related to argo data
#'
#' @importFrom utils read.csv tail
#' @importFrom curl curl_download
#' @export
dc.argoIndex <- function(server="ftp://usgodae.org/pub/outgoing/argo",
                         file="ar_index_global_prof.txt.gz",
                         destdir=".",
                         age=2,
                         quiet=FALSE, debug=getOption("dcDebug", 0))
{
    if (!requireNamespace("curl", quietly=TRUE))
        stop('must install.packages("curl") to download Argo data')
    ## Sample file
    ## ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/1900710/1900710_prof.nc
    ## ftp://usgodae.org/pub/outgoing/argo/dac/aoml/1900710/1900710_prof.nc
    dcDebug(debug, "dc.argoIndex(server=\"", server, "\", file=\"", file, "\"", ", destdir=\"", destdir, "\") {", sep="", "\n", style="bold", unindent=1)
    url <- paste(server, file, sep="/")
    destfile <- paste(destdir, file, sep="/")
    ## NOTE: we save an .rda file, not the .gz file, for speed of later operations
    destfileRda <- gsub(".gz$", ".rda", destfile)
    ## See if we have an .rda file that is sufficiently youthful.
    if (file.exists(destfileRda)) {
        destfileAge <- (as.integer(Sys.time()) - as.integer(file.info(destfileRda)$mtime)) / 86400 # in days
        if (destfileAge < age) {
            if (!quiet)
                cat("The local .rda file\n    ", destfileRda, "\nis not being updated from\n    ", url, "\nbecause it is only", round(destfileAge, 4), "days old.\n")
            return(destfileRda)
        }
    }
    ## We need to download data. We do that to a temporary file, because we will be saving
    ## an .rda file, not the data on the server.
    destfileTemp <- tempfile(pattern="argo", fileext=".gz")
    dcDebug(debug, format(Sys.time(), "[%H:%M:%S]"), " downloading temporary index file\n    ", destfileTemp, "\nfrom\n    ", url, "\n", sep="")
    curl::curl_download(url=url, destfile=destfileTemp, quiet=quiet, mode="wb")
    dcDebug(debug, format(Sys.time(), "[%H:%M:%S]"), " about to read header.\n", sep="")
    first <- readLines(destfileTemp, 100)
    hash <- which(grepl("^#", first))
    ftpRoot <- gsub("^[^:]*:[ ]*(.*)$", "\\1", first[which(grepl("^# FTP", first))[1]])
    header <- first[hash]
    lastHash <- tail(hash, 1)
    names <- strsplit(first[1 + lastHash], ",")[[1]]
    dcDebug(debug, format(Sys.time(), "[%H:%M:%S]"), " about to read the newly-downloaded index file.\n", sep="")
    data <- read.csv(destfileTemp, skip=2 + lastHash, col.names=names, stringsAsFactors=FALSE)
    dcDebug(debug, format(Sys.time(), "[%H:%M:%S]"), " removing temporary file '", destfileTemp, "'.\n", sep="")
    unlink(destfileTemp)
    dcDebug(debug, format(Sys.time(), "[%H:%M:%S]"), " about to decode dates.\n", sep="")
    data$date <- as.POSIXct(as.character(data$date), format="%Y%m%d%H%M%S", tz="UTC")
    data$date_update <- as.POSIXct(as.character(data$date_update), format="%Y%m%d%H%M%S",tz="UTC")
    dcDebug(debug, format(Sys.time(), "[%H:%M:%S]"), " about to save to rda file '", destfileRda, "'.\n", sep="")
    argoIndex <- list(ftpRoot=ftpRoot, server=server, file=file, header=header, data=data)
    save(argoIndex, file=destfileRda)
    dcDebug(debug, format(Sys.time(), "[%H:%M:%S]"), " dc.argoIndex() has saved results to '", destfileRda, "'.\n", sep="")
    dcDebug(debug, "} # dc.argoIndex()", sep="", "\n", style="bold", unindent=1)
    destfileRda
}
