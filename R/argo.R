#' Convert string date to POSIXct time
#'
#' @param t character value giving the time in yyyymmddHHMMSS format,
#' as illustrated in \dQuote{Examples}.
#' @param tz character value holding the timezone.  It seems unlikely
#' that a value other than `"UTC"` will be appropriate, so that
#' is the default.
#'
#' @param tz character string
#' @return POSIXct time
#'
#' @examples
#' decodeTextDate("19970729200300") ## 1997 07 29 20 03 00
#'
#' @export
decodeTextDate <- function(t, tz="UTC")
{
    t <- as.character(t)
    ISOdatetime(substr(t,1,4), substr(t,5,6), substr(t,7,8),
                substr(t,9,10), substr(t,11,12), substr(t,13,14))
}

#' Download and Cache an Argo Dataset
#'
#' Download and cache data from an argo profiling float.
#'
#' [dc.argoID()] downloads all data for the float with
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
#' 1. \url{http://.usgodae.org}
#'
#' 2. \url{http://www.usgodae.org/cgi-bin/argo_select.pl}
#'
#' @author Dan Kelley
#'
#' @family functions related to argo data
#' @export
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
        rval <- dc.argoID(id=id,
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

#' FIXME(dk): I'll document this later, if I decide it's still useful.
#' @template id
#' @template server
#' @template destdir
#' @template destfile
#' @template force
#' @template dryrun
#' @template debug
#' @family functions related to argo data
#' @export
dc.argoID <- function(id=NULL,
                      ##server="www.usgodae.org",
                      server="ftp://usgodae.org",
                      destdir=".", destfile, force=FALSE, dryrun=FALSE,
                      debug=getOption("dcDebug", 0))
{
    dcDebug(debug, "dc.argoID(id=\"", id, "\", ...) {", sep="", "\n", style="bold", unindent=1)
    ##> http://www.usgodae.org/ftp/outgoing/argo/ar_index_global_meta.txt.gz
    ##> indexURL <- paste("http://", server, "/ftp/outgoing/argo/ar_index_global_meta.txt.gz", sep="")
    ##
    ## ftp://usgodae.org/pub/outgoing/argo/ar_index_global_meta.txt
    indexURL <- paste0(server, "/pub/outgoing/argo/ar_index_global_meta.txt.gz")
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
    header <- which(grepl("^#.*$", index))
    if (length(header))
        index <- index[-header]
    i <- read.csv(text=index, header=TRUE, stringsAsFactors=FALSE)
    if (is.null(id)) {
    } else {
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
    }
    dcDebug(debug, "} # dc.argoID", sep="", "\n", style="bold", unindent=1)
    rval
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
#' data frame that stores information about available float files.
#'
#' The first step is to construct a URL that will yield an index file to be stored in
#' a file named by the `file` argument, stored within the directory named in
#' the `destdir` argument. If that destination file was downloaded less than
#' `age` days ago, it is reused. Otherwise, [curl::curl_download()]
#' from the \CRANpkg{curl} package is used to download the file.
#'
#' The next step is to read that file and infers the relevant data, by ignoring
#' all leading lines that start with the `#` character, and determining
#' column names from the first line after this sequence.
#' The data are read into a data frame using [read.csv()].
#'
#' @template server
#' @param file character value indicating the file on the server, also
#' used for the downloaded file, which is placed in the `destdir` directory.
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
#' @return a data frame that has columns named
#' `"file"`, `"date"`, `"latitude"`, `"longitude"`,
#' `"ocean"`, `"profiler_type"`, `"institution"` and `"date_update"`.
#' The dates are stored in the file in numeric (yyyymmddhhmmss) format, but
#' the return value represents these in POSIXct format, after conversion
#' with [decodeTextDate()].
#'
#' @examples
#' # The download takes several of order 1 to 10 minutes.
#'\dontrun{
#' # Download whole index
#' ai <- dc.argoIndex()
#' # Plot histograms of 'date' and 'date_update'
#' par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
#' hist(ai$date, breaks="years",
#'      main="", xlab="Time", freq=TRUE)
#' hist(ai$date_update, breaks="years",
#'      main="", xlab="Last Update Time", freq=TRUE)
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
                         quiet=FALSE,
                         debug=getOption("dcDebug", 0))
{
    if (!requireNamespace("curl", quietly=TRUE))
        stop('must install.packages("curl") to download Argo data')
    ## Sample file
    ## ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/1900710/1900710_prof.nc
    ## ftp://usgodae.org/pub/outgoing/argo/dac/aoml/1900710/1900710_prof.nc
    dcDebug(debug, "dc.argoIndex(server=\"", server, "\", file=\"", file, "\"", ", destdir=\"", destdir, "\") {", sep="", "\n", style="bold", unindent=1)
    url <- paste(server, file, sep="/")
    destfile <- paste(destdir, file, sep="/")
    if (file.exists(destfile)) {
        destfileAge <- (as.integer(Sys.time()) - as.integer(file.info(destfile)$mtime)) / 86400 # in days
        if (destfileAge > age) {
            if (!quiet)
                cat("Downloading local file\n    ", destfile, "\nfrom\n    ", url, "\nbecause it is more than", round(age, 4), " days old\n")
            curl::curl_download(url=url, destfile=destfile, quiet=quiet)
            ##curl_download(url=url, destfile=destfile, quiet=quiet)
        } else {
            if (!quiet)
                cat("The local file\n    ", destfile, "\nis not being downloaded from\n    ", url, "\nbecause it is only", round(destfileAge, 4), "days old.\n")
        }
    } else {
        if (!quiet)
            cat("Downloading local file\n    ", destfile, "\nfrom\n    ", url, "\n")
        curl::curl_download(url=url, destfile=destfile, quiet=quiet)
        ##curl_download(url=url, destfile=destfile, quiet=quiet)
    }
    if (!quiet)
        cat("Reading header.\n")
    first <- readLines(destfile, 100)
    hash <- which(grepl("^#", first))
    lastHash <- tail(hash, 1)
    names <- strsplit(first[1 + lastHash], ",")[[1]]
    if (!quiet)
        cat("Reading local index file.\n")
    res <- read.csv(destfile, skip=2 + lastHash, col.names=names, stringsAsFactors=FALSE)
    if (!quiet)
        cat("Decoding dates.\n")
    res$date <- decodeTextDate(res$date)
    res$date_update <- decodeTextDate(res$date_update)
    dcDebug(debug, "} # dc.argoIndex()", sep="", "\n", style="bold", unindent=1)
    res
}
