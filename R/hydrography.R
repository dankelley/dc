#' Download and Cache a Hydrographic Dataset
#'
#' @description
#'
#' @template intro
#'
#' @details
#' \code{dc.hydography} downloads hydographic datasets from the CCHDO archive,
#' (or mirrors that use the exact same URL structure, apart from the 
#' server name) based on a specified "expocode" that specifies the
#' expedition or cruise. This is convenient, because expocodes are how
#' cruises are referred to in the literature. See \dQuote{Limitiations} for
#' a comments on the brittleness of \code{dc.hydrography}.
#'
#' @section Limitations:
#' Unfortunately, \url{cchdo.ucsd.edu} does not provide data organized
#' into URLs that can be inferred from the expocode alone, and so the
#' procedure used here is to scan the HTML for a summary URL that
#' \emph{can} be inferred, and then to discover links from the HTML
#' text. This is brittle to changes in the formatting used by CCHDO, which
#' means that changes may need to be made to \code{dc.hydography} whenever
#' CCHDO changes their website. Another consequence is that a web connection
#' is required even if the desired file has already been downloaded.
#'
#' @param expocode String indicating the cruise (or 'expedition') code.
#'
#' @param type String indicating data type, either \code{"bottle"} (the
#' default) or \code{"ctd"}.
#'
#' @param format String indicating data format, either \code{"exchange"}
#' (the default) or \code{"whp_netcdf"}.
#'
#' @param unzip Logical value, \code{TRUE} by default, indicating
#' whether to unzip any ownloaded files that are in .zip form.
#' This will construct a new subdirectory to hold the unzipped data,
#' and sets the return value to the name of that new subdirectory,
#' instead of the name of the zipfile. The zipfile is retained.
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
#' ## 1. Bottle stations for a Southern Ocean cruise
#' bottle <- dc.hydrography("09AR20041223")
#' plot(read.section(bottle), which="map")
#' ##
#' ## 2. CTD data from same cruise, subsetted to
#' ##    a section between Antarctica and Australia
#' ctd <- dc.hydrography("09AR20041223", type="ctd")
#' secCTD <- read.section(directory=ctd)
#' transectCTD <- subset(secCTD, 100 < longitude & longitude < 120)
#' plot(transectCTD)
#'}
#'
#' @seealso The work is done with \code{\link[utils]{download.file}}.
#'
#' @references
#' 1. \url{https://cchdo.ucsd.edu}
#'
#' @author Dan Kelley (2017-11-25)
dc.hydrography <- function(expocode, type="bottle", format="exchange", server, unzip=TRUE,
                         destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                         debug=getOption("dcDebug", 0))
{
    dcDebug(debug, "dc.hydrography(...) {", sep="", "\n", unindent=1)
    if (missing(expocode))
        stop("expocode must be provided. Consult cchdo.ucsd.edu to learn more")
    if (type != "bottle" && type != "ctd")
        stop("type must be either \"bottle\" or \"ctd\", not \"", type, "\" as given")
    if (format != "exchange" && format != "whp_netcdf")
        stop("format must be either \"exchange\" or \"whp_netcdf\", not \"", type, "\" as given")
    if (missing(server))
        server <- "cchdo.ucsd.edu"
    ## base example
    ##   https://cchdo.ucsd.edu/cruise/09AR20041223
    ## the above has the following links. But I cannot see how
    ## to guess the portion after "data" so I think we are going
    ## to have to parse pages like the above. Uggg.
    ##
    ## bottle examples
    ##   https://cchdo.ucsd.edu/data/9582/09AR20041223_hy1.csv
    ##   https://cchdo.ucsd.edu/data/3566/09AR20041223_nc_hyd.zip
    ##
    ## CTD examples
    ##    https://cchdo.ucsd.edu/data/2684/i09s_09AR20041223_ct1.zip
    ##    https://cchdo.ucsd.edu/data/2463/i09s_09AR20041223_nc_ctd.zip
    baseURL <- paste("https://", server, "/cruise/", expocode, sep="")
    con <- url(baseURL)
    ## Turn off warnings before reading to avoid warnings about a missing
    ## newline at the end.
    owarn <- options("warn")$warn
    options(warn=-1)
    baseHTML <- readLines(con)
    options(warn=owarn)
    close(con)
    baseLook <- grep(paste("<li><h([1-9])>", type, "</h\\1>", sep=""), baseHTML)
    examine <- baseHTML[baseLook[1] + 0:10]
    ##print(examine)
    focus <- grep(format, examine)
    if (0 == length(focus))
        stop("cannot determine a URL for \"", format, "\" from code at ", baseURL)
    ##print(examine[focus])
    ## "      <li><b>exchange:</b> <a href=\"/data/9582/09AR20041223_hy1.csv\">09AR20041223_hy1.csv</a> (810.2 kB)</li>"
    urlSuffix <- gsub('^.*<a href="(.*)".*$', "\\1", examine[focus])
    destfile <- gsub("/data/.*/", "", urlSuffix)
    url <- paste("https://", server, "/", urlSuffix, sep="")
    ##    https://cchdo.ucsd.edu/data/2463/i09s_09AR20041223_nc_ctd.zip
    ##
    ## Below is standard code that should be used at the end of every dc.x() function.
    destination <- paste(destdir, destfile, sep="/")
    dcDebug(debug, "url:", url, "\n")
    if (dryrun) {
        cat(url, "\n")
    } else {
        destfileClean <- if (length(grep(".zip$", destfile))) gsub(".zip$", "", destfile) else destfile
        if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfileClean, "$", sep="")))) {
            dcDebug(debug, "Not downloading \"", destfileClean, "\" because it is already present in the \"", destdir, "\" directory\n", sep="")
        } else {
            download.file(url, destination)
            dcDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
            if (unzip && 1 == length(grep(".zip$", destination))) {
                exdir <-  gsub(".zip$", "", destination)
                unzip(destination, exdir=exdir)
                destination <- exdir
                dcDebug(debug, "unzipped data into '", destination, "'\n", sep="")
            }
        }
    }
    dcDebug(debug, "} # dc.hydrography", sep="", "\n", unindent=1)
    destination
}

