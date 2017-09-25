#' Download and Cache a World Ocean Atlas File
#'
#' @description
#'
#' @template intro
#'
#' @details
#'
#' The data are downloaded with \code{\link[utils]{download.file}}
#' pointed to links inferred by tracing a NOAA website [ref 1] through a series
#' of links down to a THREDDS server and finally to an HTTPServer.  At the
#' moment, there is no choice on which version of the database is used: it is
#' WOA13 for all variables except salinity and temperature, and in these latter
#' two, it is WOA13V2, which is said to be an improvement over the initial
#' release [ref 2].
#'
#' @param database String indicating the name of the database. This must be
#' \code{"woa13"} in this version, but other databases can be added if users
#' need them.
#'
#' @param version String indicating the version of the atlas. This is
#' ignored in this version, but a future version could allow specification
#' of either the original or the version-2 data.
#'
#' @param time String indicating the time. This is ignored in this version,
#' but a future version could allow division into months for some fields
#' and decades for others.
#'
#' @param resolution Number indicating the resolution, in degrees
#' of longitude and latitude. The permitted values are 0.25, 1
#' and 5. The downloaded files are of typical size 634 Mb, 177 Mb
#' and 4.1 Mb, respectively. The 1-deg resolution is a good
#' choice for global-scale applications.
#'
#' @param field String indicating the variable. This must be one of
#' the following: \code{"temperature"}, \code{"temperature"},
#' \code{"salinity"}, \code{"density"}, \code{"oxygen"},
#' \code{"phosphate"}, \code{"nitrate"} or \code{"silicate"}.
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
#' ## Example 1: download and plot 1-deg grid (177 Mb file)
#' ## Download Levitus 2013 (Version 2) temperature on a 1-degree grid,
#' ## in netcdf format, then read the data, and finally, plot sea-surface
#' ## temperature using oce::imagep() to get an image with a palette.
#' T1f <- dc.woa(field="temperature")
#' library(ncdf4)
#' T1nc <- nc_open(T1f) # or nc_open("woa13_decav_t00_01v2.nc")
#' lon1 <- ncvar_get(T1nc, "lon")
#' lat1 <- ncvar_get(T1nc, "lat")
#' T1 <- ncvar_get(T1nc, "t_an")
#' library(oce)
#' imagep(lon1, lat1, T1[,,1])
#'
#' ## Example 2: download and plot 5-deg grid (4.1 Mb file)
#' ## Note that the variable 't_an' used above must be switched
#' ## to 't_mn' here. Also, a point of confusion: the metadata
#' ## in the downloaded file say that t_an is 4D, with the
#' ## extra dimension being time, but the time vector in the file
#' ## is of unit length, so ncdf4 appraently demotes it to 3D.
#' T5f <- dc.woa(field="temperature", resolution=5)
#' T5nc <- nc_open(T5f)
#' lon5 <- ncvar_get(T5nc, "lon")
#' lat5 <- ncvar_get(T5nc, "lat")
#' T5 <- ncvar_get(T5nc, "t_mn")
#' library(oce)
#' imagep(lon5, lat5, T5[,,1])
#'
#' ## Example 3: download all fields on default 1-deg grid
#' for (field in c("temperature", "salinity", "oxygen", "silicate",
#'                 "phosphate", "nitrate", "density"))
#' 	dc.woa(field=field)
#'}
#'
#' @seealso The work is done with \code{\link[utils]{download.file}}.
#'
#' @references
#' 1. \url{https://www.nodc.noaa.gov/OC5/woa13/woa13data.html}
#'
#' 2. \url{https://www.nodc.noaa.gov/OC5/woa13}
#'
#' @author Dan Kelley (2017-09-22)
dc.woa <- function(database="woa13", version=NULL, time=NULL, resolution=1, field="temperature",
                         destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                         debug=getOption("dcDebug", 0))
{
    if (database != "woa13")
        stop("database must equal \"woa13\"")
    if (!is.null(time))
        stop("time must be NULL")
    ## set up resolution nicknames (used in the query) and also ensure
    ## resolution is in permitted list
    if (0.01 > abs(resolution - 0.25)) {
        rn1 <- "0.25"
        rn2 <- "04"
    } else if (0.01 > abs(resolution - 1)) {
        rn1 <- "1.00"
        rn2 <- "01"
    } else if (0.01 > abs(resolution - 5)) {
        rn1 <- "5deg"
        rn2 <- "5d"
    } else {
        stop("resolution must equal 0.25, 1, or 5, but it is ", resolution)
    }
    ## Expand field, if abbreviated.
    fieldAllowed <- c("temperature", "salinity", "density",
                      "oxygen", "phosphate", "nitrate", "silicate")
    fnumber <- pmatch(field, fieldAllowed)
    if (is.na(fnumber))
        stop("field '", field, "' is not understood; use one of: ", paste(fieldAllowed, collapse=" "))
    field <- fieldAllowed[fnumber]

    URLv2 <- "https://data.nodc.noaa.gov/thredds/fileServer/woa/WOA13/DATAv2"
    URL <- "https://data.nodc.noaa.gov/thredds/fileServer/woa/WOA13/DATA"
    ## https://data.nodc.noaa.gov/thredds/fileServer/woa/WOA13/DATAv2/temperature/netcdf/decav/0.25/woa13_decav_t00_04v2.nc
    ## https://data.nodc.noaa.gov/thredds/fileServer/woa/WOA13/DATAv2/temperature/netcdf/decav/1.00/woa13_decav_t00_01v2.nc
    ## https://data.nodc.noaa.gov/thredds/fileServer/woa/WOA13/DATAv2/temperature/netcdf/decav/5deg/woa13_decav_t00_5dv2.nc
    if (field == "temperature" || field == "salinity") {
        fn <- if (field == "temperature") "t" else "s"
        url <- paste(URLv2, "/", field, "/netcdf/decav/", rn1, "/woa13_decav_", fn, "00_", rn2, "v2.nc", sep="")
        destfile <- paste("woa13_decav_", fn, "00_", rn2, "v2.nc", sep="")
    } else if (field == "oxygen") {
        url <- paste(URL, "/", field, "/netcdf/all/", rn1, "/woa13_all_o00_", rn2, ".nc", sep="")
        destfile <- paste("woa13_all_o00_", rn2, ".nc", sep="")
    } else if (field == "silicate") {
        url <- paste(URL, "/", field, "/netcdf/all/", rn1, "/woa13_all_i00_", rn2, ".nc", sep="")
        destfile <- paste("woa13_all_i00_", rn2, ".nc", sep="")
    } else if (field == "phosphate") {
        url <- paste(URL, "/", field, "/netcdf/all/", rn1, "/woa13_all_p00_", rn2, ".nc", sep="")
        destfile <- paste("woa13_all_p00_", rn2, ".nc", sep="")
    } else if (field == "nitrate") {
        url <- paste(URL, "/", field, "/netcdf/all/", rn1, "/woa13_all_n00_", rn2, ".nc", sep="")
        destfile <- paste("woa13_all_n00_", rn2, ".nc", sep="")
    } else if (field == "density") {
        ## https://data.nodc.noaa.gov/thredds/fileServer/woa/WOA13/DATAv2/density/netcdf/decav/1.00/woa13_decav_I00_01.nc
        url <- paste(URLv2, "/", field, "/netcdf/decav/", rn1, "/woa13_decav_I00_", rn2, ".nc", sep="")
        destfile <- paste("woa13_decav_I00_", rn2, ".nc", sep="")
    } else {
        stop("unknown field, \"", field, "\"") # we cannot reach this point, but keep it in case of code changes
    }
    ##
    ## Below is standard code that should be used at the end of every dc.x() function.
    destination <- paste(destdir, destfile, sep="/")
    dcDebug(debug, "url:", url, "\n")
    if (dryrun) {
        cat(url, "\n")
    } else {
        if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
            dcDebug(debug, "Not downloading \"", destfile, "\" because it is already present in the \"", destdir, "\" directory\n", sep="")
        } else {
            download.file(url, destination)
            dcDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
        }
    }
    destination
}

