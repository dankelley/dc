#' Download and Cache a World Ocean Atlas File
#'
#' @description
#'
#' @template intro
#'
#' @details
#'
#' The data are downloaded with \code{\link[utils]{download.file}}
#' pointed to links inferred by tracing a NOAA website [1] through a series
#' of links down to a THREDDS server and finally to an HTTPServer.  At the
#' moment, there is no choice on which version of the database is used: it is
#' WOA13 for all variables except salinity and temperature, and in these latter
#' two, it is WOA13V2, which is said to be an improvement over the initial
#' release [2]. Also, there is no choice of resolution; the present
#' version only downloads data on a 1-degree grid.
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
#' @param resolution Number indicating the resolution. This must equal
#' \code{1} in this version, but a future version may allow other resolutions.
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
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' ## Download Levitus 2013 (Version 2) temperature on a 1-degree grid,
#' ## in netcdf format, then read the data, and finally, plot sea-surface
#' ## temperature using oce::imagep() to get an image with a palette.
#' Tnc <- download.woa(field="temperature")
#' library(ncdf4)
#' Tf <- nc_open(Tnc) # or nc_open("woa13_decav_t00_01v2.nc")
#' longitude <- ncvar_get(Tf, "lon")
#' latitude <- ncvar_get(Tf, "lat")
#' T <- ncvar_get(Tf, "t_an")
#' library(oce)
#' imagep(longitude, latitude, T[,,1])
#'}
#'
#' @seealso The work is done with \code{\link[utils]{download.file}}.
#'
#' @references
#' 1. \url{https://www.nodc.noaa.gov/OC5/woa13/woa13data.html}
#'
#' 2. \url{https://www.nodc.noaa.gov/OC5/woa13}
download.woa <- function(database="woa13", version=NULL, time=NULL, resolution=1, field="temperature",
                         destdir=".", destfile, force=FALSE,
                         debug=getOption("dacDebug", 0))
{
    if (database != "woa13")
        stop("database must equal \"woa13\"")
    if (!is.null(time))
        stop("time must be NULL")
    if (resolution != 1)
        stop("resolution must equal 1")
    fieldAllowed <- c("temperature", "temperature", "salinity", "density",
                      "oxygen", "phosphate", "nitrate", "silicate")
    if (!(field %in% fieldAllowed))
        stop("field must be one of: ", paste(fieldAllowed, collapse=" "))
    URLv2 <- "https://data.nodc.noaa.gov/thredds/fileServer/woa/WOA13/DATAv2"
    URL <- "https://data.nodc.noaa.gov/thredds/fileServer/woa/WOA13/DATA"
    if (field == "temperature") {
        url <- paste(URLv2, "temperature/netcdf/decav/1.00/woa13_decav_t00_01v2.nc", sep="/")
        destfile <- "woa13_decav_t00_01v2.nc"
    } else if (field == "salinity") {
        url <- paste(URLv2, "salinity/netcdf/decav/1.00/woa13_decav_s00_01v2.nc", sep="/")
        destfile <- "woa13_decav_s00_01v2.nc"
    } else if (field == "oxygen") {
        url <- paste(URL, "oxygen/netcdf/all/1.00/woa13_all_o00_01.nc", sep="/")
        destfile <- "woa13_all_o00_01.nc"
    } else if (field == "silicate") {
        url <- paste(URL, "silicate/netcdf/all/1.00/woa13_all_i00_01.nc", sep="/")
        destfile <- "woa13_all_i00_01.nc"
    } else if (field == "phosphate") {
        url <- paste(URL, "phosphate/netcdf/all/1.00/woa13_all_p00_01.nc", sep="/")
        destfile <- "woa13_all_p00_01.nc"
    } else if (field == "nitrate") {
        url <- paste(URL, "nitrate/netcdf/all/1.00/woa13_all_n00_01.nc", sep="/")
        destfile <- "woa13_all_n00_01.nc"
    } else if (field == "density") {
        url <- paste(url, "/density/netcdf/decav/1.00/woa13_decav_I00_01.nc", sep="/")
        destfile <- "woa13_decav_I00_01.nc"
    } else {
        stop("unknown field, \"", field, "\"")
    }
    destination <- paste(destdir, destfile, sep="/")
    dacDebug(debug, "url:", url, "\n")
    if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
        dacDebug(debug, "Not downloading \"", destfile, "\" because it is already present in the \"", destdir, "\" directory\n", sep="")
    } else {
        download.file(url, destination)
        dacDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
    }
    destination
}


