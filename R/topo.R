## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Download and Cache a Topographic File
#'
#' Data are downloaded (from \url{https://maps.ngdc.noaa.gov/viewers/wcs-client}, by
#' default) and a string containing the full path
#' to the downloaded file is returned. If `destfile` is not supplied,
#' then the filename is constructed from the query, which means that
#' subsequent calls to [dc.topo()]  with identical parameters will
#' simply return the name of the cached file, assuming the user has not
#' deleted it in the meantime.
#'
#' The data are downloaded with [utils::download.file()], using a URL
#' devised from reverse engineering web-based queries constructed by
#' the default `server` used here. Note that the data source is
#' `"etopo1"`, which is a 1 arc-second file (ref 1,2).
#'
#' Three values are permitted for `format`,
#' each named after the
#' targets of menu items on the
#' NOAA website (as of August 2016): (1) `"aaigrid"` (for
#' the menu item "ArcGIS ASCII Grid"), which
#' yields a text file, (2) `"netcdf"` (the default,
#' for the menu item named
#' "NetCDF"), which yields a NetCDF file
#' and (3) `"gmt"` (for the menu item named
#' "GMT NetCDF"), which yields a NetCDF file in
#' another format. All of these file formats are
#' recognized by [oce::read.topo()] in the \CRANpkg{oce} package.
#' (The NOAA server has more options, and if
#' [oce::read.topo()] is extended to handle them, they will
#' also be added here.)
#'
#' @param west,east Longitudes of the western and eastern sides of the box.
#' @param south,north Latitudes of the southern and northern sides of the box.
#' @param resolution Optional grid spacing, in minutes. If not supplied,
#' a default value of 4 (corresponding to 7.4km, or 4 nautical
#' miles) is used. Note that (as of August 2016) the original data are on
#' a 1-minute grid, which limits the possibilities for `resolution`.
#' @param format ignored, as of May 30, 2020.
#' @template server
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
#' library(dc)
#' topoFile <- dc.topo(west=-64, east=-60, south=43, north=46, destdir="~/data/topo")
#' library(oce)
#' topo <- read.topo(topoFile)
#' imagep(topo, zlim=c(-400, 400), drawTriangles=TRUE)
#' data(coastlineWorldFine, package="ocedata")
#' lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
#'}
#'
#' @section Webserver history:
#' This function has been changed repeatedly over the years, to keep up
#' with changes in server URLs and query strings. Until May 2020, the
#' NOAA server was set up to supply data in netcdf and other formats,
#' but then there was a change to only supply image (tiff) format
#' or JSON format, neither of which is particularly useful for reading
#' in R.  To get around this, a scheme used by [marmap::getNOAA.bathy()] 
#' was mimicked here, to convert a TIFF object into a raster object,
#' and then the result was written into a local netcdf file.  In this way,
#' `dc.topo()` is still able to accomplish what it has before May 2020,
#' although it stopped paying attention to the `format` argument.
#'
## In case it will be of any use to users or developers, the following
## is a rough sketch of the changes in the server.
##
## \itemize{
## \item August 2016.
## \samp{http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/wcs.groovy}
##
## \item December 2016.
## \samp{http://mapserver.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz}
##
## \item June-September 2017.
## \samp{https://gis.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz}
##
## \item May 2020.
## \samp{https://gis.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz}
## }
#'
#' @seealso The work is done with [utils::download.file()].
#'
#' @references
#' 1. \samp{https://www.ngdc.noaa.gov/mgg/global/global.html}
#'
#' 2. Amante, C. and B.W. Eakins, 2009. ETOPO1 1 Arc-Minute Global Relief
#' Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum
#' NESDIS NGDC-24. National Geophysical Data Center, NOAA. doi:10.7289/V5C8276M
#' (access date: Aug 30, 2017).
#'
#' @author Dan Kelley 2020-05-30
#'
#' @examples
#'\dontrun{
#' # not run because it is slow, and downloads files
#' library(dc)
#' library(oce)
#' f <- dc.topo(-70, -50, 35, 50, resolution=2, force=TRUE)
#' topo <- read.topo(f)
#' summary(topo)
#' imagep(topo, colormap(name="gmt_globe"))}
#'
#' @family functions that download ocean-related data
#' @importFrom raster flip raster
#' @importFrom ncdf4 nc_create ncdim_def ncvar_def
#' @export
dc.topo <- function(west, east, south, north, resolution, format=NULL, server,
                          destdir=".", destfile, force=FALSE, dryrun=FALSE, # standard args
                          debug=getOption("dcDebug", 0))
{
    debug <- max(0, min(1, debug))
    dcDebug(debug, "dc.topo(west=", west, ", east=", east, ", south=", south,
            ", north=", north, ", resolution=",
            if (missing(resolution)) "(missing)" else resolution,
            ", ...) {", sep="", "\n", unindent=1)
    if (missing(server)) {
        server <- "https://gis.ngdc.noaa.gov"
        ## server <- "https://gis.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz"
        ## server <- "http://mapserver.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz"
        ## server <- "http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/wcs.groovy"
    }
    if (missing(destdir))
        destdir <- "."
    if (missing(resolution)) {
        resolution <- 4
        dcDebug(debug, "using default resolution", resolution, "minutes\n")
    }
    resolution <- resolution / 60
    if (west > 180)
        west <- west - 360
    if (east > 180)
        east <- east - 360
    if (east <= west)
        stop("must have 'west' > 'east'")
    if (north <= south)
        stop("must have 'north' > 'south'")
    wName <- paste(abs(round(west,2)), if (west <= 0) "W" else "E", sep="")
    eName <- paste(abs(round(east,2)), if (east <= 0) "W" else "E", sep="")
    sName <- paste(abs(round(south,2)), if (south <= 0) "S" else "N", sep="")
    nName <- paste(abs(round(north,2)), if (north <= 0) "S" else "N", sep="")
    resolutionName <- paste(resolution, "min", sep="")
    if (missing(destfile))
        destfile <- paste0(paste("topo", wName, eName, sName, nName, resolutionName, sep="_"), ".nc")
    destfile <- paste0(destdir, "/", destfile)
    if (!force && file.exists(paste0(destdir, "/", destfile))) {
        dcDebug(debug, "using existing file \"", destfile, "\"\n", sep="")
        dcDebug(debug, "} # dc.topo\n", unindent=1, sep="")
        return(destfile)
    }
    nlon <- (east - west) * 60 / resolution
    nlat <- (north - south) * 60 / resolution
    url <- paste0(server, "/arcgis/rest/services/DEM_mosaics/ETOPO1_bedrock/ImageServer/exportImage",
                  "?bbox=", west, ",", south, ",", east, ",", north,
                  "&bboxSR=4326",
                  "&size=", nlon, ",", nlat,
                  "&imageSR=4326",
                  "&format=tiff",
                  "&pixelType=S16",
                  "&interpolation=+RSP_NearestNeighbor",
                  "&compression=LZW",
                  "&f=image")
    dcDebug(debug, "querying \"", url, "\"\n", sep="")
    r <- raster::raster(x=url)
    dcDebug(debug, "converting data\n", sep="")
    longitude <- seq(r@extent@xmin, r@extent@xmax, length.out=r@ncols)
    latitude <- seq(r@extent@ymin, r@extent@ymax, length.out=r@nrows)
    z <- t(raster::as.matrix(raster::flip(r, direction="y")))
    dcDebug(debug, "saving to \"", destfile, "\"\n", sep="")
    ## create netcdf file
    ## dimensions
    #side <- ncdf4::ncdim_def("side", units="", vals=2.0)
    fillvalue <- 1e32
    lonDim <- ncdf4::ncdim_def("lon", "degrees_east", as.double(longitude))
    #lonVar <- ncdf4::ncvar_def("lon", "degrees_east", lonDim, fillValue, "lon", prec="double")
    latDim <- ncdf4::ncdim_def("lat", "degrees_north", as.double(latitude))
    #latVar <- ncdf4::ncvar_def("lat", "degrees_north", latDim, fillValue, "lat", prec="double")
    Band1 <- ncdf4::ncvar_def("Band1", "m", list(lonDim, latDim), fillvalue, "elevation m", prec="double")
    nc <- ncdf4::nc_create(destfile, list(Band1))
    ncdf4::ncvar_put(nc, "Band1", z)
    ##x_range <- ncdf4::ncvar_put(nc, "x_range", "meters", as.double(range(longitude)), start=1, count=2)
    ##y_range <- ncdf4::ncvar_put(nc, "y_range", "meters", as.double(range(latitude)), start=1, count=2)
    ncdf4::nc_close(nc)
    dcDebug(debug, "} # dc.topo", sep="", "\n", unindent=1)
    destfile
}

