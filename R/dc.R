#' dc: A Package to download and cache data files from the web
#' @docType package
#' @name dc
NULL

#' Download and Cache a Dataset
#'
#' @description
#' General function for downloading and caching a dataset. This is called
#' by e.g. \code{\link{dc.argo}} and the other functions in the \code{dc}
#' package.
#'
#' @template intro
#' @param url String containing the name of the URL from which the data file
#' is to be downloaded.
#' @template filenames
#' @template debug
#'
#' @return String indicating the full pathname to the downloaded file.
dc.dc <- function(url=NULL, destdir=".", destfile=NULL, force=FALSE, dryrun=FALSE, debug=getOption("dcDebug", 0))
{
    if (missing(url))
        stop("missing url")
    if (missing(destdir))
        stop("missing destdir")
    dcDebug(debug, "dc(",
            "url=c('", paste(url, collapse="', '"), "'), ",
            "destdir='", destdir, "', ",
            "destfile=c('", paste(destfile, collapse="', '"), "'), ",
            "debug=", debug,
            ") {", sep="", "\n", unindent=1)
    destination <- paste(destdir, destfile, sep="/")
    dcDebug(debug, "destination: ", destination, "\n")
    n <- length(url)
    if (length(destfile) != n)
        stop("length(url) must equal length(destfile)")
    if (dryrun) {
        for (i in 1:n) {
            cat("'", url[i], "' -> '", destination[i], "'\n", sep="")
        }
    } else {
        if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
            dcDebug(debug, "Skipping \"", destfile, "\" because it is already in \"", destdir, "\n", sep="")
        } else {
            for (i in 1:n) {
                download.file(url[i], destination[i], mode="wb")
                dcDebug(debug, "Downloaded file stored as '", destination[i], "'\n", sep="")
            }
        }
    }
    dcDebug(debug, "} # dc()", sep="", "\n", unindent=1)
    destination
}

#' Possibly print a debugging message
#'
#' \code{dcDebug} prints a message, if its first argument exceeds 0.
#' Many \code{dc} functions decrease the \code{debug} level by 1 when they call other
#' functions, so the effect is a nesting, with more space for deeper function
#' level. Messages are indented according to the value of the \code{debug+indent}.
#'
#' @param debug an integer, less than or equal to zero for no message, and
#' greater than zero for increasing levels of debugging.  Values greater than 4
#' are treated like 4.
#' @param \dots items to be supplied to \code{\link{cat}}, which does the
#' printing.  A trailing newline must be given to prevent subsequent
#' messages from appearing on the same line.
#' @param unindent Number of levels to un-indent, e.g. it is common to set
#' this to \code{-1} for messages about entering or exiting a function.
#' @author Dan Kelley
#' @examples
#'
#' foo <- function(debug=1)
#' {
#'    dcDebug(debug, "in foo, about to call bar()\n")
#'    bar(debug=debug-1)
#'    dcDebug(debug, "in foo, after calling bar()\n")
#' }
#' bar <- function(debug=1)
#' {
#'    dcDebug(debug, "in bar()\n")
#' }
#' foo(debug=2)
#' foo(debug=1)
#' foo(debug=0)
dcDebug <- function(debug=0, ..., unindent=0)
{
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- 5 - debug - unindent
        if (n > 0)
            cat(paste(rep("  ", n), collapse=""))
        cat(...)
    }
    flush.console()
    invisible()
}


