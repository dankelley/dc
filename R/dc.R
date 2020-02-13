#' dc: A Package to download and cache data files from the web
#' @docType package
#' @name dc
NULL

#' Download and Cache a Dataset
#'
#' General function for downloading and caching a dataset. This is called
#' by e.g. [dc.argo()] and the other functions in the `dc`
#' package.
#'
#' @template url
#' @template destdir
#' @template destfile
#' @template mode
#' @template quiet
#' @template force
#' @template dryrun
#' @template ntrials
#' @template debug
#'
#' @return String indicating the full pathname to the downloaded file.
#' @importFrom utils unzip
#' @importFrom curl curl_download
#' @export
dc <- function(url=NULL, destdir=".", destfile=NULL, mode="wb",
               quiet=FALSE, force=FALSE, dryrun=FALSE, ntrials=3,
               debug=getOption("dcDebug", 0))
{
    if (missing(url))
        stop("missing url")
    if (missing(destdir))
        stop("missing destdir")
    ntrials <- as.integer(ntrials)
    if (ntrials < 1)
        ntrials <- 1
    dcDebug(debug, "dc(",
            "url=c('", paste(url, collapse="', '"), "'), ",
            "destdir='", destdir, "', ",
            "destfile=c('", paste(destfile, collapse="', '"), "'), ",
            "debug=", debug,
            ") {", sep="", "\n", style="bold", unindent=1)
    destination <- paste(destdir, destfile, sep="/")
    dcDebug(debug, "destination: ", destination, "\n")
    n <- length(url)
    if (length(destfile) != n)
        stop("length(url) must equal length(destfile)")
    if (dryrun) {
        for (i in 1:n) {
            cat("curl::curl_download(\"", url[i], "\", \"", destination[i], "\")\n", sep="")
        }
    } else {
        if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
            dcDebug(debug, "Skipping \"", destfile, "\" because it is already in \"", destdir, "\n", sep="")
        } else {
            for (i in 1:n) {
                success <- FALSE
                for (trial in seq_len(ntrials)) {
                    t <- try(curl::curl_download(url=url, destfile=destfile, quiet=quiet, mode=mode))
                    if (!inherits(t, "try-error")) {
                        success <- TRUE
                        break
                    }
                }
                if (!success)
                    stop("failed to download, after ", ntrials, " trial")
                if (1 == length(grep(".zip$", destfile[i]))) {
                    destinationClean <- gsub(".zip$", "", destination[i])
                    unzip(destination[i], exdir=destinationClean)
                    destination[i] <- destinationClean
                    dcDebug(debug, "Downloaded and unzipped into '", destinationClean, "'\n", sep="")
                } else {
                    dcDebug(debug, "Downloaded file stored as '", destination[i], "'\n", sep="")
                }
            }
        }
    }
    dcDebug(debug, "} # dc()", sep="", "\n", style="bold", unindent=1)
    destination
}

#' Possibly print a debugging message
#'
#' [dcDebug()] prints a message, if its first argument exceeds 0.
#' Many `dc` functions decrease the `debug` level by 1 when they call other
#' functions, so the effect is a nesting, with more space for deeper function
#' level.
#'
#' @param debug an integer, less than or equal to zero for no message, and
#' greater than zero for increasing levels of debugging.  Values greater than 4
#' are treated like 4.
#' @param ... one or several character or other values to be printed, analogous
#' to the `...` argument of [cat()].
#' @param style either a string or a function. If a string,
#' it must be `"plain"` (the default) for plain text,
#' `"bold"`, `"italic"`, `"red"`, `"green"` or `"blue"` (with
#' obvious meanings).
#' If `style` is a function, it must prepend and postpend the text
#' with control codes, as in the cyan-coloured example; note that
#' \CRANpkg{crayon} provides many functions that work well for `style`.
#' @param unindent Number of levels to un-indent, e.g. it is common to set
#' this to `-1` for messages about entering or exiting a function.
#'
#' @examples
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
#'
#' @author Dan Kelley
#'
#' @importFrom utils flush.console
#' @export
dcDebug <- function(debug=0, ..., style="plain", unindent=0)
{
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    if (debug > 0) {
        n <- 5 - debug - unindent
        if (is.character(style) && style == "plain") {
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
        } else if (is.character(style) && style == "bold") {
            cat("\033[1m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "italic") {
            cat("\033[3m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "red") {
            cat("\033[31m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "green") {
            cat("\033[32m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.character(style) && style == "blue") {
            cat("\033[34m")
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
            cat("\033[0m")
        } else if (is.function(style)) {
            if (n > 0)
                cat(style(paste(rep("  ", n), collapse="")))
            cat(style(...))
        } else { # fallback
            if (n > 0)
                cat(paste(rep("  ", n), collapse=""))
            cat(...)
        }
        flush.console()
    }
    invisible()
}

