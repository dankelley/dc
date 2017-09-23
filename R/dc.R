#' dc: A Package to download and cache data files from the web
#' @docType package
#' @name dc
NULL


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


