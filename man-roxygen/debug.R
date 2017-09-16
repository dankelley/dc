#' @param debug an integer specifying whether debugging information is
#' to be printed during processing. The printing is done by
#' a call to \code{\link{dacDebug}}.  Setting \code{debug=0}
#' turns off this form of debugging, while higher values yield
#' more information. If one \code{dac} function calls another, it
#' passes the value of \code{debug} but decreased by 1, which means
#' that the value of \code{debug} controls not just the breadth
#' of debugging, but also the depth.
