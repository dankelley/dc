#' @param debug an integer specifying whether debugging information is
#' to be printed during processing. The printing is done by
#' a call to [dcDebug()].  Setting `debug=0`
#' turns off this form of debugging, while higher values yield
#' more information. If one `dc` function calls another, it
#' passes the value of `debug` but decreased by 1, which means
#' that the value of `debug` controls not just the breadth
#' of debugging, but also the depth.

