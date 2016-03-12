#' get fitted values for an ElementRatio object
#'
#' get fitted values for an ElementRatio object
#' @param object the object
#' @param depth the depth to get the fitted values for
#' @output a data frame with the estimate and sd for each depth
#' @export
fitted.ElementRatio <- function(object, depth=object$data$depth) {
  do.call("getmsd", c(list(x=depth), as.list(object$par)))
}

#' plot an ElementRatio object
#'
#' plot an ElementRatio object
#' @param x object
#' @param sd how many sd out to put the lines
#' @param main title of plot
#' @param to put on log scale or not
#' @param ylab y label
#' @param ... additional parameters sent to plot
#' @return NULL
#' @export
plot.ElementRatio <- function(x, sd=1, main=paste0(x$mobile, "/", x$immobile), log=FALSE,
                      ylab=if(log) "logratio" else "ratio", ...) {
  depth <- x$data$depth
  xx <- c(0, seq(x$output[["depth1"]], x$output[["depth2"]], len=if(log) 50 else 2), max(depth))
  x.fit <- fitted.ElementRatio(x, depth=xx)
  tolog <- if(log) identity else exp
  plot(depth, tolog(x$data$logratio), main=main, ylab=ylab, ...)
  lines(xx, tolog(x.fit$estimate))
  lines(xx, tolog(x.fit$estimate + sd*x.fit$sd), lty=2)
  lines(xx, tolog(x.fit$estimate - sd*x.fit$sd), lty=2)
}
