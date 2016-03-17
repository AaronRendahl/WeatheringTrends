#' get fitted values for an ElementRatio object
#'
#' get fitted values for an ElementRatio object
#' @param object the object
#' @param depth the depth to get the fitted values for
#' @param ... unused
#' @return a data frame with the estimate and sd for each depth
#' @export
fitted.ElementRatio <- function(object, depth=object$data$depth, ...) {
  do.call("getmsd", c(list(x=depth), as.list(object$par)))
}

#' plot an ElementRatio object
#'
#' plot an ElementRatio object
#' @param x object
#' @param sd how many sd out to put the lines
#' @param main title of plot
#' @param log to put on log scale or not
#' @param ylab y label
#' @param ... additional parameters sent to plot
#' @return NULL
#' @export
plot.ElementRatio <- function(x, sd=1, main=paste0(x$mobile, "/", x$immobile), log=FALSE,
                      ylab=if(log) "logratio" else "ratio", ...) {
  depth <- x$data$depth
  xx <- c(0, seq(x$output[["depth1"]], x$output[["depth2"]], len=50), max(depth))
  tolog <- if(log) identity else exp
  x.fit <- fitted.ElementRatio(x, depth=xx)
  x.fit$lower <- tolog(x.fit$estimate + sd*x.fit$sd)
  x.fit$upper <- tolog(x.fit$estimate - sd*x.fit$sd)
  x.fit$estimate <- tolog(x.fit$estimate)
  cis <- NULL
  if(!is.null(x$confint)) {
    if("x2" %in% rownames(x$confint)) {
      ci <- x$confint["x2",]
      d0 <- diff(tolog(range(x$data$logratio)))*0.01
      x0 <- x$output[["depth2"]]
      y0 <- tolog(x$output[["logratio2"]])
      dx <- diff(grconvertX(c(0, 1), "npc", "inches"))
      dy <- diff(grconvertY(c(0, 1), "npc", "inches"))
      len <- sqrt(dx^2 + dy^2) * 0.02
      cis <- data.frame(x0=ci[1], x1=ci[2], y0=y0 + d0, y1=y0 + d0)
      rownames(cis) <- "x2"
    }
  }
  plot(depth, tolog(x$data$logratio), main=main, ylab=ylab, ...)
  lines(xx, x.fit$estimate)
  lines(xx, x.fit$lower, lty=2)
  lines(xx, x.fit$upper, lty=2)
  if(!is.null(cis)) for(i in seq_len(nrow(cis))) {
    arrows(x0=cis$x0[1], x1=cis$x1[1], y0=cis$y0[1], y1=cis$y1[1],
           code=3, angle=90, length=len, lty=3)
  }
}
