#' print an ElementRatio object
#'
#' print an ElementRatio object
#' @param x the object
#' @param digits number of decimal places to show
#' @param ... unused
#' @export
print.ElementRatio <- function(x, digits=3, ...) {
  cat("Fit of ", x$mobile, "/", x$immobile, "\n\n", sep="")
  out <- data.frame(estimate=x$par)
  m <- match(rownames(x$confint), rownames(out))
  out$upr <- out$lwr <- NA
  out$lwr[m] <- x$confint[,"lower"]
  out$upr[m] <- x$confint[,"upper"]
  cat("Fitted Parameters (with CI, if found)\n")
  ff <- function(x, digits=3, format="f", ...) {
    n <- is.na(x)
    out <- formatC(x, digits=digits, format=format, ...)
    out[n] <- ""
    out
  }
  for(i in seq_along(out)) out[[i]] <- ff(out[[i]])
  print(out)
  cat("\n")
  print(x$output, digits=digits)
}

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
#' @param rotate should the plot be rotated to have depth on vertical axis
#' @param log to put on log scale or not
#' @param ci.lty line type of confidence intervals
#' @param responselabel label of response axis
#' @param ... additional parameters sent to plot
#' @return NULL
#' @export
plot.ElementRatio <- function(x, sd=1, main=paste0(x$mobile, "/", x$immobile), rotate=TRUE, log=FALSE, ci.lty=3,
                      responselabel=if(log) "logratio" else "ratio", ...) {
  depth <- x$data$depth
  xx <- c(0, seq(x$output[["depth1"]], x$output[["depth2"]], len=50), max(depth))
  tolog <- if(log) identity else exp
  x.fit <- fitted.ElementRatio(x, depth=xx)
  x.fit$lower <- tolog(x.fit$estimate + sd*x.fit$sd)
  x.fit$upper <- tolog(x.fit$estimate - sd*x.fit$sd)
  x.fit$estimate <- tolog(x.fit$estimate)
  cis <- NULL
  if(!is.null(x$confint)) {
    if("d" %in% rownames(x$confint)) {
      ci <- x$confint["d",]
      x0 <- x$output[["depth2"]]
      y0 <- tolog(x$output[["logratio2"]])
      cis <- data.frame(x0=ci[1], x1=ci[2], y0=y0, y1=y0, xd=0, yd=0.005)
      rownames(cis) <- "d"
    }
  }
  if(rotate) {
    plot(tolog(x$data$logratio), depth, main=main, xlab=responselabel, ylim=rev(range(depth)), ...)
    lines(x.fit$estimate, xx)
    lines(x.fit$lower, xx, lty=2)
    lines(x.fit$upper, xx, lty=2)
    xl <- diff(grconvertX(c(0,1), "npc", "user"))
    yl <- diff(grconvertY(c(0,1), "npc", "user"))
    dx <- diff(grconvertX(c(0, 1), "npc", "inches"))
    dy <- diff(grconvertY(c(0, 1), "npc", "inches"))
    len <- sqrt(dx^2 + dy^2) * 0.01
    if(!is.null(cis)) for(i in seq_len(nrow(cis))) {
      arrows(y0=cis$x0[i]+cis$xd[i]*yl, y1=cis$x1[i]+cis$xd[i]*yl,
             x0=cis$y0[i]+cis$yd[i]*xl, x1=cis$y1[i]+cis$yd[i]*xl,
             code=3, angle=90, length=len, lty=ci.lty)
    }
  } else {
    plot(depth, tolog(x$data$logratio), main=main, ylab=responselabel, ...)
    lines(xx, x.fit$estimate)
    lines(xx, x.fit$lower, lty=2)
    lines(xx, x.fit$upper, lty=2)
    xl <- diff(grconvertX(c(0,1), "npc", "user"))
    yl <- diff(grconvertY(c(0,1), "npc", "user"))
    dx <- diff(grconvertX(c(0, 1), "npc", "inches"))
    dy <- diff(grconvertY(c(0, 1), "npc", "inches"))
    len <- sqrt(dx^2 + dy^2) * 0.01
    if(!is.null(cis)) for(i in seq_len(nrow(cis))) {
      arrows(x0=cis$x0[i]+cis$xd[i]*xl, x1=cis$x1[i]+cis$xd[i]*xl,
             y0=cis$y0[i]+cis$yd[i]*yl, y1=cis$y1[i]+cis$yd[i]*yl,
             code=3, angle=90, length=len, lty=ci.lty)
    }
  }
}
