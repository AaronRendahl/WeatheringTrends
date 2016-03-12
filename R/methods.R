fitted.ElementRatio <- function(x, depth=x$data$depth) {
  do.call("getmsd", c(list(x=depth), as.list(x$par)))
}

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
