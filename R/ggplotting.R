getdata <- function(x, log=TRUE, addnames=FALSE) {
  out <- x$data
  if(!log) {
    out$logratio <- exp(out$logratio)
    names(out)[2] <- "ratio"
  }
  if(addnames) out <- cbind(mobile=x$mobile, immobile=x$immobile,
                            what=paste0(x$mobile, "/", x$immobile),
                            out, stringsAsFactors=FALSE)
  out
}

getlines <- function(x, sd=1, log=TRUE, addnames=FALSE) {
  depth <- x$data$depth
  xx <- c(0, seq(x$output[["depth1"]], x$output[["depth2"]], len=50), max(depth))
  tolog <- if(log) identity else exp
  x.fit <- fitted.ElementRatio(x, depth=xx)
  x.fit <- cbind(depth=xx, x.fit)
  x.fit$lower <- tolog(x.fit$estimate - sd*x.fit$sd)
  x.fit$upper <- tolog(x.fit$estimate + sd*x.fit$sd)
  x.fit$estimate <- tolog(x.fit$estimate)
  if(addnames) x.fit <- cbind(mobile=x$mobile, immobile=x$immobile,
                              what=paste0(x$mobile, "/", x$immobile),
                              x.fit, stringsAsFactors=FALSE)
  x.fit
}

rbindfits <- function(fits, FUN, ...) {
  out <- do.call(rbind, lapply(fits, function(fi) do.call(rbind, lapply(fi, FUN, ...))))
  rownames(out) <- NULL
  out
}
