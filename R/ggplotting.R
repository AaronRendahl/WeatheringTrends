#' Extract information from ElementRatio fits
#'
#' Extract information from ElementRatio fits
#' @param x the object
#' @param ... additional arguments
#' @return a data frame with the desired information
#' @export
extractFrom <- function(x, ...) {
  UseMethod("extractFrom")
}

#' @describeIn extractFrom for ElementRatio
#' @export
extractFrom.ElementRatio <- function(x, what=c("data","lines","cis"), log=TRUE, addnames=FALSE, ...) {
  what <- match.arg(what)
  if(what=="data") getdata(x, log=log, addnames=addnames, ...)
  else if(what=="lines") getlines(x, log=log, addnames=addnames, ...)
  else if(what=="cis") getcis(x, log=log, addnames=addnames, ...)
}

#' @describeIn extractFrom for ElementRatios
#' @param what either data, lines, or cis
#' @param log should response be on logratio scale?
#' @param addnames should the mobile and immobile names be added to the output data frame
#' @export
extractFrom.ElementRatios <- function(x, what=c("data","lines","cis"), log=TRUE, addnames=TRUE, ...) {
  what <- match.arg(what)
  rbindfits(x, extractFrom.ElementRatio, what=what, log=log, addnames=addnames, ...)
}

getdata <- function(x, log=TRUE, addnames=FALSE, ...) {
  out <- x$data
  if(!log) {
    out$logratio <- 10^(out$logratio)
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
  tolog <- if(log) identity else 10^
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

getcis <- function(x, log=TRUE, addnames=FALSE) {
  tolog <- if(log) identity else 10^
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
  if(!is.null(cis) & addnames) {
    cis <- cbind(mobile=x$mobile, immobile=x$immobile,
                what=paste0(x$mobile, "/", x$immobile),
                cis, stringsAsFactors=FALSE)
  }
  cis
}

rbindfits <- function(fits, FUN, ...) {
  out <- do.call(rbind, lapply(fits, function(fi) do.call(rbind, lapply(fi, FUN, ...))))
  rownames(out) <- NULL
  out
}
