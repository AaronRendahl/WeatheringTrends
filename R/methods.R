#' Extract model residuals for an ElementRatio object
#'
#' get residuals for an ElementRatio object
#' @param object the object
#' @param type either response or pearson residuals
#' @param ... other arguments.
#' @export
residuals.ElementRatio <- function(object, type=c("response", "pearson"), ...) {
  type <- match.arg(type)
  ff <- fitted(object)
  res <- object$data$logratio - ff$estimate
  if(type=="pearson") res <- res/ff$sd
  res
}

#' get coefficients for an ElementRatios object
#'
#' get coefficients for an ElementRatios object
#' @param object the ElementRatios object
#' @param type Either output, par, or par.long; output yields the parameters
#' that are most interpretable, par yields the fitted parameters, and par.long
#' yields the fitted parameters in long format with the confidence interval,
#' if available
#' @param ... additional parameters, unused
#' @export
coef.ElementRatios <- function(object, type=c("output","par","par.long"), ...) {
  type <- match.arg(type)
  out <- do.call(rbind, lapply(object, function(x) {
    do.call(rbind, lapply(x, function(y) {
      coef(y, type=type)
    }))
  }))
  rownames(out) <- NULL
  out
}

#' get coefficients for an ElementRatio object
#'
#' get coefficients for an ElementRatio object
#' @param object the ElementRatio object
#' @param type Either output, par, or par.long; output yields the parameters
#' that are most interpretable, par yields the fitted parameters, and par.long
#' yields the fitted parameters in long format with the confidence interval,
#' if available
#' @param ... additional parameters, unused
#' @export
coef.ElementRatio <- function(object, type=c("output","par","par.long"), ...) {
  type <- match.arg(type)
  if(type %in% c("output","par")) {
    out <- data.frame(rbind(object[[type]]))
  } else {
    out <- data.frame(estimate=object$par)
    m <- match(rownames(object$confint), rownames(out))
    out$upper <- out$lower <- NA
    out$lower[m] <- object$confint[,"lower"]
    out$upper[m] <- object$confint[,"upper"]
  }
  out <- cbind(mobile=object$mobile, immobile=object$immobile, out, stringsAsFactors=FALSE)
  rownames(out) <- NULL
  out
}

#' plot an ElementRatios object
#'
#' plot an ElementRatios object
#' @param x the object
#' @param morelines also add lines from this ElementRatios object
#' @param scales set axis width; sliced will use same width for all plots, but allow for shifting as needed, free will set each plot differently
#' @param ... additional parameters sent to the individual plots
#' @export
plot.ElementRatios <- function(x, morelines=NULL, scales=c("sliced", "free"), ...) {
  scales <- match.arg(scales)
  nm <- length(x)
  ni <- length(x[[1]])
  if(scales=="sliced") {
    m <- max(sapply(x, function(xj) sapply(xj, function(xi) diff(range(xi$data$logratio)))))
  } else if(scales=="free") {
    m <- NA
  }
  par(mfrow=c(nm, ni), mar=c(2.5, 2.5, 2, 0))
  for(i in 1:nm) for(j in 1:ni) plot(x[[i]][[j]], morelines=morelines[[i]][[j]], range=m, ...)
}

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
  if(!is.null(x$confint)) {
    out$upr <- out$lwr <- NA
    out$lwr[m] <- x$confint[,"lower"]
    out$upr[m] <- x$confint[,"upper"]
  }
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
  do.call("getmsd", c(list(x=depth, loglinear=object$loglinear), as.list(object$par)))
}

addlines <- function(x, sd=1, rotate=TRUE, log=TRUE, ...) {
  x.fit <- getlines(x, sd=sd, log=log)
  if(rotate) {
    lines(x.fit$estimate, x.fit$depth, ...)
    lines(x.fit$lower, x.fit$depth, lty=2, ...)
    lines(x.fit$upper, x.fit$depth, lty=2, ...)
  } else {
    lines(x.fit$depth, x.fit$estimate, ...)
    lines(x.fit$depth, x.fit$lower, lty=2, ...)
    lines(x.fit$depth, x.fit$upper, lty=2, ...)
  }
}

#' plot an ElementRatio object
#'
#' plot an ElementRatio object
#' @param x object
#' @param morelines also add lines from this ElementRatio object
#' @param sd how many sd out to put the lines
#' @param main title of plot
#' @param rotate should the plot be rotated to have depth on vertical axis
#' @param log to put on log scale or not
#' @param ci.lty line type of confidence intervals
#' @param responselabel label of response axis
#' @param range how wide the range of the axis should be; set to NA to set automatically
#' @param ... additional parameters sent to plot
#' @return NULL
#' @export
plot.ElementRatio <- function(x, morelines=NULL, sd=1,
                              main=paste0(x$mobile, "/", x$immobile),
                              rotate=TRUE, log=TRUE, ci.lty=3,
                              range=NA,
                              responselabel=if(log) "logratio" else "ratio", ...) {
  depth <- x$data$depth
  ## start with points
  tolog <- if(log) identity else function(x) 10^x
  rr <- range(tolog(x$data$logratio))
  if(is.na(range)) range <- diff(rr)*1.05
  lim <- mean(rr) + c(-1,1)*range/2
  if(rotate) {
    plot(tolog(x$data$logratio), depth, main=main, xlab=responselabel, ylim=rev(range(depth)), xlim=lim, ...)
  } else {
    plot(depth, tolog(x$data$logratio), main=main, ylab=responselabel, ylim=lim, ...)
  }
  ## add lines
  addlines(x, sd=sd, rotate=rotate, log=log)
  if(!is.null(morelines)) addlines(morelines, sd=sd, rotate=rotate, log=log, col="red")
  ## add confidence intervals
  cis <- getcis(x, log=log)
  if(rotate) {
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
