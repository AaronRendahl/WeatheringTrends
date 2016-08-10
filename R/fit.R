negloglik <- function(par, depth, logratio, loglinear, ...) {
  fit <- do.call("getmsd", c(list(x=depth, y=logratio, loglinear=loglinear), as.list(par), ...))
  if(any(fit$estimate==-Inf)) {stop("Infinite fit found; cannot continue.")}
  -sum(stats::dnorm(logratio, mean=fit$estimate, sd=fit$sd, log=TRUE))
}

#' Fit the element ratio for a single pair
#'
#' \code{FitElementRatio} creates the mobile/immobile ratio and fits it to the depth.
#'
#' @param mobile the name of the mobile variable
#' @param immobile the name of the immobile variable
#' @param depth the name of the depth variable
#' @param data the data set where these variables are found
#' @param min.mobile will set zero mobile values to this value (default is the smallest value)
#' @param profile variables to profile over automatically
#' @param loglinear allows the linear portion to be linear on the log10(ratio) scale instead of the ratio scale
#' @param verbose set verbosity
#' @param hessian get the hessian from the optimization
#' @param ... additional parameters sent to \code{ControlElementRatioFit}
#' @return an ElementRatio object, with the output from the optimization and the optimal parameters, presented both in the
#' @export
FitElementRatio <- function(mobile, immobile, depth, data,
                   min.mobile, profile="d", loglinear=FALSE,
                   verbose=TRUE, hessian=FALSE, ...) {
  name.mobile <- mobile
  name.immobile <- immobile
  if(verbose) message("fitting ", paste0(name.mobile, "/", name.immobile))
  depth <- data[[depth]]
  mobile <- data[[mobile]]
  immobile <- data[[immobile]]
  ok <- !is.na(immobile) & !is.na(mobile)
  if(!all(ok)) {
    depth <- depth[ok]
    mobile <- mobile[ok]
    immobile <- immobile[ok]
    if(verbose) message("removed ", sum(!ok), " observations with missing values")
  }
  fix <- mobile <= 0
  if(any(fix)) {
    if(missing(min.mobile)) {
      min.mobile <- min(mobile[!fix])
    }
    mobile[fix] <- min.mobile
    if(verbose) message("set ", sum(fix), " zero observations to the minimum value")
  }
  logratio <- log10(mobile/immobile)
  control <- ControlElementRatioFit(depth=depth, logratio=logratio, loglinear=loglinear, ...)
  data <- data.frame(depth=depth, logratio=logratio)
  out <- list(data=data, control=control, mobile=name.mobile, immobile=name.immobile, loglinear=loglinear)
  out <- refit(out, hessian=hessian)
  if(isTRUE(profile)) profile <- "d"
  if(!isTRUE(all.equal(profile, FALSE))) {
    for(pp in profile) out <- profile(out, pp)
  }
  out$s.overall <- c(s.overall=stats::sd(logratio))
  out
}

refit <- function(x, hessian=FALSE, ...) {
  x$fixed <- list(...)
  n <- setdiff(rownames(x$control), names(x$fixed))
  n <- setdiff(n, "r")
  control <- x$control[n, ]
  par.start <- stats::setNames(control$par.start, n)
  if(stats::sd(x$data$logratio)==0) {
    x$optim <- "No variation, optimization not performed."
    p <- x$par <- c(p=0, d=0, c=0, s1=0, s2=0, r=x$data$logratio[1])
  } else {
    opt <- stats::optim(par=par.start, fn=negloglik, depth=x$data$depth, logratio=x$data$logratio, ...,
                 loglinear=x$loglinear,
                 hessian=hessian, method="L-BFGS-B",
                 lower=control$lower, upper=control$upper,
                 control=list(parscale=control$parscale))
    x$optim <- opt
    p <- opt$par
    if(!"r" %in% names(x$fixed)) {
      p <- c(p, r=do.call("getmsd", c(list(x=x$data$depth, y=x$data$logratio, r.only=TRUE, loglinear=x$loglinear), c(as.list(opt$par), ...))))
    }
    p <- x$par <- c(p, ...)[c("p", "d", "c", "s1", "s2", "r")]
  }
  x$profile <- x$confint <- NULL
  x$output <- c(depth1=p[["p"]]*p[["d"]], depth2=p[["d"]],
                logratio1=p[["r"]] - p[["c"]], logratio2=p[["r"]], p["s1"], p["s2"])
  class(x) <- "ElementRatio"
  x
}
