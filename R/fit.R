negloglik <- function(par, depth, logratio, ...) {
  fit <- do.call("getmsd", c(list(x=depth, y=logratio), as.list(par), ...))
  if(any(fit$estimate==-Inf)) {print(fit); print(par)}
  -sum(dnorm(logratio, mean=fit$estimate, sd=fit$sd, log=TRUE))
}

#' Fit the element ratio
#'
#' \code{FitElementRatio}
#'
#' @param mobile the name of the mobile variable
#' @param immobile the name of the immobile variable
#' @param depth the name of the depth variable
#' @param data the data set where these variables are found
#' @param min.mobile will set zero mobile values to this value (default is the smallest value)
#' @param verbose set verbosity
#' @param hessian get the hessian from the optimization
#' @param ... additional parameters sent to \code{ControlElementRatioFit}
#' @return an ElementRatio object, with the output from the optimization and the optimal parameters, presented both in the
#' @export
FitElementRatio <- function(mobile, immobile, depth, data,
                   min.mobile,
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
  logratio <- log(mobile/immobile)
  control <- ControlElementRatioFit(depth=depth, logratio=logratio, ...)
  data <- data.frame(depth=depth, logratio=logratio)
  out <- list(data=data, control=control, mobile=name.mobile, immobile=name.immobile)
  refit(out, hessian=hessian)
}

refit <- function(x, hessian=FALSE, ...) {
  x$fixed <- list(...)
  n <- setdiff(rownames(x$control), names(x$fixed))
  n <- setdiff(n, "r")
  control <- x$control[n, ]
  par.start <- setNames(control$par.start, n)
  opt <- optim(par.start, negloglik, depth=x$data$depth, logratio=x$data$logratio, ...,
               hessian=hessian, method="L-BFGS-B",
               lower=control$lower, upper=control$upper,
               control=list(parscale=control$parscale))
  x$optim <- opt
  p <- opt$par
  if(!"r" %in% names(x$fixed)) {
    p <- c(p, r=do.call("getmsd", c(list(x=x$data$depth, y=x$data$logratio, r.only=TRUE), c(as.list(opt$par), ...))))
  }
  p <- x$par <- c(p, ...)[c("p", "d", "c", "s1", "s2", "r")]
  x$output <- c(depth1=p[["p"]]*p[["d"]], depth2=p[["d"]],
                logratio1=p[["r"]] - p[["c"]], logratio2=p[["r"]], p["s1"], p["s2"])
  class(x) <- "ElementRatio"
  x
}
