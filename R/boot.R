#' Parametric Bootstrap of ElementRatio object
#'
#' \code{BootElementRatio}
#'
#' @param x the ElementRatio object
#' @param hessian get the hessian from the optimization
#' @param ... additional parameters sent to \code{ControlElementRatioFit}
#' @return a new ElementRatio object, with bootstrapped data
#' @export
BootElementRatio <- function(x, hessian=!is.null(x$optim$hessian), ...) {
  f <- stats::fitted(x)
  di <- data.frame(depth=x$data$depth, logratio=stats::rnorm(nrow(f), mean=f$estimate, sd=f$sd))
  refit(list(data=di, control=ControlElementRatioFit(di$depth, di$logratio, ...),
             mobile=x$mobile, immobile=x$immobile), hessian=hessian)
}
