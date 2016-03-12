#' Parametric Bootstrap of ElementRatio object
#'
#' \code{BootElementRatio}
#'
#' @param x the ElementRatio object
#' @return a new ElementRatio object, with bootstrapped data
#' @export
BootElementRatio <- function(x) {
  f <- fitted(x)
  di <- data.frame(depth=x$data$depth, logratio=rnorm(nrow(f), mean=f$estimate, sd=f$sd))
  refit(list(data=di, control=ControlElementRatioFit(di$depth, di$logratio),
             mobile=x$mobile, immobile=x$immobile))
}
