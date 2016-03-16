#' profile an ElementRatio object
#'
#' \code{profile.ElementRatio} profiles an ElementRatio object to get a
#' confidence interval for a desired parameter
#'
#' @param fitted the ElementRatio object
#' @param variable the variable to profile over
#' @param confidence the desired confidence level
#' @param tolerance how tight to profile it
#' @param maxiter maximum number of iterations
#' @param ... unused
#' @return the original object with elements profile and confint added
#' @export
profile.ElementRatio <- function(fitted, variable, confidence=0.95, tolerance=0.1, maxiter=10, ...) {
  x <- fitted
  k <- qchisq(confidence, 1)
  if(is.null(x$profile)) x$profile <- list()
  if(variable=="d") {
    a <- min(x$data$logratio)
    b <- max(x$data$logratio)
  } else {
    a <- x$control[variable,"lower"]
    b <- x$control[variable,"upper"]
  }
  out <- data.frame(i=0,x=x$par[[variable]], y=0)
  mm <- x$opt$value
  ## try going up
  ## message("up")
  mx <- do.call("refit", c(list(x), setNames(list(b), variable)))$optim$value - mm
  out <- rbind(out, c(1, b, mx))
  if(mx < k) { ## if not enough change at boundary don't bother looking
    upper <- b
  } else {
    iter <- 1
    ok1 <- ok2 <- FALSE
    p0 <- x$par[[variable]]
    p1 <- b
    m0 <- 0
    m1 <- mx
    while((!ok1 || !ok2) && iter < maxiter) {
      iter <- iter + 1
      ## message(iter)
      px <- (p0 + p1)/2
      mx <- do.call("refit", c(list(x), setNames(list(px), variable)))$optim$value - mm
      out <- rbind(out, c(iter, px, mx))
      if(mx < k) {
        if(k - mx < tolerance) ok1 <- TRUE
        p0 <- px
        m0 <- mx
      } else {
        if(mx - k < tolerance) ok2 <- TRUE
        p1 <- px
        m1 <- mx
      }
      ## if(iter==maxiter) message("hit max iterations going up for ", variable)
    }
    upper <- p0 + (p1-p0)*(k-m0)/(m1-m0)
  }
  ## try going down
  ## message("down")
  mx <- do.call("refit", c(list(x), setNames(list(a), variable)))$optim$value - mm
  out <- rbind(out, c(-1, a, mx))
  if(mx < k) { ## if not enough change at boundary don't bother looking
    lower <- a
  } else {
    iter <- 1
    ok1 <- ok2 <- FALSE
    p1 <- x$par[[variable]]
    p0 <- a
    m1 <- 0
    m0 <- mx
    while((!ok1 || !ok2) && iter < maxiter) {
      iter <- iter + 1
      ## message(iter)
      px <- (p0 + p1)/2
      mx <- do.call("refit", c(list(x), setNames(list(px), variable)))$optim$value - mm
      out <- rbind(out, c(-iter, px, mx))
      if(mx < k ) {
        if(k - mx < tolerance) ok1 <- TRUE
        p1 <- px
        m1 <- mx
      } else {
        if(mx - k < tolerance) ok2 <- TRUE
        p0 <- px
        m0 <- mx
      }
      ## if(iter==maxiter) message("hit max iterations going down for ", variable)
    }
    lower <- p0 + (p1-p0)*(k-m0)/(m1-m0)
  }
  #out <- out[order(out$x),]
  x$profile[[variable]] <- out
  x$confint <- rbind(x$confint, matrix(c(lower, upper), ncol=2,
                                       dimnames=list(variable, c("lower", "upper"))))
  x
}
