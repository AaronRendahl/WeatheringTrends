#' Set the control parameters for the optimization of the element ratio fit
#'
#' \code{ControlElementRatioFit} sets the control parameters for the optimization
#' of the element ratio fit
#'
#' @param depth depth
#' @param logratio logratio
#' @param p1.start p1.start
#' @param p2.start p2.start
#' @param c.start c.start
#' @param x2.start x2.start
#' @param par.start par.start
#' @param lower lower
#' @param upper upper
#' @param parscale parscale
#' @return data frame with the starting parameters, the lower and upper bounds, and the scaling
#' @export
ControlElementRatioFit <- function(depth, logratio, p1.start=0.2, p2.start=0.5,
                                   c.start=diff(range(logratio)) / 2,
                                   x2.start=p2.start*max(depth),
                                   par.start,
                                   lower=c(p1=0, x2=0.05*max(depth), c=0,
                                           sd1=0.1*sd(logratio), sd2=0.1*sd(logratio), d=min(logratio)),
                                   upper=c(p1=0.95, x2=max(depth), c=diff(range(logratio)),
                                           sd1=sd(range(logratio)), sd2=sd(range(logratio)), d=max(logratio)),
                                   parscale=c(p1=1, x2=max(depth), c=1, sd1=1, sd2=1, d=1)
) {
  if(missing(par.start)) {
    par.start <- c(p1 = p1.start,
                   x2 = x2.start,
                   c = c.start)
    fit.start <- do.call("getmsd", c(list(x=depth, y=logratio, sd1=1, sd2=1, fit.only=TRUE),
                                     as.list(par.start)))
    sd.start <- sd(logratio - fit.start)
    sdlo <- max(lower[c("sd1", "sd2")])
    sdhi <- min(upper[c("sd1", "sd2")])
    if(sd.start < sdlo | sd.start > sdhi) sd.start <- (sdlo + sdhi) / 2
    par.start <- c(par.start, sd1=sd.start, sd2=sd.start)
  }
  d.start <- do.call("getmsd", c(list(x=depth, y=logratio, d.only=TRUE), par.start))
  par.start <- c(par.start, d=d.start)
  n <- names(par.start)
  data.frame(par.start=par.start, lower=lower[n], upper=upper[n], parscale=parscale[n])
}
