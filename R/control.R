#' Set the control parameters for the optimization of the element ratio fit
#'
#' \code{ControlElementRatioFit} sets the control parameters for the optimization
#' of the element ratio fit
#'
#' @param depth depth
#' @param logratio logratio
#' @param loglinear allows the linear portion to be linear on the log(ratio) scale instead of the ratio scale
#' @param p.start p.start
#' @param pd.start pd.start Where d should start, in terms of proportion of maximum depth
#' @param c.start c.start
#' @param d.start d.start
#' @param par.start par.start
#' @param lower lower
#' @param upper upper
#' @param parscale parscale
#' @return data frame with the starting parameters, the lower and upper bounds, and the scaling
#' @export
ControlElementRatioFit <- function(depth, logratio, loglinear, p.start=0.2, pd.start=0.5,
                                   c.start=diff(range(logratio)) / 2,
                                   d.start=pd.start*max(depth),
                                   par.start,
                                   lower=c(p=0, d=0.05*max(depth), c=-diff(range(logratio)),
                                           s1=0.1*stats::sd(logratio), s2=0.1*stats::sd(logratio), r=min(logratio)),
                                   upper=c(p=0.95, d=max(depth), c=diff(range(logratio)),
                                           s1=stats::sd(range(logratio)), s2=stats::sd(range(logratio)), r=max(logratio)),
                                   parscale=c(p=1, d=max(depth), c=1, s1=1, s2=1, r=1)
) {
  if(missing(par.start)) {
    par.start <- c(p = p.start,
                   d = d.start,
                   c = c.start)
    fit.start <- do.call("getmsd", c(list(x=depth, y=logratio, loglinear=loglinear, s1=1, s2=1, fit.only=TRUE),
                                     as.list(par.start)))
    sd.start <- stats::sd(logratio - fit.start)
    sdlo <- max(lower[c("s1", "s2")])
    sdhi <- min(upper[c("s1", "s2")])
    if(sd.start < sdlo | sd.start > sdhi) sd.start <- (sdlo + sdhi) / 2
    par.start <- c(par.start, s1=sd.start, s2=sd.start)
  }
  r.start <- do.call("getmsd", c(list(x=depth, y=logratio, loglinear=loglinear, r.only=TRUE), par.start))
  par.start <- c(par.start, r=r.start)
  n <- names(par.start)
  data.frame(par.start=par.start, lower=lower[n], upper=upper[n], parscale=parscale[n])
}
