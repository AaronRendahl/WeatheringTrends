#' Fit a bunch of element ratios all at once
#'
#' \code{FitElementRatios} runs \code{FitElementRatio} on several elements
#' @param mobile vector of mobile variable names
#' @param immobile vector of immobile variable names
#' @param depth name of the depth variable
#' @param data data frame where these variables are found
#' @param ... additional parameters sent to \code{ControlElementRatioFit}
#' @return an ElementRatios object, which is a list of lists of ElementRatio objects
#' @export
FitElementRatios <- function(mobile, immobile, depth, data,...) {
  ok <- names(data)[colSums(!is.na(data)) > 0]
  mobile <- mobile[mobile %in% ok]
  immobile <- immobile[immobile %in% ok]
  names(mobile) <- mobile
  names(immobile) <- immobile
  out <- lapply(mobile, function(y) {
    lapply(immobile, function(x) {
      FitElementRatio(mobile=y, immobile=x, depth=depth, data=data, ...)
    })
  })
  class(out) <- "ElementRatios"
  out
}

refits <- function(par, fits) {
  lapply(fits, function(x)
    do.call(refit, c(list(x), as.list(par))))
}

# @import mvtnorm
neglogliks <- function(par, fits, usecor=FALSE) {
  if(!missing(par))
  fits <- refits(par, fits)
  sum(sapply(fits, function(f) negloglik(f$par, f$data$depth, f$data$logratio, loglinear=FALSE)))
  #res <- sapply(fits, residuals, type="pearson")
  #sig <- if(usecor) cor(res) else diag(ncol(res))
  #-sum(mvtnorm::dmvnorm(res, sigma=sig, log=TRUE))
}

ControlConstantRatioFit <- function(f) {
  cc <- lapply(f, function(x) x$control[c("p","d","c"),])
  data.frame(par.start=rowMeans(sapply(cc, `[[`, "par.start")),
             lower=apply(sapply(cc, `[[`, "lower"), 1, min),
             upper=apply(sapply(cc, `[[`, "upper"), 1, max),
             parscale=rowMeans(sapply(cc, `[[`, "parscale")),
             row.names=c("p","d","c"))
}

FitConstantRatio <- function(f, controlfun, usecor) {
  control <- controlfun(f)
  par.start <- stats::setNames(control$par.start, rownames(control))
  opt2 <- stats::optim(par=par.start, fn=neglogliks, fits=f, usecor=usecor)
  refits(opt2$par, f)
}

#' Refit to have the same ratio fit over several immobile elements
#'
#' \code{FitConstantRatios}
#' @param f ElementRatios object
#' @param controlfun function to get control parameters for each mobile element fit
#' @param verbose set verbosity
#' @return an ElementRatios object, which is a list of lists of ElementRatio objects
#' @export
FitConstantRatios <- function(f, controlfun=ControlConstantRatioFit, verbose=TRUE) {
  out <- lapply(names(f), function(n) {
    if(verbose) message("fitting ", n)
    FitConstantRatio(f[[n]], controlfun=ControlConstantRatioFit, usecor=FALSE)
  })
  class(out) <- "ElementRatios"
  out
}
