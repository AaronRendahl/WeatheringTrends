## define mobile elements
## define immobile elements (multiple, please)
## assign parent material by averaging all values below a user defined depth
## print/output a grid of all elements like we have done with the weathering trends outputs
## apply weathering trends to log transformed tau values
## -- so we can see the piecewise regression
## -- and apply a confidence interval on the depth

#' Fit the element ratio for a single pair
#'
#' \code{FitTau} creates the mobile/immobile ratio and fits it to the depth.
#' @param mobile the name of the mobile variable
#' @param immobile the name of the immobile variable
#' @param depth the name of the depth variable
#' @param data the data set where these variables are found
#' @param cutoff how deep to assume constant
#' @param verbose set verbosity
#' @return Tau object, a list with data frame (with depth, ratio, tau) and the mean depth beyond the cutoff
#' @export
FitTau <- function(mobile, immobile, depth, data, cutoff, verbose=FALSE) {
  name.mobile <- mobile
  name.immobile <- immobile
  if(verbose) message("fitting ", paste0(name.mobile, "/", name.immobile))
  dat <- data[,c(depth, mobile, immobile)]
  names(dat) <- c("depth", "mobile", "immobile")
  dat <- dat[complete.cases(dat),]
  ratio <- with(dat, mobile/immobile)
  mm <- with(dat, mean(mobile[depth>=cutoff])/mean(immobile[depth>=cutoff]))
  tau <- ratio/mm - 1
  out <- list(data=data.frame(depth=dat$depth, ratio=ratio, tau=tau),
              meanratio=mm, cutoff=cutoff, mobile=name.mobile, immobile=name.immobile)
  class(out) <- c("Tau", class(out))
  out
}

#' print a Tau object
#'
#' print basic information about a Tau object
#' @param x the object
#' @param digits how many digits to round output to
#' @param ... unused
#' @export
print.Tau <- function(x, digits=3, ...) {
  cat("Tau for ", x$mobile,"/",x$immobile,"\n", sep="")
  cat("Depth cutoff at ", round(x$cutoff, digits=digits),"\n", sep="")
  cat("Ratio of parent material means: ", round(x$meanratio, digits=digits),"\n", sep="")
}

#' Fit a bunch of Taus all at once
#'
#' \code{FitTaus} runs \code{FitTau} on several elements
#' @param mobile vector of mobile variable names
#' @param immobile vector of immobile variable names
#' @param depth name of the depth variable
#' @param data data frame where these variables are found
#' @param cutoff how deep to assume constant, a vector of same length as immobile, or a scalar to use the same depth for all
#' @return an Taus object, which is a list of lists Tau objects
#' @export
FitTaus <- function(mobile, immobile, depth, data, cutoff) {
  if(length(cutoff)==1) cutoff <- rep(cutoff, length(immobile))
  if(length(cutoff) != length(immobile)) {
    stop("cutoff is not a single value or the same length as immobile")
  }
  ok <- names(data)[colSums(!is.na(data)) > 0]
  mobile <- mobile[mobile %in% ok]
  immobile <- immobile[immobile %in% ok]
  cutoff <- cutoff[immobile %in% ok]
  names(mobile) <- mobile
  immobileidx <- seq_along(immobile)
  names(immobileidx) <- immobile
  out <- lapply(mobile, function(y) {
    lapply(immobileidx, function(idx) {
      x <- immobile[idx]
      cuti <- cutoff[idx]
      FitTau(mobile=y, immobile=x, depth=depth, data=data, cutoff=cuti)
    })
  })
  class(out) <- "Taus"
  out
}

#'
#' plot a Tau object
#' @param x object
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param main main title of plot
#' @param ... additional parameters sent to plot
#' @return NULL
#' @export
plot.Tau <- function(x, xlim=NULL, ylim=NULL, main=paste0(x$mobile, "/", x$immobile), ...) {
  if(is.null(ylim)) {
    ylim <- range(x$data$depth, na.rm=TRUE)
    ylim <- rev(ylim + c(-1,1)*0.05*diff(ylim))
  }
  if(is.null(xlim)) {
    xlim <- range(x$data$tau, na.rm=TRUE)
    xlim <- xlim + c(-1,1)*0.05*diff(xlim)
  }
  graphics::plot(x$data$tau, x$data$depth, xlim=xlim, ylim=ylim,
       main=main, xlab="tau", ylab="depth", ...)
  graphics::abline(h=x$cutoff, v=0)
}

#' plot a Taus object
#'
#' plot a Taus object
#' @param x the object
#' @param scales set axis width; sliced will use same width for all plots, but allow for shifting as needed, free will set each plot differently
#' @param ... additional parameters sent to the individual plots
#' @export
plot.Taus <- function(x, scales=c("same", "free"), ...) {
  op <- par("mfrow", "mar")
  on.exit(par(op))
  scales <- match.arg(scales)
  nm <- length(x)
  ni <- length(x[[1]])
  if(scales=="same") {
    xlim <- range(sapply(x, function(xj) sapply(xj, function(xi) range(xi$data$tau))))
    xlim <- xlim + c(-1,1)*0.05*range(xlim)
  } else if(scales=="free") {
    xlim <- NULL
  }
  graphics::par(mfrow=c(nm, ni), mar=c(2.5, 2.5, 2, 0))
  for(i in 1:nm) for(j in 1:ni) graphics::plot(x[[i]][[j]], xlim=xlim, ...)
}
