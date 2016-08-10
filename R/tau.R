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
#' @return list with data frame (with depth, ratio, tau) and the mean depth beyond the cutoff
#' @export
FitTau <- function(mobile, immobile, depth, data, cutoff, verbose=TRUE) {
  name.mobile <- mobile
  name.immobile <- immobile
  if(verbose) message("fitting ", paste0(name.mobile, "/", name.immobile))
  depth <- data[[depth]]
  mobile <- data[[mobile]]
  immobile <- data[[immobile]]
  ratio <- mobile/immobile
  mm <- mean(mobile[depth>=cutoff])/mean(immobile[depth>=cutoff])
  tau <- ratio/mm - 1
  list(data=data.frame(depth=depth, ratio=ratio, tau=tau), meanratio=mm, cutoff=cutoff)
}
