#' Fit a bunch of element ratios all at once
#'
#' \code{FitElementRatios} runs \code{FitElementRatio} on several elements
#' @param mobile vector of mobile variable names
#' @param immobile vector of immobile variable names
#' @param depth name of the depth variable
#' @param data data frame where these variables are found
#' @param ... additional parameters sent to \code{ControlElementRatioFit}
#' @return a list of lists of ElementRatio objects
#' @export
FitElementRatios <- function(mobile, immobile, depth, data,...) {
  ok <- names(data)[colSums(!is.na(data)) > 0]
  mobile <- mobile[mobile %in% ok]
  immobile <- immobile[immobile %in% ok]
  names(mobile) <- mobile
  names(immobile) <- immobile
  lapply(mobile, function(y) {
    lapply(immobile, function(x) {
      FitElementRatio(mobile=y, immobile=x, depth=depth, data=data, ...)
    })
  })
}
