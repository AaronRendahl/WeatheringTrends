getmsd <- function(x, y, p1, x2, c, sd1, sd2, d, fit.only=FALSE, d.only=FALSE) {
  x1 <- p1 * x2
  k1 <- x < x1
  k12 <- x >= x1 & x < x2
  x12 <- x[k12]
  tmp <- (x2 - x12) / (x2 - x1)
  ## mean
  m <- rep(0, length(x))
  m[k1] <- -c
  m[k12] <- log(1 - (1 - exp(-c)) * tmp)
  ## standard deviation
  s <- rep(sd2, length(x))
  s[k1] <- sd1
  s[k12] <- sd2 - (sd2 - sd1) * tmp
  ## d and output
  if(missing(d)) {
    d <- weighted.mean(y - m, 1 / s^2)
  }
  m <- m + d
  if(d.only) return(d)
  if(fit.only) return(m)
  data.frame(estimate=m, sd=s)
}
