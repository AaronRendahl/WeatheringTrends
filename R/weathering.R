getmsd <- function(x, y, p, d, c, s1, s2, r, fit.only=FALSE, r.only=FALSE, loglinear) {
  x1 <- p * d
  x2 <- d
  k1 <- x < x1
  k12 <- x >= x1 & x < x2
  x12 <- x[k12]
  tmp <- (x2 - x12) / (x2 - x1)
  ## mean
  m <- rep(0, length(x))
  m[k1] <- -c
  if(loglinear) {
    m[k12] <- -c*tmp
  } else {
    m[k12] <- log10(1 - (1 - 10^(-c)) * tmp)
  }
  ## standard deviation
  s <- rep(s2, length(x))
  s[k1] <- s1
  s[k12] <- s2 - (s2 - s1) * tmp
  ## r and output
  if(missing(r)) {
    if(any(s==0)) {
      r <- mean((y-m)[s==0])
    } else {
      r <- weighted.mean(y - m, 1 / s^2)
    }
  }
  m <- m + r
  if(r.only) return(r)
  if(fit.only) return(m)
  data.frame(estimate=m, sd=s)
}
