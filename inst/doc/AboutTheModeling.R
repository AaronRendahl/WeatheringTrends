## ---- echo=FALSE, fig.width=8, fig.height=4------------------------------
p <- 0.25
d <- 8
c <- 1.5
s1 <- 0.25
s2 <- 0.3
r <- 1
xx <- c(0, seq(2, 8, len=21), 12)
fit <- WeatheringTrends:::getmsd(x=xx, p=p, d=8, c=c, s1=s1, s2=s2, r=r)
fit$x <- xx
fit <- within(fit,{
  lwr <- estimate - sd
  upr <- estimate + sd
})
par(mfrow=c(1,2), cex=0.8)
with(fit, {
  plot(x, estimate, type="l", ylab="log10(y)", ylim=c(r-c-s1, r+s2), xaxt="n", yaxt="n", frame=FALSE)
  lines(x, lwr, lty=2)
  lines(x, upr, lty=2)
  ll <- 0.1
  arrows(x0=d+1,y0=r,y1=r+s2,code=3,len=ll)
  text(d+1, r+s2/2, expression(s[2]), pos=2)
  arrows(x0=p*d-1,y0=r-c,y1=r-c+s1,code=3,len=ll)
  text(p*d-1, r-c+s1/2, expression(s[1]), pos=2)
  axis(1, c(p*d, d), labels=c("pd", "d"), las=1)
  axis(2, c(r-c, r), labels=c("r-c", "r"), las=1)
})
with(fit, {
  plot(x, 10^(estimate), type="l", ylab="y", ylim=c(10^(r-c-s1), 10^(r+s2)), xaxt="n", yaxt="n", frame=FALSE)
  lines(x, 10^(lwr), lty=2)
  lines(x, 10^(upr), lty=2)
  axis(1, c(p*d, d), labels=c("pd", "d"), las=1)
  axis(2, c(10^(r-c), exp(r)), labels=expression(10^(r-c), 10^r), las=1)
})

