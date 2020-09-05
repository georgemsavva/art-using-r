
# Discoball

b <- 8 # offset
a <- seq(-1,1,l=1001)
t <- seq(0,2,l=22)
d <- expand.grid(t=t,a=a)
d <-within(d,{z <- 1*(1i*sin(t*pi)+1*cos(t*pi)) +
    -1*a^1*exp(-10i*pi*(t+a+b/200))+
    2*a^1*exp(16i*pi*t)
  w <- (1+0*(a%%.2==0))
  h <- t%%1
  s <- a%%1
  v <- 1
  alpha <- (1-abs(a))^3
})
l <- 1.8
png(filename = "discoball.png",
    width = 2000, 
    height=2000,
    type="cairo")
par(mar=c(0,0,0,0), bg="#000010")
plot(NA, xlim = c(-l, l), ylim=c(-l,l), axes=FALSE, ask=FALSE)
for(ai in a){
  d2 <- subset(d,a==ai)
  d2$x0=Re(d2$z)
  d2$y0=Im(d2$z)
  d2$x1=c(d2$x0[-1],NA)
  d2$y1=c(d2$y0[-1],NA)
  segments(x0 = d2$x0,y0=d2$y0,x1 = d2$x1, y1=d2$y1,
           col=hsv(d2$h, d2$s, d2$v, d2$alpha),lwd=d2$w)
}
dev.off()