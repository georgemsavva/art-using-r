t <- seq(0,2,l=2001)
z <- 1i*sin(t*pi)+1*cos(t*pi)+exp(983i*pi*t)*(t*6)%%1
x=Re(z)
y=Im(z)
l=max(Mod(z))*1.2
png("temp.png", width=2000, height=2000, type="cairo-png", antialias = "subpixel")
par(mar=c(0,0,0,0), bg="#373737")
plot(NA, axes=FALSE, xlim=c(-l,l), ylim=c(-l,l))
segments(x,y,c(x[-1], NA), 
         c(y[-1], NA), 
         lwd=ifelse(2*((t%%(2/20))>=1/20),2,-1),
         col=rgb(1,1,1,.7))
dev.off()
