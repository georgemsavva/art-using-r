
## Spinning 7-gram construction
n <- 100
N = 7
s = 3
starpoints <- exp((0:(N-1))*2*pi*1i/N) * exp(1i*pi/2)
vector <- c(seq(starpoints[2], starpoints[(1+1*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+s)%%N+1], starpoints[(1+2*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+2*s)%%N+1], starpoints[(1+3*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+3*s)%%N+1], starpoints[(1+4*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+4*s)%%N+1], starpoints[(1+5*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+5*s)%%N+1], starpoints[(1+6*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+6*s)%%N+1], starpoints[(1+7*s)%%N+1], l=n)[-c(1,n)]
)
gft <- function(vector, n=0){
  dt <- 1/length(vector)
  t <- (1:length(vector))*dt
  sum( vector * exp(-2i*pi*n*t)*dt )
}
components <- cbind(-100:100,sapply(-100:100, function(j) gft(vector,j)))
components <- components[order(-abs(components[,2])),]

circle <- function(x,y,r,l=1000,col="red", border="white"){
  t <- seq(0,2*pi,l=l)
  polygon(cbind(x+r*sin(t), y+r*cos(t)), col=col, border=border)
}
getlines <- function(t) {
  cumsum(c(0,exp(components[,1]*1i*t)*components[,2]))
}
makegrid <- function(col,rot){
  z=1.1*c(-1i+-1,1i+-1,1i+1,-1i+1,-1i+-1 )*rotfactor
  polygon(x=Re(z), y=Im(z), border="black", col="white")
  zstart = (seq(-1,1,.2) + 1i*rep(-1,11))*rotfactor
  zend = (seq(-1,1,.2) + 1i*rep(1,11))*rotfactor
  z2start = (1i*seq(-1,1,.2) + rep(-1,11))*rotfactor
  z2end = (1i*seq(-1,1,.2) + rep(1,11))*rotfactor
  segments(Re(zstart), Im(zstart), Re(zend), Im(zend), lty=2, col="lightblue")
  segments(Re(z2start), Im(z2start), Re(z2end), Im(z2end), lty=2, col="lightblue")
}
limit = 1.3
split =1
plotted = NA
plotted2 = NA
hue2=NA
p=0
for(i in c(seq(0,1,l=1000),rep(1,50))){
  rotfactor <-  exp(-6i*pi*((sin(i*pi-pi/2)+1)))
  #rotfactor <- 1
p=p+1
png(file=sprintf("c:/work/ismallspiral%05d.png",p),width=800,height=800, type="cairo")
par(bg="#222255", mar=c(0,0,0,0))  
plot(NA, type="l",xlim=c(-limit,limit), ylim=c(-limit,limit), axes = FALSE, ann=F)
makegrid(col="lightblue")
lines(plotted*rotfactor, pch=19, cex=.4, col=rgb(.2,0,.2,1), lwd=20)
lines(plotted2*rotfactor, pch=19, cex=.4, col=rgb(1,1,.5,1), lwd=10)


for(j in 0:(split-1)){
  pointsToPlot <- getlines(2*pi*((sin(i*pi-pi/2)+1))+(2*j*pi/6))
  if(i<0.5)plotted <- c(plotted, pointsToPlot[100])
  if(i>0.5){
    plotted2 <- c(plotted2, pointsToPlot[100])
    hue2 <- c(hue2, 2*(i-0.5))
    }
  points(pointsToPlot[1:20] *rotfactor, type="o", col=hsv(0,1,.3), pch=19)
}
dev.off()
}
graphics.off()


shell("rm output4.mp4")
shell("ffmpeg -framerate 40 -i  c:/work/ismallspiral%05d.png -c:v libx264 -vf \"format=yuv420p\" -loop 0 output4.mp4")
shell("rm c:/work/ismallspiral*.png")
shell("output4.mp4")

