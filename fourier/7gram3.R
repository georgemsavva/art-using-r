
## Spinning 7-gram construction
n <- 200
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
s = 2
vector2g <- c(seq(starpoints[2], starpoints[(1+1*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+s)%%N+1], starpoints[(1+2*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+2*s)%%N+1], starpoints[(1+3*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+3*s)%%N+1], starpoints[(1+4*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+4*s)%%N+1], starpoints[(1+5*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+5*s)%%N+1], starpoints[(1+6*s)%%N+1], l=n)[-c(1,n)],
            seq(starpoints[(1+6*s)%%N+1], starpoints[(1+7*s)%%N+1], l=n)[-c(1,n)]
)
#vector2g <- c(vector, vector2g)
s = 1
vector3g <- c(seq(starpoints[2], starpoints[(1+1*s)%%N+1], l=n)[-c(1,n)],
              seq(starpoints[(1+s)%%N+1], starpoints[(1+2*s)%%N+1], l=n)[-c(1,n)],
              seq(starpoints[(1+2*s)%%N+1], starpoints[(1+3*s)%%N+1], l=n)[-c(1,n)],
              seq(starpoints[(1+3*s)%%N+1], starpoints[(1+4*s)%%N+1], l=n)[-c(1,n)],
              seq(starpoints[(1+4*s)%%N+1], starpoints[(1+5*s)%%N+1], l=n)[-c(1,n)],
              seq(starpoints[(1+5*s)%%N+1], starpoints[(1+6*s)%%N+1], l=n)[-c(1,n)],
              seq(starpoints[(1+6*s)%%N+1], starpoints[(1+7*s)%%N+1], l=n)[-c(1,n)]
)

vector <- rep(vector, 2)
vector2g <- rep(vector2g, 3)
vector3g <- rep(vector3g, 6)

gft <- function(vector, n=0,scale=1){
  dt <- 1/length(vector)
  t <- (1:length(vector))*dt
  sum( vector * exp(-2i*scale*pi*n*t)*dt*scale )
}
detail <- 100
components <- cbind(-detail:detail,sapply(-detail:detail, function(j) gft(vector,j)))
components <- components[order(-abs(components[,2])),]

components2 <- cbind(-detail:detail,sapply(-detail:detail, function(j) gft(vector2g,j,scale=1)))
components2 <- components2[order(-abs(components2[,2])),]

components3 <- cbind(-detail:detail,sapply(-detail:detail, function(j) gft(vector3g,j,scale=1)))
components3 <- components3[order(-abs(components3[,2])),]


circle <- function(x,y,r,l=1000,col="red", border="white"){
  t <- seq(0,2*pi,l=l)
  polygon(cbind(x+r*sin(t), y+r*cos(t)), col=col, border=border)
}
getlines <- function(t, comps,scale=1) {
  cumsum(c(0,exp(comps[,1]*1i*t)*comps[,2]))
}

makegrid <- function(col,rot){
  z=1.3*c(-1i+-1,1i+-1,1i+1,-1i+1,-1i+-1 )*rotfactor
  polygon(x=Re(z), y=Im(z), border="black", col="#fdfdfd")
  zstart = (seq(-1,1,.2) + 1i*rep(-1,11))*rotfactor
  zend = (seq(-1,1,.2) + 1i*rep(1,11))*rotfactor
  z2start = (1i*seq(-1,1,.2) + rep(-1,11))*rotfactor
  z2end = (1i*seq(-1,1,.2) + rep(1,11))*rotfactor
  segments(Re(zstart), Im(zstart), Re(zend), Im(zend), lty=2, col="darkblue")
  segments(Re(z2start), Im(z2start), Re(z2end), Im(z2end), lty=2, col="darkblue")
}
limit = 1.5
split =1
plotted = NA
plotted2 = NA
plotted3 = NA
plotted4 = NA
plotted5 = NA
plotted6 = NA
hue2=1
p=0
for(i in c(seq(0,1,l=3000),rep(1,100))){
  rotfactor <-  exp(-12i*pi*((sin(i*pi-pi/2)+1)))
  #rotfactor <- 1
p=p+1
png(file=sprintf("c:/work/ismallspiral%05d.png",p),width=800,height=800, type="cairo")
par(mar=c(0,0,0,0), bg="#fdfdfd")  
plot(NA, type="l",xlim=c(-limit,limit), ylim=c(-limit,limit), axes = FALSE, ann=F)
makegrid(col="lightblue")
lines(plotted*rotfactor, pch=19, cex=.4, col=rgb(.2,0,.2,1), lwd=8)
lines(plotted3*rotfactor, pch=19, cex=.4, col=rgb(.2,0,.2,1), lwd=8)
lines(plotted5*rotfactor, pch=19, cex=.4, col=rgb(.2,0,.2,1), lwd=8)
z=plotted2*rotfactor
segments(x0 = Re(z), y0 = Im(z),x1 = Re(c(z[-1],NA)), y1=Im(c(z[-1],NA)), pch=19, cex=.4, col=hsv(0,.7,1,.5), lwd=4)
z=plotted4*rotfactor
segments(x0 = Re(z), y0 = Im(z),x1 = Re(c(z[-1],NA)), y1=Im(c(z[-1],NA)), pch=19, cex=.4, col=hsv(.2,.7,1,.5), lwd=4)
z=plotted6*rotfactor
segments(x0 = Re(z), y0 = Im(z),x1 = Re(c(z[-1],NA)), y1=Im(c(z[-1],NA)), pch=19, cex=.4, col=hsv(.7,.7,.8,.5), lwd=4)


for(j in 0:(split-1)){
  pointsToPlot <- getlines(2*pi*((sin(i*pi-pi/2)+1))+(2*j*pi/6), components)* exp(-1i*pi*pi/4)
  if(i<0.5)plotted <- c(plotted, pointsToPlot[100])
  if(i>0.5){
    plotted2 <- c(plotted2, pointsToPlot[100])
    hue2 <- c(hue2, 2*(i-0.5))
    }
  points(pointsToPlot[1:20] *rotfactor, type="o", col="black", pch=19, cex=1.2, lwd=4)
  points(pointsToPlot[1:20] *rotfactor, type="o", col="white", pch=19,cex=.7)
}
for(j in 0:(split-1)){
  pointsToPlot <- getlines(2*pi*((sin(i*pi-pi/2)+1))+(2*j*pi/6), components2) * exp(-1i*pi*pi/4)
  if(i<0.5)plotted3 <- c(plotted3, pointsToPlot[100])
  if(i>0.5){
    plotted4 <- c(plotted4, pointsToPlot[detail])
    #hue2 <- c(hue2, 2*(i-0.5))
    }
  points(pointsToPlot[1:detail] *rotfactor, type="o", col="black", pch=19, cex=1.2, lwd=4)
  points(pointsToPlot[1:detail] *rotfactor, type="o", col="white", pch=19,cex=.7)
  
}
for(j in 0:(split-1)){
  pointsToPlot <- getlines(2*pi*((sin(i*pi-pi/2)+1))+(2*j*pi/6), components3)* exp(-1i*pi*pi/4)
  if(i<0.5)plotted5 <- c(plotted5, pointsToPlot[100])
  if(i>0.5){
    plotted6 <- c(plotted6, pointsToPlot[100])
    #hue2 <- c(hue2, 2*(i-0.5))
  }
  points(pointsToPlot[1:100] *rotfactor, type="o", col="black", pch=19, cex=1.2, lwd=4)
  points(pointsToPlot[1:100] *rotfactor, type="o", col="white", pch=19,cex=.7)
  
}

dev.off()
}
graphics.off()


shell("rm output4.mp4")
shell("ffmpeg -framerate 60 -i  c:/work/ismallspiral%05d.png -c:v libx264 -vf \"format=yuv420p\" -loop 0 output4.mp4")
shell("rm c:/work/ismallspiral*.png")
shell("output4.mp4")

