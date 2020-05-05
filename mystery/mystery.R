
png(file="tmp.png",w=1000,h=1000,type="cairo")
par(bg="#000022", mar=c(1,1,1,1))
l=1.6
plot(NA, xlim=c(-l,l), ylim=c(-l,l),axes=F,ann=F)
grid(col="#222244")
t=seq(0,2*pi,l=8)[-1] + 3*pi/2
for(o in seq(0,4*pi,l=1000)){
  x=sin(t+o)
  y=sin(o)*cos(t+o)+cos(o)
  x1=cos(t+1.5*o)
  y1=-sin(o)*sin(t+1.5*o)-cos(o)
  segments(x,y,x1,y1,col=c("#ff999910","#eeeeff20",NA))
  points(c(x[7],x1[7]),c(y[7],y1[7]),pch=19,col=c("#ffaaaa"),cex=.5)
}
dev.off()