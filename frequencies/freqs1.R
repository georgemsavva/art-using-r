
#graphics.off()
#png(filename="temp.png", type="cairo", width=600, height=900)
x <- c(seq(0,2*pi,l=1000),NA)
buffer=0.9
par(mar=c(0,0,0,0))
y <- c(buffer*sin(27*6*x)+2, 
       buffer*sin(3*x)+4,
       buffer*sin(9*x)+6,
       buffer*cos(27*x)+8,
       buffer*cos(27*3*x)+10,
       buffer*cos(27*9*x)+12)
plot(cbind(y,x), axes=FALSE, pch=19, cex=0.5, type="o")
#dev.off()

iris

f1 <- function(x) if(length(x)<10) summary(x) else str(x)
f1(iris)



length(iris)
summary(iris)
