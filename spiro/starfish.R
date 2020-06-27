
res=1000 # resolution
a=seq(0,0.2,l=200) # 200 = number of frames
pl=length(a)
t = seq(0,100,l=50000)

d = expand.grid(ai=1:pl,ti=1:50000) # d will hold data for all frames
d$t = t[d$ti]
d$a = a[d$ai]

# f is the function for the graph.
f <- function(t,a)  (2*exp(1i*2*pi*t) + .4*exp(-(2+1*(.01))*1i*4*pi*t) + 
     .2*exp(11*1i*2*pi*(t+a*10/5.5)))*(sin(t*20*pi)+3)^.2*exp(1i*2*a*pi)*.99^t
d$z=f(d$t,d$a) # make calculation for all frames at once.
l=max(Mod(d$z))*1.2 # plot limits
png_path <- file.path(tempdir(), "frame%03d.png")
png(png_path, type = "cairo-png",width=res,height=res, antialias = "subpixel")
par(ask = FALSE,mar=c(0,0,0,0), bg="#2B172E")
print(paste0("printing ",pl," frames."))
for (j in 1:pl) {
  plot(NA,xlim=c(lmin,-l), ylim=c(lmin,-l),axes=F, ann=F)
  d2 <- d[d$ai==j,] # get the data for the jth frame
  x0 <- Re(d2$z) # get segment coordinates.
  y0 <- Im(d2$z) 
  x1 <- c(Re(d2$z)[-1], NA)
  y1 <- c(Im(d2$z)[-1],NA)
  segments(x0,y0,x1,y1, col = hsv(.6, sin(t/100)^2, 1, .5), lwd=2)
  print(j)
}
dev.off()
png_files <- sprintf(png_path, 1:pl)
gif_file <- "starfish.gif"
gifski(png_files, "starfish.gif",delay = 0.033, width = res,height=res)
unlink(png_files)

## you need ffmpeg installed for this.
shell("ffmpeg -f gif -i starfish.gif starfish.mp4")
