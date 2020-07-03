
library(gganimate)
library(Cairo)

## Making a harmonograph with gganimate
N=500
t <- seq(0,4*pi,l=N)

## Set the pendulum frequencies and phase
x1 <- sin(5*t)+3
x2 <- sin(1*t+pi/2) # x2 needs a phase shift to draw the central circle.
x3 <- sin(3*t)-3
y1 <- sin(4*t)+3
y2 <- sin(1*t)
y3 <- sin(2*t)-3

# combine all the point data together for plotting
dat <- data.frame(t=rep(t,9),
           x=rep(c(x1,x2,x3),3),
           y=c(y1,y1,y1,y2,y2,y2,y3,y3,y3),
           x0=4.5, y0=4.5)

g <- ggplot(dat,aes(x,y)) + 
  geom_point(color="darkblue",aes(group=seq_along(t))) + 
  geom_point(aes(y=y0)) + 
  geom_point(aes(x=x0)) +
  geom_point(aes(x=-x0)) +
  geom_point(aes(y= -y0)) +
  scale_x_continuous(breaks=-5:5) + 
  scale_y_continuous(breaks=-5:5)+
  geom_segment(aes(x=-x0,xend=x0,yend=y), col="red") + 
  geom_segment(aes(y=-y0,yend=y0,xend=x), col="red") + 
  theme_bw()+coord_fixed()+
  theme(panel.background =  element_rect(fill="#ffffee") ,
      plot.background =element_rect(fill="#ffffee"),
      plot.margin = margin(2,2,2,2, "cm"))

  g <- g +  transition_time(t) + shadow_mark(exclude_layer = c(2,3,4,5,6,7), color="#111144")
  animate(g,nframes=N,fps=30, type="cairo", width=600,height=600,end_pause = 60)
