
library(ggplot2)
library(extrafont)
# font_import() uncomment this to import your fonts the first time you use extrafont
loadfonts(device = "win")
dat <- expand.grid(x=1:12, y=1:12)
ggplot(dat, aes(x,y)) + 
  geom_tile(aes(fill=factor(y))) + 
  geom_text(aes(label=x*y, angle= 180-(x+5.5)*360/(24), color=factor(y)),
            family="Comic Sans MS", fontface="bold")+ 
  scale_fill_manual(values=c("lightblue", "black", "purple", "pink", "darkblue", "cyan", "lightgreen", "darkgreen", "yellow","orange", "red", "brown"))+
  scale_color_manual(values=c("black", "white", "pink", "black", "lightblue", "black", "black", "lightgreen", "black","black", "black", "white"))+
  theme_void() + 
  theme(legend.position = "none") + 
  theme(panel.background = element_rect(fill="lightblue", color="lightblue"))+
  coord_polar(start = pi) + 
  scale_x_continuous(limits=c(-5.5,18.5)) + 
  scale_y_continuous(limits=c(-5,13)) + 
  geom_text(x=-5.5,y=-1,label="Ellie's multiplication rainbow", 
            family="Comic Sans MS", fontface="bold", cex=10, col="darkblue")
ggsave("rainbow.png", width=9, height=9)

