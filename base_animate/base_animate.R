
# Make a movie 

library(gapminder)
data("gapminder")

# Tweening, use the 'approx' function for linear interpolation, ten frames per year
# Make a data frame for each country
interpList <- by(gapminder, gapminder$country, function(d){
  year=seq(1952,2007,0.1)
  lifeExp <- approx(d$year, d$lifeExp, year)$y
  pop <- approx(d$year, d$pop, year)$y
  gdpPercap <- approx(d$year, d$gdpPercap, year)$y
  data.frame(country=d$country[1], continent=d$continent[1], year, lifeExp, pop, gdpPercap)
})
# Now bind the individual countries back into one.
gapminder2 <- do.call(rbind, interpList)



# For each frame, create a png using base R graphics
by(gapminder2, gapminder2$year, function(d){
  d <- d[order(-d$pop),]
  png(filename=sprintf("%stmp%05d.png",tempdir(),d$year2[1]), width=600, height=600,type="cairo")
  plot(NA, xlim=c(200,50000),ylim=c(20,90), log="x",las=1,cex=1.4,
       xlab="GDP per capita ($)",
       ylab="Life expectancy (years)")
  text(x = exp(mean(log(c(200,50000)))),y=55,floor(d$year[1]),adj=0.5,col="grey",cex=15)
  grid()
  points( lifeExp ~ gdpPercap , data=d,
          bg=continent, col="black",
          cex=(pop^(1/3))/100,
          pch=21)
  legend("bottomright",legend=levels(gapminder$continent),col=1:5,pch=20)
  dev.off()
})

# Finally call ffmpeg to make the video.
shell("ffmpeg -y -i tmp%05d.png -c:v libx264 -r 30 -pix_fmt yuv420p out.mp4")


