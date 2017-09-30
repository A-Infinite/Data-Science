#Correlation

library(ggmap)
#=========map of india========
cities <- data.frame(
  city = c("RkPuram", "Mandir Marg"),
  lat = c(28.5660, 28.37),
  long = c(77.1767, 77.13)
)

delhi <- get_map(location = "Delhi", zoom = 10, maptype = "satellite")

ggmap(delhi) +
  geom_text(data = cities,  aes(x = long, y = lat, label = city), colour = "white") +
  theme_nothing()




path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/pollutants"
setwd(path)


no2rk <- read.csv("no2rkpuram.csv")



path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/meterological"
setwd(path)

wsrk <- read.csv("wsRkPuram.csv")


plot(wsrk$Concentration,no2rk$Concentration,main="NO2 VS Wind Speed",las=1)
cor(wsrk$Concentration,no2rk$Concentration)
mod<-lm(wsrk$Concentration~no2rk$Concentration)
summary(mod)


