#Correlation Analysis

path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram"
setwd(path)

combinedrk <- read.csv("allPollutantFactorscombined.csv")

combinedrk$Date<- as.Date(combinedrk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(combinedrk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(combinedrk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)
combinedrk <- cbind(combinedrk,Months)

attach(combinedrk)

#combinedrk[,(2:9)]
df<-combinedrk[,(2:9)]

n<-cor(df)
library("corrplot")
n

#d <- data.frame(rh=rnorm(92),ws=rnorm(92),sr=rnorm(92),temp=rnorm(92),no2=rnorm(92),so2=rnorm(92),o3=rnorm(92),pm10=rnorm(92))
#M <- cor(d)

corrplot(n, method = "circle",addCoef.col = "gray")

corrplot.mixed(n)

par(bg="mistyrose")

#Analysis of No2
modno2a<-lm(no2~ws+rh+sr+temp)
summary(modno2a)
attributes(modno2a)
cor(ws,no2)
cor(rh,no2)
cor(no2,temp)
modno2a

#ggplot(combinedrk,aes(ws,no2,color=2))+ geom_point(size = 1,show.legend = FALSE)+geom_abline()+ xlab("Solar Radiation (w/m2)") + ylab("NO2 Concentration(µg/m3)") + ggtitle(" R.K. Puram : NO2 Vs Solar Radiation Intensity ")+ylim(0,200) + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))
pairs(df)
par(mfrow=c(1,1))

md1<-lm(no2~ws)
plot(ws,no2,las=1,col=2,xlab = "wind speed")
abline(md1,col="blue",lty=1,lw=1)

md2<-lm(no2~rh)
plot(rh,no2,las=1,col=2,xlab = "Rel. Humidity")
abline(md2,col="blue",lty=1,lw=1)

md3<-lm(no2~temp)
plot(temp,no2,las=1,col=2,xlab = "Temperature")
abline(md3,col="blue",lty=1,lw=1)

md4<-lm(no2~sr)
plot(sr,no2,las=1,col=2,xlab = "Solar Radiation Intensity")
abline(md4,col="blue",lty=1,lw=1)

#md<-lm(no2~ws)
#summary(md)
#plot(ws,no2,main="NO2 Regression Model",las=1)

#ggplot(combinedrk,aes(ws,no2,color=2))+ geom_point(size = 1,show.legend = FALSE)++geom_smooth(se=FALSE)+geom_abline(aes(slope=-44,intercept=131),data=md)+ xlab("Solar Radiation (w/m2)") + ylab("NO2 Concentration(µg/m3)") + ggtitle(" R.K. Puram : NO2 Vs Solar Radiation Intensity ")+ylim(0,200) + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))


#Analysis of so2
modso2a<-lm(so2~ws+rh+sr+temp)
summary(modso2a)
attributes(modso2a)
cor(ws,so2)
cor(rh,so2)
cor(so2,temp)
cor(so2,sr)
modso2a

#ggplot(combinedrk,aes(ws,no2,color=2))+ geom_point(size = 1,show.legend = FALSE)+geom_abline()+ xlab("Solar Radiation (w/m2)") + ylab("NO2 Concentration(µg/m3)") + ggtitle(" R.K. Puram : NO2 Vs Solar Radiation Intensity ")+ylim(0,200) + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))

md1<-lm(so2~ws)
plot(ws,so2,las=1,col=3,xlab = "wind speed")
abline(md1,col="blue",lty=1,lw=1)

md2<-lm(so2~rh)
plot(rh,so2,las=1,col=3,xlab = "Rel. Humidity")
abline(md2,col="blue",lty=1,lw=1)

md3<-lm(so2~temp)
plot(temp,so2,las=1,col=3,xlab = "Temperature")
abline(md3,col="blue",lty=1,lw=1)

md4<-lm(so2~sr)
plot(sr,so2,las=1,col=3,xlab = "Solar Radiation Intensity")
abline(md4,col="blue",lty=1,lw=1)









#Analysis of O3
modo3a<-lm(o3~ws+rh+sr+temp)
summary(modo3a)
attributes(modo3a)
cor(ws,o3)
cor(rh,o3)
cor(o3,temp)
cor(o3,sr)
modo3a

#ggplot(combinedrk,aes(ws,no2,color=2))+ geom_point(size = 1,show.legend = FALSE)+geom_abline()+ xlab("Solar Radiation (w/m2)") + ylab("NO2 Concentration(µg/m3)") + ggtitle(" R.K. Puram : NO2 Vs Solar Radiation Intensity ")+ylim(0,200) + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))

md1<-lm(o3~ws)
plot(ws,o3,las=1,col=6,xlab = "wind speed")
abline(md1,col="blue",lty=1,lw=1)

md2<-lm(o3~rh)
plot(rh,o3,las=1,col=6,xlab = "Rel. Humidity")
abline(md2,col="blue",lty=1,lw=1)

md3<-lm(o3~temp)
plot(temp,o3,las=1,col=6,xlab = "Temperature")
abline(md3,col="blue",lty=1,lw=1)

md4<-lm(o3~sr)
plot(sr,o3,las=1,col=6,xlab = "Solar Radiation Intensity")
abline(md4,col="blue",lty=1,lw=1)


#Analysis of pm10
modpm10a<-lm(pm10~ws+rh+sr+temp)
summary(modpm10a)
attributes(modpm10a)
cor(ws,pm10)
cor(rh,pm10)
cor(pm10,temp)
cor(pm10,sr)
modpm10a

#ggplot(combinedrk,aes(ws,no2,color=2))+ geom_point(size = 1,show.legend = FALSE)+geom_abline()+ xlab("Solar Radiation (w/m2)") + ylab("NO2 Concentration(µg/m3)") + ggtitle(" R.K. Puram : NO2 Vs Solar Radiation Intensity ")+ylim(0,200) + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))



md1<-lm(pm10~ws)
plot(ws,pm10,las=1,col=9,xlab = "wind speed")
abline(md1,col="blue",lty=1,lw=1)

md2<-lm(pm10~rh)
plot(rh,pm10,las=1,col=9,xlab = "Rel. Humidity")
abline(md2,col="blue",lty=1,lw=1)

md3<-lm(pm10~temp)
plot(temp,pm10,las=1,col=9,xlab = "Temperature")
abline(md3,col="blue",lty=1,lw=1)

md4<-lm(pm10~sr)
plot(sr,pm10,las=1,col=9,xlab = "Solar Radiation Intensity")
abline(md4,col="blue",lty=1,lw=1)





path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/mandirMarg"
setwd(path)

combinedmg <- read.csv("allPollutantFactorsCombinedmg.csv")
attach(combinedmg)
df2<-combinedrk[,(2:5)]
df2<-cbind(df2,pm10)
n2<-cor(df2)
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))

corrplot.mixed(n2)
detach(combinedmg)


attach(combinedrk)
df2<-combinedrk[,(2:5)]
df2<-cbind(df2,pm10)
n2<-cor(df2)
par(mfrow=c(1,1))
par(mar=c(1,1,1,1))

corrplot.mixed(n2)
detach(combinedrk)



