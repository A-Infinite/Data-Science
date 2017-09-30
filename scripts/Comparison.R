path<-"~/Desktop/IT NSIT/6th sem/Project/Data Science/Mandir Marg/Pollutants/Data Sets"
setwd(path)

#PM10
#Load Datasets
pm10mg <- read.csv("pm10MandirMarg.csv")
attach(pm10mg)
dim(pm10mg)
str(pm10mg)

table(is.na(pm10mg))
colSums(is.na(pm10mg))

summary(pm10mg)

#Assigning Boolean Values to the variable -> "Exceeding.Standard...Yes.No."
levels(pm10mg$Exceeding.Standard...Yes.No.)
levels(pm10mg$Exceeding.Standard...Yes.No.)[1] <- 0
levels(pm10mg$Exceeding.Standard...Yes.No.)[2] <- 1

#imputing missing values 
#for continuous values
if(any(is.na(pm10mg))){
  pm10mg$Concentration[is.na(pm10mg$Concentration)] <- median(pm10mg$Concentration, na.rm = TRUE)
  #imputing values of exceeding standard acc. to imputed Concentration
  p<- pm10mg[which(is.na(pm10mg$Unit)),][5]
  c<-as.character(p[1,])
  std<- as.numeric(strsplit(c," ")[[1]][1])
  for(i in 1:nrow(pm10mg[which(is.na(pm10mg$Unit)),][3])){
    if((pm10mg[which(is.na(pm10mg$Unit)),][3][i,])>std){pm10mg[which(is.na(pm10mg$Unit)),][6][i,]<-1} 
    else{pm10mg[which(is.na(pm10mg$Unit)),][6][i,]<-0}}
}

#Adding a Column to show Exceedence Factor
ExceedenceFactor<- pm10mg$Concentration/std
pm10mg<- cbind(pm10mg,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
pm10mg<- cbind(pm10mg,PollutionType)
attach(pm10mg)
#Now,Lets see the summary 
summary(pm10mg)

pm10avgEF<-mean(pm10mg$ExceedenceFactor);

#NO2 Mandir Marg

no2mg <- read.csv("no2MandirMarg.csv")

dim(no2mg)
str(no2mg)
table(is.na(no2mg))
colSums(is.na(no2mg))
summary(no2mg)
levels(no2mg$Exceeding.Standard...Yes.No.)
levels(no2mg$Exceeding.Standard...Yes.No.)[1] <- 0
levels(no2mg$Exceeding.Standard...Yes.No.)[2] <- 1

#Adding a Column to show Exceedence Factor
ExceedenceFactor<- no2mg$Concentration/std
no2mg<- cbind(no2mg,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
no2mg<- cbind(no2mg,PollutionType)
attach(no2mg)
#Now,Lets see the summary 
summary(no2mg)

no2avgEF<-mean(no2mg$ExceedenceFactor);

#SO2
so2mg <- read.csv("so2MandirMarg.csv")

dim(so2mg)
str(so2mg)
table(is.na(so2mg))
colSums(is.na(so2mg))
summary(so2mg)
levels(so2mg$Exceeding.Standard...Yes.No.)
levels(so2mg$Exceeding.Standard...Yes.No.)[1] <- 0
levels(so2mg$Exceeding.Standard...Yes.No.)[2] <- 1

c<-as.character(so2mg$Prescribed.Standard[1])
std<- as.numeric(strsplit(c," ")[[1]][1])

#Adding a Column to show Exceedence Factor
ExceedenceFactor<- so2mg$Concentration/std
so2mg<- cbind(so2mg,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
so2mg<- cbind(so2mg,PollutionType)
attach(so2mg)
#Now,Lets see the summary 
summary(so2mg)
so2avgEF<-mean(so2mg$ExceedenceFactor);

#EF_Place<-data.frame(AvgEF =c(pm10avgEF,no2avgEF,so2avgEF),
#                     Pollutant=c('PM10','NO2','SO2')
#                    )
AvgEF<-c(pm10avgEF,no2avgEF,so2avgEF)
Pollutant<-c('PM10','NO2','SO2')
mat<-table(AvgEF,Pollutant)
class(mat)
barplot(mat,width=5)

