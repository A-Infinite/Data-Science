#set the working directory
path<-"~/Desktop/Data Science/Mandir Marg/Pollutants/Data Sets"
setwd(path)

#PM10 Mandir Marg
#Load Datasets
pm10mg <- read.csv("pm10MandirMarg.csv")
attach(pm10mg)

#Import the libraries 
library("lubridate")
library("plyr")
library("ggplot2")

#Data Exploration
dim(pm10mg)
str(pm10mg)

#Check For Missing Values
table(is.na(pm10mg))
colSums(is.na(pm10mg))

#Get Summary of all Variables
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

#date manipulation, for changing the date format
pm10mg$Date<- as.Date(pm10mg$Date,format = "%d/%m/%Y")
month(as.POSIXlt(pm10mg$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(pm10mg$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
pm10mg <- cbind(pm10mg,Months)

#We can see the data of a paricular month 
pm10mg[Months=="Oct",]
pm10mg[Months=="Oct" & Concentration<400,] # do some analysis ,add one more column of levels : low, moderate , high ,according to Concentration

#Adding a Column to show Exceedence Factor
ExceedenceFactor<- pm10mg$Concentration/std
pm10mg<- cbind(pm10mg,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
pm10mg<- cbind(pm10mg,PollutionType)
attach(pm10mg)
#Now,Lets see the summary 
summary(pm10mg)

#plotting
par(bg="bisque")

table2<-table(PollutionType,Months)
barplot(table2,beside = T,main="Mandir Marg: Pollution Category For each Month for PM10",ylab="No.of Days",col=c("sienna1","salmon","orangered1","red3"))
legend(5,30,legend=c("low","moderate","high","critical"),fill=c("sienna1","salmon","orangered1","red3"),bty="n",cex = 0.6)
box()

par(mar=c(7,5,3,3))
boxplot(Concentration~Months*PollutionType,main="Mandir Marg: Concentration of PM10",ylab="Concentration (µg/m3)",ylim=c(0,1000),las=2,col=c("lightpink1","turquoise1","thistle"),cex.axis = 0.8)

pm10mg[Months=="Oct",]$Concentration
summary(pm10mg[Months=="Oct",]$Concentration)

#Line Chart
ggplot(pm10mg,aes(pm10mg$Date,pm10mg$Concentration,color=2))+ geom_line(size = 1,show.legend = FALSE) + xlab("Months") + ylab("Concentration(µg/m3)") + ggtitle(" Mandir Marg : PM10 Concentration vs Months ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 100,color="red")+geom_hline(yintercept =mean(pm10mg$Concentration),color="orchid3")+ ylim(0, 1000)+geom_text(aes(as.Date("2016-11-15"),124),cex=4,color="red",fontface="italic",label="Prescribed Standard=100")+geom_text(aes(as.Date("2016-10-15"),325),cex=4,color="orchid4",fontface="italic",label="Mean=307.34")

detach(pm10mg)
#rm(list = ls())

#******************************************************************************************************************************
#NO2 Mandir Marg

#Load File
no2mg <- read.csv("no2MandirMarg.csv")

#Data Exploration
dim(no2mg)
str(no2mg)
table(is.na(no2mg))
colSums(is.na(no2mg))
summary(no2mg)
levels(no2mg$Exceeding.Standard...Yes.No.)
levels(no2mg$Exceeding.Standard...Yes.No.)[1] <- 0
levels(no2mg$Exceeding.Standard...Yes.No.)[2] <- 1

#date manipulation
no2mg$Date<- as.Date(no2mg$Date,format = "%d/%m/%Y")
month(as.POSIXlt(no2mg$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(no2mg$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
no2mg <- cbind(no2mg,Months)

attach(no2mg)

#We can see the data of a paricular month 
no2mg[Months=="Oct",]
no2mg[Months=="Oct" & Concentration<80,] # do some analysis ,add one more column of levels : low, moderate , high ,according to Concentration

#Adding a Column to show Exceedence Factor
ExceedenceFactor<- no2mg$Concentration/std
no2mg<- cbind(no2mg,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
no2mg<- cbind(no2mg,PollutionType)
attach(no2mg)

#Now,Lets see the summary 
summary(no2mg)

#plotting
par(bg="gray91")

table2<-table(PollutionType,Months)
par(mar=c(3,5,3,3))
barplot(table2,beside = T,main="Mandir Marg: Pollution Category For each Month for NO2",ylab="No.of Days",col=c("sienna1","salmon","orangered1","red3"),ylim =c(0,30))
legend(5,29,legend=c("low","moderate","high","critical"),fill=c("sienna1","salmon","orangered1","red3"),bty="n",cex =0.7)
box()

par(mar=c(5,4,1,0))
boxplot(Concentration~Months*PollutionType,main="Mandir Marg: Concentration of NO2",ylab="Concentration (µg/m3)",ylim=c(20,140),las=2,col=c("lightpink1","turquoise1","thistle"),cex.axis = 0.8)

no2mg[Months=="Oct",]$Concentration
summary(no2mg[Months=="Oct",]$Concentration)

#Line Charts
ggplot(no2mg,aes(no2mg$Date,no2mg$Concentration,color=2))+ geom_line(size = 1,show.legend = FALSE) + xlab("Months") + ylab("Concentration(µg/m3)") + ggtitle(" Mandir Marg : NO2 Concentration vs Months ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 80,color="red")+ ylim(0, 140)+geom_text(aes(as.Date("2016-10-13"),83),cex=3,color=2,fontface="italic",label="Prescribed Standard=80")+geom_hline(yintercept =mean(no2mg$Concentration),color="orchid3")+geom_text(aes(as.Date("2016-10-15"),71),cex=4,color="orchid4",fontface="italic",label="Mean=68.50")

detach(no2mg)
#rm(list = ls())

#******************************************************************************************************************************
#SO2 Mandir Marg

#Load File
so2mg <- read.csv("so2MandirMarg.csv")

#Data Exploration
dim(so2mg)
str(so2mg)
table(is.na(so2mg))
colSums(is.na(so2mg))
summary(so2mg)
levels(so2mg$Exceeding.Standard...Yes.No.)
levels(so2mg$Exceeding.Standard...Yes.No.)[1] <- 0
levels(so2mg$Exceeding.Standard...Yes.No.)[2] <- 1

#date manipulation
so2mg$Date<- as.Date(so2mg$Date,format = "%d/%m/%Y")
month(as.POSIXlt(so2mg$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(so2mg$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)


c<-as.character(so2mg$Prescribed.Standard[1])
std<- as.numeric(strsplit(c," ")[[1]][1])

# Add a new Variable called "Months"
so2mg <- cbind(so2mg,Months)

attach(so2mg)

#We can see the data of a paricular month 
so2mg[Months=="Oct",]
so2mg[Months=="Oct" & Concentration<80,] # do some analysis ,add one more column of levels : low, moderate , high ,according to Concentration

#Adding a Column to show Exceedence Factor
ExceedenceFactor<- so2mg$Concentration/std
so2mg<- cbind(so2mg,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
so2mg<- cbind(so2mg,PollutionType)
attach(so2mg)

#Now,Lets see the summary 
summary(so2mg)

#plotting
par(bg="gray91")

table2<-table(PollutionType,Months)
par(mar=c(3,5,3,3))
barplot(table2,beside = T,main="Mandir Marg: Pollution Category For each Month for SO2",ylab="No.of Days",col=c("sienna1","salmon","orangered1","red3"),ylim =c(0,32))
legend(11.67,30,legend=c("low","moderate","high","critical"),fill=c("sienna1","salmon","orangered1","red3"),bty="n",cex = 1)
box()

par(mar=c(5,4,1,0))
boxplot(Concentration~Months*PollutionType,main="Mandir Marg: Concentration of SO2",ylab="Concentration (µg/m3)",ylim=c(0,35),las=2,col=c("lightpink1","turquoise1","thistle"),cex.axis = 0.8)

so2mg[Months=="Dec",]$Concentration
summary(so2mg[Months=="Dec",]$Concentration)

#Line Charts
ggplot(so2mg,aes(so2mg$Date,so2mg$Concentration,color=2))+ geom_line(size = 1,show.legend = FALSE) + xlab("Months") + ylab("Concentration(µg/m3)") + ggtitle(" Mandir Marg : SO2 Concentration vs Months") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 80,color="red")+ ylim(0, 85)+geom_text(aes(as.Date("2016-11-10"),83),cex=3.7,color=2,fontface="italic",label="Prescribed Standard=80")+geom_hline(yintercept =mean(so2mg$Concentration),color="orchid3")+geom_text(aes(as.Date("2016-10-15"),15),cex=4,color="orchid4",fontface="italic",label="Mean=11.33")

detach(so2mg)
#rm(list = ls())


ggplot(pm10mg,aes(Date,Concentration,color=5))+ geom_line(size = 1,color="5") + xlab("Dates") + ylab("Concentration(µg/m3)") + ggtitle(" Mandir Marg : Concentration of all Pollutants") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ylim(0,950) +
  geom_line(data=so2mg,color=3)+geom_line(data=no2mg,color=4)+scale_colour_manual(values=c("5","3","4"))+theme_bw()+theme(legend.position="bottom")+geom_hline(yintercept = 80,color="red")+geom_hline(yintercept = 100,color="black")
