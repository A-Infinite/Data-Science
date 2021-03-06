#set the working directory
path<-"/home/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/pollutants"
setwd(path)

#Load Datasets
pm10rk <- read.csv("pm10rkPuram.csv")
attach(pm10rk)

#Import the libraries 
library("lubridate")
library("plyr")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

#Data Exploration
dim(pm10rk)
str(pm10rk)

#Check For Missing Values
table(is.na(pm10rk))
colSums(is.na(pm10rk))

#Get Summary of all Variables
summary(pm10rk)

#Assigning Boolean Values to the variable -> "Exceeding.Standard...Yes.No."
levels(pm10rk$Exceeding.Standard...Yes.No.)
levels(pm10rk$Exceeding.Standard...Yes.No.)[1] <- 0
levels(pm10rk$Exceeding.Standard...Yes.No.)[2] <- 1

#imputing missing values 
#for continuous values
if(any(is.na(pm10rk))){
  pm10rk$Concentration[is.na(pm10rk$Concentration)] <- median(pm10rk$Concentration, na.rm = TRUE)
  #imputing values of exceeding standard acc. to imputed Concentration
  p<- pm10rk[which(is.na(pm10rk$Unit)),][5]
  c<-as.character(p[1,])
  std<- as.numeric(strsplit(c," ")[[1]][1])
  for(i in 1:nrow(pm10rk[which(is.na(pm10rk$Unit)),][3])){
    if((pm10rk[which(is.na(pm10rk$Unit)),][3][i,])>std){pm10rk[which(is.na(pm10rk$Unit)),][6][i,]<-1} 
    else{pm10rk[which(is.na(pm10rk$Unit)),][6][i,]<-0}}
}


#date manipulation, for changing the date format
pm10rk$Date<- as.Date(pm10rk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(pm10rk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(pm10rk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
pm10rk <- cbind(pm10rk,Months)


#We can see the data of a paricular month 
pm10rk[Months=="Oct",]
pm10rk[Months=="Oct" & Concentration<400,] # do some analysis ,add one more column of levels : low, moderate , high ,according to Concentration


#Adding a Column to show Exceedence Factor
ExceedenceFactor<- pm10rk$Concentration/std
pm10rk<- cbind(pm10rk,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
pm10rk<- cbind(pm10rk,PollutionType)
attach(pm10rk)

#Now,Lets see the summary 
summary(pm10rk)


#plotting
par(bg="bisque")

table2<-table(PollutionType,Months)
barplot(table2,beside = T,main="RK Puram : Pollution Category For each Month for PM10",ylab="No.of Days",col=c("sienna1","salmon","orangered1","red3"))
legend(10,30,legend=c("low","moderate","high","critical"),fill=c("sienna1","salmon","orangered1","red3"),bty="n",cex = 0.8)
box()

par(mar=c(7,5,3,3))
boxplot(Concentration~Months*PollutionType,main="RK Puram : Concentration of PM10",ylab="Concentration (µg/m3)",ylim=c(0,1000),las=2,col=c("lightpink1","turquoise1","thistle"),cex.axis = 0.8)

pm10rk[Months=="Oct",]$Concentration
summary(pm10rk[Months=="Oct",]$Concentration)

#line chart
ggplot(pm10rk,aes(pm10rk$Date,  pm10rk$Concentration,color=2))+ geom_line(size = 1,show.legend = FALSE) + xlab("Dates") + ylab("Concentration(µg/m3)") + ggtitle(" R.K. Puram : PM10 Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 100,color="red")+geom_hline(yintercept = 398.3,color="orchid3")+ ylim(0, 1000)+geom_text(aes(as.Date("2016-11-15"),115),cex=4,color=2,fontface="italic",family="arial",label="Prescribed Standard=100")+geom_text(aes(as.Date("2016-10-10"),410),cex=4,color="orchid4",fontface="italic",family="arial",label="Mean=398.3")

detach(pm10rk)

#rm(list = ls())


#****************************************************************************************************************************
#NO2 rKpuram

no2rk <- read.csv("no2rkpuram.csv")

dim(no2rk)
str(no2rk)
table(is.na(no2rk))
colSums(is.na(no2rk))
summary(no2rk)
levels(no2rk$Exceeding.Standard...Yes.No.)
levels(no2rk$Exceeding.Standard...Yes.No.)[1] <- 0
levels(no2rk$Exceeding.Standard...Yes.No.)[2] <- 1



#date manipulation
no2rk$Date<- as.Date(no2rk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(no2rk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(no2rk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)


c<-as.character(no2rk$Prescribed.Standard[1])
std<- as.numeric(strsplit(c," ")[[1]][1])

# Add a new Variable called "Months"
no2rk <- cbind(no2rk,Months)

attach(no2rk)

#We can see the data of a paricular month 
no2rk[Months=="Oct",]
# do some analysis ,add one more column of levels : low, moderate , high ,according to Concentration
no2rk[Months=="Oct" & Concentration<80,] 
#Adding a Column to show Exceedence Factor
ExceedenceFactor<- no2rk$Concentration/std
no2rk<- cbind(no2rk,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
no2rk<- cbind(no2rk,PollutionType)
attach(no2rk)
#Now,Lets see the summary 
summary(no2rk)


#plotting
par(bg="gray91")

table2<-table(PollutionType,Months)
par(mar=c(3,5,3,3))
barplot(table2,beside = T,main="Rk Puram :Pollution Category For each Month For NO2",ylab="No.of Days",col=c("sienna1","salmon","orangered1","red3"),ylim =c(0,25))
legend(5,24,legend=c("low","moderate","high","critical"),fill=c("sienna1","salmon","orangered1","red3"),bty="n",cex = 1)
box()

par(mar=c(5,4,1,0))
boxplot(Concentration~Months*PollutionType,main=" RK Puram :Concentration of NO2",ylab="Concentration (µg/m3)",ylim=c(55,156),las=2,col=c("lightpink1","turquoise1","thistle"),cex.axis = 0.8)

no2rk[Months=="Oct",]$Concentration
summary(no2rk[Months=="Oct",]$Concentration)



ggplot(no2rk,aes(no2rk$Date,no2rk$Concentration,color=2))+ geom_line(size = 1,show.legend = FALSE) + xlab("Dates") + ylab("Concentration(µg/m3)") + ggtitle(" R.K. Puram : NO2 Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 80,color="red")+ ylim(50, 160)+geom_text(aes(as.Date("2016-11-10"),83),cex=3.7,color=2,fontface="italic",family="arial",label="Prescribed Standard=80")+geom_text(aes(as.Date("2016-10-3"),98),cex=4,color="orchid4",fontface="italic",family="arial",label="Mean=95.67")+geom_hline(yintercept =95.67,color="orchid3")

detach(no2rk)
#rm(list = ls())

#****************************************************************************************************************************
#SO2

so2rk <- read.csv("so2rkPuram.csv")
dim(so2rk)
str(so2rk)
table(is.na(so2rk))
colSums(is.na(so2rk))
summary(so2rk)
levels(so2rk$Exceeding.Standard...Yes.No.)
levels(so2rk$Exceeding.Standard...Yes.No.)[1] <- 0
levels(so2rk$Exceeding.Standard...Yes.No.)[2] <- 1

#date manipulation
so2rk$Date<- as.Date(so2rk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(so2rk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(so2rk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

c<-as.character(so2rk$Prescribed.Standard[1])
std<- as.numeric(strsplit(c," ")[[1]][1])

# Add a new Variable called "Months"
so2rk <- cbind(so2rk,Months)
attach(so2rk)

#We can see the data of a paricular month 
so2rk[Months=="Oct",]
so2rk[Months=="Oct" & Concentration<80,] # do some analysis ,add one more column of levels : low, moderate , high ,according to Concentration

#Adding a Column to show Exceedence Factor
ExceedenceFactor<- so2rk$Concentration/std
so2rk<- cbind(so2rk,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
so2rk<- cbind(so2rk,PollutionType)
attach(so2rk)
#Now,Lets see the summary 
summary(so2rk)


#plotting

par(bg="gray91")

table2<-table(PollutionType,Months)
par(mar=c(3,5,3,3))
barplot(table2,beside = T,main="RK Puram :Pollution Category For each Month for SO2",ylab="No.of Days",col=c("sienna1","salmon","orangered1","red3"),ylim =c(0,30))
legend(11.67,30,legend=c("low","moderate","high","critical"),fill=c("sienna1","salmon","orangered1","red3"),bty="n",cex = 1)
box()

par(mar=c(5,4,1,0))
boxplot(Concentration~Months*PollutionType,main="RK Puram : Concentration of SO2",ylab="Concentration (µg/m3)",ylim=c(10,70),las=2,col=c("lightpink1","turquoise1","thistle"),cex.axis = 0.8)

so2rk[Months=="Oct",]$Concentration
summary(so2rk[Months=="Oct",]$Concentration)

ggplot(so2rk,aes(so2rk$Date,so2rk$Concentration,color=2))+ geom_line(size = 1,show.legend = FALSE) + xlab("Dates") + ylab("Concentration(µg/m3)") + ggtitle(" R.K. Puram : SO2 Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 80,color="red")+ ylim(0, 85)+geom_text(aes(as.Date("2016-11-10"),83),cex=3.7,color=2,fontface="italic",family="arial",label="Prescribed Standard=80")+geom_text(aes(as.Date("2016-10-3"),32),cex=4,color="orchid4",fontface="italic",family="arial",label="Mean=29.04")+geom_hline(yintercept =29.04,color="orchid3")

detach(so2rk)
#rm(list = ls())

#****************************************************************************************************************************
#o3
o3rk <- read.csv("o3rkPuram.csv")

dim(o3rk)
str(o3rk)
table(is.na(o3rk))
colSums(is.na(o3rk))
summary(o3rk)
levels(o3rk$Exceeding.Standard...Yes.No.)
levels(o3rk$Exceeding.Standard...Yes.No.)[1] <- 0
levels(o3rk$Exceeding.Standard...Yes.No.)[2] <- 1
#imputing missing values 
#for continuous values
std<- o3rk$Prescribed.Standard[1]

#date manipulation
o3rk$Date<- as.Date(o3rk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(o3rk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(o3rk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)


# Add a new Variable called "Months"
o3rk <- cbind(o3rk,Months)

attach(o3rk)
#We can see the data of a paricular month 
o3rk[Months=="Oct",]
o3rk[Months=="Oct" & Concentration<80,] # do some analysis ,add one more column of levels : low, moderate , high ,according to Concentration


#Adding a Column to show Exceedence Factor
ExceedenceFactor<- o3rk$Concentration/std
o3rk<- cbind(o3rk,ExceedenceFactor)
PollutionType<-cut(ExceedenceFactor,breaks=c(0,0.5,1.0,1.5,10),labels=c("Low","Moderate","High","Critical"),right = F)
o3rk<- cbind(o3rk,PollutionType)
attach(o3rk)
#Now,Lets see the summary 
summary(o3rk)


#plotting
par(bg="gray91")

table2<-table(PollutionType,Months)
par(mar=c(3,5,3,3))
barplot(table2,beside = T,main="RK Puram : Pollution Category For each Month For O3",ylab="No.of Days",col=c("sienna1","salmon","orangered1","red3"),ylim =c(0,30))
legend(12,30,legend=c("low","moderate","high","critical"),fill=c("sienna1","salmon","orangered1","red3"),bty="n",cex = 1)
box()

par(mar=c(5,4,1,0))
boxplot(Concentration~Months*PollutionType,main=" RK Puram : Concentration of O3 ",ylab="Concentration (µg/m3)",ylim=c(0,100),las=2,col=c("lightpink1","turquoise1","thistle"),cex.axis = 0.8)

o3rk[Months=="Oct",]$Concentration
summary(o3rk[Months=="Oct",]$Concentration)



ggplot(o3rk,aes(o3rk$Date,o3rk$Concentration,color=2))+ geom_line(size = 1,show.legend = FALSE) + xlab("Dates") + ylab("Concentration(µg/m3)") + ggtitle(" R.K. Puram : O3 Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 100,color="red")+ ylim(0, 100)+geom_text(aes(as.Date("2016-11-10"),97),cex=3.7,color=2,fontface="italic",family="arial",label="Prescribed Standard=100")+geom_text(aes(as.Date("2016-10-3"),32),cex=4,color="orchid4",fontface="italic",family="arial",label="Mean=29.04")+geom_hline(yintercept =29.04,color="orchid3")

#detach(o3rk)
#rm(list = ls())

ggplot(o3rk,aes(Date,Concentration,color=2))+ geom_line(size = 1,show.legend = TRUE,color="2") + xlab("Dates") + ylab("Concentration(µg/m3)") + ggtitle(" R.K. Puram : Concentration of all Pollutants") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ylim(0,950) +
  geom_line(data=so2rk,color=3)+geom_line(data=no2rk,color=4)+geom_line(data=pm10rk,color=5)

#comparative study of RKPuram and MandirMarg 
#For the following analysis, You will need to run the pollutantAnalysis script of MandirMarg alongside with this script.
par(mar=c(5,4,2,1))

matr<-matrix(c(3.98,3.07,1.19,0.68,0.362,0.141,0.377,0),nrow = 2,byrow = FALSE)
barplot(matr,beside = T,main="Comparative Study of Monitoring Stations",ylab="Average Exceedence Factor",xlab = "Pollutants",col=c("mediumpurple4","seagreen1"),ylim =c(0,5),las=1)
axis(side = 1,at=c(2,5,8,11),labels=c("PM10","NO2","SO2","O3"))
legend(8,4,legend=c("Rk Puram","Mandir Marg"),fill=c("mediumpurple4","seagreen1"),bty="n",cex = 1)
box()
abline(h=0.5,col="paleturquoise4",lty=2,lw=2)
abline(h=1,col="orchid4",lty=2,lw=2)
abline(h=1.5,col="brown4",lty=2,lw=2)
text(x=11.3,y=0.25,adj=0,label="Low",cex=1,col="paleturquoise4")
text(x=10.3,y=0.75,adj=0,label="Moderate",cex=1,col="orchid4")
text(x=11,y=1.25,adj=0,label="High",cex=1,col="brown4")
text(x=10.7,y=2,adj=0,label="Critical",cex=1,col="red")
















