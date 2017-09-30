#set the working directory

path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/pollutants"
setwd(path)


#Load Datasets
#pm10rk <- read.csv("pm10rkPuram.csv")

#so2rk <- read.csv("so2rkPuram.csv")
no2rk <- read.csv("no2rkpuram.csv")
library("lubridate")
library("plyr")
dim(no2rk)
str(no2rk)
table(is.na(no2rk))
colSums(is.na(no2rk))
summary(no2rk)
levels(no2rk$Exceeding.Standard...Yes.No.)
levels(no2rk$Exceeding.Standard...Yes.No.)[1] <- 0
levels(no2rk$Exceeding.Standard...Yes.No.)[2] <- 1
#imputing missing values 
#for continuous values

if(any(is.na(no2rk))){
  no2rk$Concentration[is.na(no2rk$Concentration)] <- median(no2rk$Concentration, na.rm = TRUE)
  #imputing values of exceeding standard acc. to imputed Concentration
  p<- no2rk[which(is.na(no2rk$Unit)),][5]
  c<-as.character(p[1,])
  std<- as.numeric(strsplit(c," ")[[1]][1])
  for(i in 1:nrow(no2rk[which(is.na(no2rk$Unit)),][3])){if((no2rk[which(is.na(no2rk$Unit)),][3][i,])>std){no2rk[which(is.na(no2rk$Unit)),][6][i,]<-1} else{no2rk[which(is.na(no2rk$Unit)),][6][i,]<-0}}}


#date manipulation
no2rk$Date<- as.Date(no2rk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(no2rk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(no2rk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)
#plotting
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
ggplot(no2rk,aes(no2rk$Date,  no2rk$Concentration,color=Months))+ geom_point(size = 1) + xlab("Dates") + ylab("Concentration") + ggtitle(" R.K. Puram : no2 Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 80,color="red")+ ylim(0, 200)
