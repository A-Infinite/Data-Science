#set the working directory

path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/pollutants"
setwd(path)


#Load Datasets
#pm10rk <- read.csv("pm10rkPuram.csv")

#so2rk <- read.csv("so2rkPuram.csv")
o3rk <- read.csv("o3rkPuram.csv")
library("lubridate")
library("plyr")
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

if(any(is.na(o3rk))){
  o3rk$Concentration[is.na(o3rk$Concentration)] <- median(o3rk$Concentration, na.rm = TRUE)
  #imputing values of exceeding standard acc. to imputed Concentration
  for(i in 1:nrow(o3rk[which(is.na(o3rk$Unit)),][3])){if((o3rk[which(is.na(o3rk$Unit)),][3][i,])>std){o3rk[which(is.na(o3rk$Unit)),][6][i,]<-1} else{o3rk[which(is.na(o3rk$Unit)),][6][i,]<-0}}
}

#date manipulation
o3rk$Date<- as.Date(o3rk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(o3rk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(o3rk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)
#plotting
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
ggplot(o3rk,aes(o3rk$Date,  o3rk$Concentration,color=Months))+ geom_point(size = 1) + xlab("Dates") + ylab("Concentration") + ggtitle(" R.K. Puram : o3 Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 100,color="red")+ ylim(0, 120)
