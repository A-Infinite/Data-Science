#set the working directory

path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/pollutants"
setwd(path)


#Load Datasets
#pm10rk <- read.csv("pm10rkPuram.csv")

so2rk <- read.csv("so2rkPuram.csv")

library("lubridate")
library("plyr")
dim(so2rk)
str(so2rk)
table(is.na(so2rk))
colSums(is.na(so2rk))
summary(so2rk)
levels(so2rk$Exceeding.Standard...Yes.No.)
levels(so2rk$Exceeding.Standard...Yes.No.)[1] <- 0
levels(so2rk$Exceeding.Standard...Yes.No.)[2] <- 1
#imputing missing values 
#for continuous values

if(any(is.na(so2rk))){
  so2rk$Concentration[is.na(so2rk$Concentration)] <- median(so2rk$Concentration, na.rm = TRUE)
  #imputing values of exceeding standard acc. to imputed Concentration
  p<- so2rk[which(is.na(so2rk$Unit)),][5]
  c<-as.character(p[1,])
  std<- as.numeric(strsplit(c," ")[[1]][1])
  for(i in 1:nrow(so2rk[which(is.na(so2rk$Unit)),][3])){if((so2rk[which(is.na(so2rk$Unit)),][3][i,])>std){so2rk[which(is.na(so2rk$Unit)),][6][i,]<-1} else{so2rk[which(is.na(so2rk$Unit)),][6][i,]<-0}}
}
#date manipulation
so2rk$Date<- as.Date(so2rk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(so2rk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(so2rk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)
#plotting
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
ggplot(so2rk,aes(so2rk$Date,  so2rk$Concentration,color=Months))+ geom_point(size = 1) + xlab("Dates") + ylab("Concentration") + ggtitle(" R.K. Puram : so2 Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 80,color="red")+ ylim(0, 100)
