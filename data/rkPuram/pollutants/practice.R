#set the working directory

path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/pollutants"
setwd(path)


#Load Datasets
pm10rk <- read.csv("pm10rkPuram.csv")
no2rk <- read.csv("no2rkpuram.csv")
so2rk <- read.csv("so2rkPuram.csv")
o3rk <- read.csv("o3rkPuram.csv")


library("lubridate")
library("plyr")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
explore<-function(dataset){
  dim(dataset)
  str(dataset)
  table(is.na(dataset))
  colSums(is.na(dataset))
  summary(dataset)
  levels(dataset$Exceeding.Standard...Yes.No.)
  levels(dataset$Exceeding.Standard...Yes.No.)[1] <- 0
  levels(dataset$Exceeding.Standard...Yes.No.)[2] <- 1
  #imputing missing values 
  #for continuous values
  dataset$Concentration[is.na(dataset$Concentration)] <- median(dataset$Concentration, na.rm = TRUE)
  #imputing values of exceeding standard acc. to imputed Concentration
  p<- dataset[which(is.na(dataset$Unit)),][5]
  c<-as.character(p[1,])
  std<- as.numeric(strsplit(c," ")[[1]][1])
  for(i in 1:nrow(dataset[which(is.na(dataset$Unit)),][3])){if((dataset[which(is.na(dataset$Unit)),][3][i,])>std){dataset[which(is.na(dataset$Unit)),][6][i,]<-1} else{dataset[which(is.na(dataset$Unit)),][6][i,]<-0}}
#  for(i in 1:nrow(so2rk[which(is.na(so2rk$Unit)),][3])){if((so2rk[which(is.na(so2rk$Unit)),][3][i,])>std){so2rk[which(is.na(so2rk$Unit)),][6][i,]<-1} else{so2rk[which(is.na(so2rk$Unit)),][6][i,]<-0}}}


  #date manipulation
  dataset$Date<- as.Date(dataset$Date,format = "%d/%m/%Y")
  month(as.POSIXlt(dataset$Date, format="%Y/%m/%d"))
  Months<- as.factor(month(as.POSIXlt(dataset$Date, formast="%Y/%m/%d")))
  Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
  levels(Months)
  return(dataset)}

pm10rk<-explore(pm10rk)
so2rk<-explore(so2rk)
no2rk<-explore(no2rk)
o3rk<-explore(o3rk)

  #plotting
  ggplot(dataset,aes(dataset$Date,dataset$Concentration,color=Months))+ geom_point(size = 1) + xlab("Dates") + ylab("Concentration") + ggtitle(" R.K. Puram :  Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 100,color="red")+ ylim(50, 1000)
  #return(dataset)


  