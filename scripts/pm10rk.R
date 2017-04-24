#set the working directory

path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/pollutants"
setwd(path)


#Load Datasets
pm10rk <- read.csv("pm10rkPuram.csv")



library("lubridate")
library("plyr")
dim(pm10rk)
str(pm10rk)
table(is.na(pm10rk))
colSums(is.na(pm10rk))
summary(pm10rk)
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
  for(i in 1:nrow(pm10rk[which(is.na(pm10rk$Unit)),][3])){if((pm10rk[which(is.na(pm10rk$Unit)),][3][i,])>std){pm10rk[which(is.na(pm10rk$Unit)),][6][i,]<-1} else{pm10rk[which(is.na(pm10rk$Unit)),][6][i,]<-0}}
}
#date manipulation
pm10rk$Date<- as.Date(pm10rk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(pm10rk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(pm10rk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)
#plotting
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
ggplot(pm10rk,aes(pm10rk$Date,  pm10rk$Concentration,color=4))+ geom_line(size = 1,show.legend =F) + xlab("Dates") + ylab("Concentration") + ggtitle(" R.K. Puram : PM10 Concentration vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+geom_hline(yintercept = 80,color="red")+ ylim(0, 1000)


+annotate("text",label="Prescribed Standard =100",x=5,y=103,size=2,color="red")text(x=5,y=103,adj=0,label ="Prescribed Standard",cex=0.65,col=2)
