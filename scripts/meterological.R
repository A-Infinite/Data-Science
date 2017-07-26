
path<-"/home/arushi/My Repositories/Projects/DataScience/DiwaliSmog2016/data/rkPuram/meterological"
setwd(path)


#Load Datasets
rhrk <- read.csv("rhRkPuram.csv")
attach(rhrk)

#Initiate the libraries 
library("lubridate")
library("plyr")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")


#Data Exploration
dim(rhrk)
str(rhrk)

#Check For Missing Values
table(is.na(rhrk))


#Get Summary of all Variables
summary(rhrk)


#date manipulation, for changing the date format
rhrk$Date<- as.Date(rhrk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(rhrk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(rhrk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
rhrk <- cbind(rhrk,Months)


ggplot(rhrk,aes(rhrk$Date,rhrk$Concentration))+ geom_line(size = 1,show.legend = FALSE,color="orchid4") + xlab("Dates") + ylab("%") + ggtitle(" R.K. Puram : Relative Humidity vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(20,90)

detach(rhrk)
#rm(list = ls())

#solarRadiation
#Load Datasets
srrk <- read.csv("srRkPuram.csv")
attach(srrk)


#Data Exploration
dim(srrk)
str(srrk)

#Check For Missing Values
table(is.na(srrk))


#Get Summary of all Variables
summary(srrk)


#date manipulation, for changing the date format
srrk$Date<- as.Date(srrk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(srrk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(srrk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
srrk <- cbind(srrk,Months)


ggplot(srrk,aes(srrk$Date,srrk$Concentration))+ geom_line(size = 1,show.legend = FALSE,color="gold") + xlab("Dates") + ylab("Intensity(w/m2)") + ggtitle(" R.K. Puram : Solar Radiation Intensity vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(10,125)



detach(srrk)
#rm(list = ls())


#Temperature
#Load Datasets
trk <- read.csv("tempRkPuram.csv")
attach(trk)


#Data Exploration
dim(trk)
str(trk)

#Check For Missing Values
table(is.na(trk))


#Get Summary of all Variables
summary(trk)


#date manipulation, for changing the date format
trk$Date<- as.Date(trk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(trk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(trk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
trk <- cbind(trk,Months)


ggplot(trk,aes(trk$Date,trk$Concentration))+ geom_line(size = 1,show.legend = FALSE,color="darkorange") + xlab("Dates") + ylab("°C") + ggtitle(" R.K. Puram : Temperature vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(5,35)

detach(trk)
#rm(list = ls())


#WINDSPEED

#Load Datasets
wsrk <- read.csv("wsRkPuram.csv")
attach(wsrk)


#Data Exploration
dim(wsrk)
str(wsrk)

#Check For Missing Values
table(is.na(wsrk))


#Get Summary of all Variables
summary(wsrk)


#date manipulation, for changing the date format
wsrk$Date<- as.Date(wsrk$Date,format = "%d/%m/%Y")
month(as.POSIXlt(wsrk$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(wsrk$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
wsrk <- cbind(wsrk,Months)


ggplot(wsrk,aes(wsrk$Date,wsrk$Concentration))+ geom_line(size = 1,show.legend = FALSE,color="darkslategray") + xlab("Dates") + ylab("Speed(m/s)") + ggtitle(" R.K. Puram : Wind Speed vs Dates ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(0,2)

detach(wsrk)
#rm(list = ls())


#Combining the graphs ...
combi <- read.csv("combined.csv")
attach(combi)
combi$Date<- as.Date(combi$Date,format = "%d/%m/%Y")
month(as.POSIXlt(combi$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(combi$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)
library(grid)
library(reshape2)
mt <- melt(combi, id = "Date", measure = c("sr", "rh","temp"))
p1<-ggplot(mt, aes(x=mt$Date, y=value, colour = variable)) + geom_line(show.legend = FALSE)+scale_colour_manual(values=c("gold","orchid4","darkorange"))+ylab("Variation")+xlab(" ")


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


p2<-ggplot(wsrk,aes(wsrk$Date,wsrk$Concentration))+ geom_line(size = 1,show.legend = FALSE,color="darkslategray") + xlab("Dates")+ ylab(" Wind Speed(m/s)") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(0,2)

multiplot(p1,p2,cols=1)

