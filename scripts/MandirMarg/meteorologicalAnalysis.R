#Set Working Directory
path<-"~/Desktop/Data Science/Mandir Marg/Meterological/Data Sets"
setwd(path)

#RELATIVE HUMIDITY
#Load Datasets
rhmg <- read.csv("rhMandirMarg.csv")
attach(rhmg)

#Import the libraries 
library("lubridate")
library("plyr")
library("ggplot2")

#Data Exploration
dim(rhmg)
str(rhmg)

#Check For Missing Values
table(is.na(rhmg))

#Get Summary of all Variables
summary(rhmg)

#date manipulation, for changing the date format
rhmg$Date<- as.Date(rhmg$Date,format = "%d/%m/%Y")
month(as.POSIXlt(rhmg$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(rhmg$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
rhmg <- cbind(rhmg,Months)

#Line Chart
ggplot(rhmg,aes(rhmg$Date,rhmg$Concentration))+ geom_line(size = 1,show.legend = FALSE,color="orchid4") + xlab("Months") + ylab("%") + ggtitle(" Mandir Marg : Relative Humidity vs Months ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(20,90)

detach(rhmg)
#rm(list = ls())

#*********************************************************************************************************************************************
#SOLAR RADIATION
#Load Datasets
srmg <- read.csv("srMandirMarg.csv")
attach(srmg)

#Data Exploration
dim(srmg)
str(srmg)

#Check For Missing Values
table(is.na(srmg))

#Get Summary of all Variables
summary(srmg)

#date manipulation, for changing the date format
srmg$Date<- as.Date(srmg$Date,format = "%d/%m/%Y")
month(as.POSIXlt(srmg$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(srmg$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
srmg <- cbind(srmg,Months)

#Line Chart
ggplot(srmg,aes(srmg$Date,srmg$Concentration))+ geom_line(size = 1,show.legend = FALSE,color="gold") + xlab("Months") + ylab("Intensity(w/m2)") + ggtitle(" Mandir Marg : Solar Radiation Intensity vs Months ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(10,125)

detach(srmg)
#rm(list = ls())

#*******************************************************************************************************************************************
#TEMPERATURE
#Load Datasets
tmg <- read.csv("tempMandirMarg.csv")
attach(tmg)

#Data Exploration
dim(tmg)
str(tmg)

#Check For Missing Values
table(is.na(tmg))

#Get Summary of all Variables
summary(tmg)

#date manipulation, for changing the date format
tmg$Date<- as.Date(tmg$Date,format = "%d/%m/%Y")
month(as.POSIXlt(tmg$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(tmg$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
tmg <- cbind(tmg,Months)

#Line Chart
ggplot(tmg,aes(Date,Concentration))+ geom_line(size = 1,show.legend = FALSE,color="darkorange") + xlab("Months") + ylab("°C") + ggtitle(" Mandir Marg : Temperature vs Months ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(5,35)

detach(tmg)
#rm(list = ls())

#*******************************************************************************************************************************************
#WINDSPEED
#Load Datasets
wsmg <- read.csv("wsMandirMarg.csv")
attach(wsmg)

#Data Exploration
dim(wsmg)
str(wsmg)

#Check For Missing Values
table(is.na(wsmg))

#Get Summary of all Variables
summary(wsmg)

#date manipulation, for changing the date format
wsmg$Date<- as.Date(wsmg$Date,format = "%d/%m/%Y")
month(as.POSIXlt(wsmg$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(wsmg$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)

# Add a new Variable called "Months"
wsmg <- cbind(wsmg,Months)

#Line Chart
ggplot(wsmg,aes(wsmg$Date,wsmg$Concentration))+ geom_line(size = 1,show.legend = FALSE,color="darkslategray") + xlab("Months") + ylab("Speed(m/s)") + ggtitle(" Mandir Marg : Wind Speed vs Months ") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(0,2.7)

detach(wsmg)
#rm(list = ls())

#*******************************************************************************************************************************************
#COMBINING THE GRAPHS ...
combi <- read.csv("allFactorsCombined.csv")
attach(combi)
combi$Date<- as.Date(combi$Date,format = "%d/%m/%Y")
month(as.POSIXlt(combi$Date, format="%Y/%m/%d"))
Months<- as.factor(month(as.POSIXlt(combi$Date, formast="%Y/%m/%d")))
Months<-mapvalues(Months, from = c(10,11,12), to = c("Oct","Nov","Dec"))
levels(Months)
library(grid)
library(reshape2)
mt <- melt(combi, id = "Date", measure = c("sr", "rh","temp"))
p1<-ggplot(mt, aes(x=mt$Date, y=value, colour = variable)) +ggtitle("Meteorological Factor")+ geom_line(show.legend = TRUE)+scale_colour_manual(values=c("gold","orchid4","darkorange"))+ylab("Variation")+xlab(" ")+ylim(0,120)

#Multiplots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
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

p2<-ggplot(wsmg,aes(wsmg$Date,wsmg$Concentration))+ geom_line(size = 1,show.legend = TRUE,color="darkslategray") + xlab("Months")+ ylab("Wind Speed(m/s)") + theme(axis.text.x = element_text(angle = 0, vjust = 0.5, color = "navy"))+ ylim(0,2.5)
multiplot(p1,p2,cols=1)
