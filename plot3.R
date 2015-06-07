SetWorkDir <- function() {
  myworkdir = #set my working directory here
  setwd(myworkdir)
}

LoadData <- function() {
  initial <<- read.csv("household_power_consumption.txt",sep=";",nrows=100)
  classes <<- sapply(initial,class)
  #data
  myData <<- read.table("household_power_consumption.txt",sep=";",header= TRUE,colClasses=classes,na.strings="?")
  myData[,1] <<- as.Date(myData[,1],"%d/%m/%Y")
  #date format
  SelectDate <<- myData[,1]==as.Date("2007-02-01") | myData[,1]==as.Date("2007-02-02")
  myDataSub <<- myData[SelectDate,]
  #time format
  timevector <<- as.POSIXct(strptime(paste(as.character(myDataSub[,1]),as.character(myDataSub[,2]),sep=" "),"%Y-%m-%d %H:%M:%S"))
  myDataSub[,2] <<- timevector
}

ClearUp <- function() {
  rm(myData, pos=.GlobalEnv)
  rm(SelectDate, pos=.GlobalEnv)
  rm(initial, pos=.GlobalEnv)
  rm(classes, pos=.GlobalEnv)
}

main <- function() {
  SetWorkDir()
  LoadData()
  ClearUp()
  myPlot3()
}

myPlot3 <- function(){
  
  png(file="plot3.png", width=480, height=480, units="px")
  
  plot(myDataSub[,7]~myDataSub[,2], ylab="Energy sub metering",xlab="",type="l")
  points(myDataSub[,8]~myDataSub[,2], col="red",type="l")
  points(myDataSub[,9]~myDataSub[,2], col="blue",type="l")
  legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),pch=NA,lty=1)         
  
  dev.off()
  
}

main()