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
  myPlot1()
}

myPlot1 <- function(){
  
  png(file="plot1.png", width=480, height=480, units="px")
  
  hist(myDataSub[,3],xlab="Global Active Power (kilowatts)",main="Global Active Power",col="red")
  
  dev.off()
  
}

main()