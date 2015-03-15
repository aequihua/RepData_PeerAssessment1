#-------------
# Reproducible Research - Peer Assessment 1
# Author: Arturo Equihua
# Collection of R code for the final report
# Just a working area to test the code prior to
# embedding it in the final report
#---------------

# Packages required: ggplot2, lubridate, plyr, stringr

load_data <- function(fname) {
  library(stringr)
  
  rawDF <- read.csv(fname, colClasses=c("numeric","Date","numeric"))
  # Transform the interval column to time values
  rawDF <- transform(rawDF,interval=str_pad(as.character(interval),4,pad="0"))
  rawDF <- transform(rawDF,interval=sub("([[:digit:]]{2,2})$", ":\\1", interval))
  rawDF <- transform(rawDF,interval=ymd_hm(paste("2010-01-01",interval)))
  
  rawDF
}

phase1 <- function(dataset) {
  library(plyr)
  library(ggplot2)

  # Group the total steps per date, the value will be in V1 column
  sumed=ddply(dataset,c("date"),function(dataset)sum(dataset$steps))

  # Plot the histogram
  ggplot(data=sumed, aes(x=sumed$V1)) + 
      geom_bar(stat="bin",binwidth=4000, colour="black", fill="purple")+
      xlab("Number of steps per day")+ylab("Frequency")+
      ggtitle("Histogram of steps per day")

  # Calculate average and median
  av = mean(sumed$V1,na.rm=TRUE)
  md = median(sumed$V1,na.rm=TRUE)
  c(av,md)
}

phase2 <- function(dataset) {
  library(lubridate)
  
  # Summarize the average values of the intervals
  sumed <- ddply(dataset,c("interval"),function(dataset){
       round(mean(dataset$steps,na.rm=TRUE))
    })
  colnames(sumed) = c("interval","avgsteps")


  # Produce the time series plot
  plot(sumed,type="l",xlab="Interval",ylab="Average steps per interval",main="Time Series Steps",col="blue")
  
  # Return which interval has the highest number of average steps, formatted as HH:MM, and include
  # the number of steps in that interval
  n=which(sumed$avgsteps==max(sumed$avgsteps,na.rm=TRUE))
  a=sumed[n,]$interval
  val=paste(str_pad(hour(a),2,pad="0"),":",str_pad(minute(a),2,pad="0"),sep="")
  c(val,round(sumed[n,]$avgsteps))
}

phase3 <- function(dataset) {
  # Calculate the number of data rows that have NA values
  nr =  nrow(dataset[which(is.na(dataset)),])
  
  # Fill the NA values, for this exercise I will take the mean of the day to
  # replace the NA values
  meanDF=ddply(dataset,c("date"),function(dataset)mean(dataset$steps,na.rm=TRUE))
  
  # Replace the NaN average values with the entire period average
  meanDF=transform(meanDF,V1=ifelse(is.nan(V1),mean(meanDF$V1,na.rm=TRUE),V1))
  
  # Replace the NA values of the main dataset using the apply function
  colnum=apply(dataset,1,function(x) { 
                            if (is.na(x[1])) 
                                  meanDF[meanDF$date==x[2],]$V1 
                            else 
                                  x[1] } )
  dataset$steps = as.numeric(colnum)
  
  # Repeat the histogram, mean and median calculations for the new dataset
  phase1(dataset)
  
  dataset
}

phase4 <- function(dataset) {
  library(lubridate)
  library(ggplot2)
  
# Create the factor column for Weekday-Weekend
  nuevo$daytype = ifelse((wday(nuevo$date) %in% c(1,7)),"Weekend","Weekday")
  nuevo$daytype = as.factor(nuevo$daytype)

# Get the averages by interval value
  sumed <- ddply(dataset,c("daytype","interval"),function(dataset){
                          round(mean(dataset$steps,na.rm=TRUE))
                      })
  colnames(sumed) = c("DayType","Interval","AvgSteps")

# Obtain the plots using ggplot2
  qplot(Interval,AvgSteps,data=sumed,facets=DayType~.,
        geom="line",ylab="Average Steps") +
        ggtitle("Comparison between weekdays and weekends")
}
