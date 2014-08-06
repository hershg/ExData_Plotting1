## Function to download a file from a URL (will be used by later function)
DownloadFile <- function(FileUrl, FileName) {
  if(!file.exists(FileName)) {
    download.file(FileUrl, destfile=FileName, method="curl")
  }
  FileName
}

## Function to download and prepare ZIP file data from URL
GetData <- function() {
  cacheFile <- "data_plot.csv"
  if(file.exists(cacheFile)) { ## If data already cached
    data <- read.csv(cacheFile) ## Read data from cache
    data$DateTime <- strptime(data$DateTime, "%Y-%m-%d %H:%M:%S") ## Process data
  }
  else {
    ## Download and unzip raw data from online
    FileName <- DownloadFile("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                             "household_power_consumption.zip")
    
    raw <- unz(FileName, "household_power_consumption.txt")
    
    ## Read data from text file
    data <- read.table(raw, header=T, sep=';', na.strings="?", colClasses=c("character","character","numeric",
                                                                            "numeric","numeric","numeric",
                                                                            "numeric","numeric","numeric"))
    
    ## Subset data to only data from 1/2/2007 to 2/2/2007 (day/month/year)
    data <- data[(data$Date == "1/2/2007") | (data$Date == "2/2/2007"),]
    data$DateTime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")
    
    ## Return the subsetted, prepared data
    write.csv(data, cacheFile)
  }
  
  ## Return data
  data
}


## Actual function to create Plot 4
Plot4 <- function() {
  plot_4_data <- GetData()
  
  ## Create transparent png image 480 by 480 pixels in size
  png(filename = "plot4.png", width = 480, height = 480, units = "px", bg="transparent")
  par (mfrow = c(2,2), mar = c(4,4,2,1), oma = c(0,0,2,0)) ## Make 2X2 grid of graphs; set margins
  
  with(plot_4_data, { ## Plot all 4 graphs
    ## Plot 1st graph
    plot(DateTime, Global_active_power, xlab="", ylab="Global Active Power", type="l")
    
    ## Plot 2nd graph
    plot(DateTime, Voltage, xlab="datetime", ylab="Voltage", type="l")
    
    ## Plot 3rd graph
    plot_3_lines = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    plot(DateTime, Sub_metering_1, type="l", xlab="", ylab="Energy sub metering") ## Draw 3 lines on graph 3
    lines(DateTime, Sub_metering_2, type="l", col="red")
    lines(DateTime, Sub_metering_3, type="l", col="blue")
    legend("topright", lty=1, lwd=1, col=c("black","blue","red"), legend=plot_3_lines, bty="n") ## Legend
    
    ## Plot 4th graph
    plot(DateTime, Global_reactive_power, xlab="datetime", ylab="Global_reactive_power", type="l")
  })    
  dev.off() ## Close
}

## Execute plotting function for Plot 4
Plot4()