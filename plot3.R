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


## Actual function to create Plot 3
Plot3 <- function() {
  plot_3_data <- GetData() ## Get the desired data using above function
  par(mfrow=c(1,1)) ## Set number of graphs on the screen to 1 with 'par' function
  
  ## Create transparent png image 480 by 480 pixels in size
  png(filename = "plot3.png", width = 480, height = 480, units = "px", bg="transparent")
  
  ## Make plot, set x and y data, and label x and y axis
  plot_3_lines = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3") ## Define the 3 lines in plot
  
  ## Plot the three lines and label the graph
  plot(plot_3_data$DateTime, plot_3_data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
  lines(plot_3_data$DateTime, plot_3_data$Sub_metering_2, type="l", col="red")
  lines(plot_3_data$DateTime, plot_3_data$Sub_metering_3, type="l", col="blue")
  
  ## Create a legend in the topright corner of the plot
  legend("topright", lty=1, lwd=1, col=c("black","blue","red"), legend=plot_3_lines)
  dev.off() ## Close
}

## Execute plotting function for Plot 3
Plot3()