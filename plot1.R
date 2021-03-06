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


## Actual function to create Plot 1
Plot1 <- function() {
  plot_1_data <- GetData() ## Get the desired data using above function
  par(mfrow=c(1,1)) ## Set number of graphs on the screen to 1 with 'par' function
  
  ## Create transparent png image 480 by 480 pixels in size
  png(filename = "plot1.png", width = 480, height = 480, units = "px", bg="transparent")
  
  ## Make histogram, give it title, label x and y axis, and set color to red
  hist(plot_1_data$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", col="red")
  dev.off() ## Close
}

## Execute plotting function for Plot 1
Plot1()