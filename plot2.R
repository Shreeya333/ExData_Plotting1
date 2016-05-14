## Calling the function plot2("filelocation") reproduces the plot 2. It reads the file given as argument to it.
plot2 <- function(filelocation) {
        ## Reading the file with a header and delimiter ';'
        epData <- read.table(filelocation, header = T, sep = ";")
        ## The date column is formatted as required
        epData$Date <- as.Date(epData$Date, format = "%d/%m/%Y")
        ## Extracting data from the dates 2007-02-01 and 2007-02-02
        twoDaysData <-
                epData[(epData$Date == "2007-02-01") |
                               (epData$Date == "2007-02-02"),]
        ## Converting as Numeric Vector
        twoDaysData$Global_active_power <-
                as.numeric(as.character(twoDaysData$Global_active_power))
        ## Extracting Character representations of Date and Time.Then, converting them to date(POSIXct) class and adding this column to twoDaysData
        twoDaysData <-
                transform(twoDaysData, timestamp = as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
        ## Creating plot as per the question
        plot(
                twoDaysData$timestamp,twoDaysData$Global_active_power, type = "l", xlab =
                        "", ylab = "Global Active Power (kilowatts)"
        )
        # The following codes are for copying the plot to the png file plot1.png
        dev.copy(png, file = "plot2.png", width = 480, height = 480)
        dev.off()
        ## Informing the location of the png file
        cat("Plot saved to the file plot2.png in : ", getwd())
}
