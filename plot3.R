## Calling the function plot3("filelocation") reproduces the plot 3. It reads the file given as argument to it.
plot3 <- function(filelocation) {
        ## Reading the file with a header and delimiter ';'
        epData <- read.table(filelocation, header = T, sep = ";")
        ## The date column is formatted as required
        epData$Date <- as.Date(epData$Date, format = "%d/%m/%Y")
        ## Extracting data from the dates 2007-02-01 and 2007-02-02
        twoDaysData <-
                epData[(epData$Date == "2007-02-01") |
                               (epData$Date == "2007-02-02"),]
        ## Converting Columns as Numeric Vector
        twoDaysData$Global_active_power <-
                as.numeric(as.character(twoDaysData$Global_active_power))
        twoDaysData$Sub_metering_1 <-
                as.numeric(as.character(twoDaysData$Sub_metering_1))
        twoDaysData$Sub_metering_2 <-
                as.numeric(as.character(twoDaysData$Sub_metering_2))
        twoDaysData$Sub_metering_3 <-
                as.numeric(as.character(twoDaysData$Sub_metering_3))
        
        ## Extracting Character representations of Date and Time.Then, converting them to date(POSIXct) class and adding this column to twoDaysData
        twoDaysData <-
                transform(twoDaysData, timestamp = as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
        
        ## Creating plot as per the question
        plot(
                twoDaysData$timestamp,twoDaysData$Sub_metering_1, type = "l", xlab = "", ylab =
                        "Energy sub metering"
        )
        lines(twoDaysData$timestamp,twoDaysData$Sub_metering_2,col = "red")
        lines(twoDaysData$timestamp,twoDaysData$Sub_metering_3,col = "blue")
        legend(
                "topright", col = c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty =
                        1, lwd = 2.5
        )
        
        # The following codes are for copying the plot to the png file plot1.png
        dev.copy(png, file = "plot3.png", width = 480, height = 480)
        dev.off()
        ## Informing the location of the png file
        cat("Plot saved to the file plot3.png in : ", getwd())
}
