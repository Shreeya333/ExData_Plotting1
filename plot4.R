## Calling the function plot4("filelocation") reproduces the plot 4. It reads the file given as argument to it.
plot4 <- function(filelocation) {
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
        twoDaysData$Global_reactive_power <-
                as.numeric(as.character(twoDaysData$Global_reactive_power))
        twoDaysData$Voltage <-
                as.numeric(as.character(twoDaysData$Voltage))
        
        ## Extracting Character representations of Date and Time.Then, converting them to date(POSIXct) class and adding this column to twoDaysData
        twoDaysData <-
                transform(twoDaysData, timestamp = as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
        
        ## Plot Layout is changed from (1,1) to (2,2) using the par command. 
        par(mfrow=c(2,2))
        
        ## Creating plot as per the question
        plot(twoDaysData$timestamp,twoDaysData$Global_active_power, type="l", xlab="", ylab="Global Active Power")
        
        plot(twoDaysData$timestamp,twoDaysData$Voltage, type="l", xlab="datetime", ylab="Voltage")
        
        plot(twoDaysData$timestamp,twoDaysData$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
        lines(twoDaysData$timestamp,twoDaysData$Sub_metering_2,col="red")
        lines(twoDaysData$timestamp,twoDaysData$Sub_metering_3,col="blue")
        legend("topright", col=c("black","red","blue"), c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  "),lty=c(1,1), bty="n", cex=.5) #bty removes the box, cex shrinks the text, spacing added after labels so it renders correctly
        
        plot(twoDaysData$timestamp,twoDaysData$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
        
        
        # The following codes are for copying the plot to the png file plot1.png
        dev.copy(png, file = "plot4.png", width = 480, height = 480)
        dev.off()
        ## Informing the location of the png file
        cat("Plot saved to the file plot4.png in : ", getwd())
}
