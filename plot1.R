## Calling the function plot1("filelocation") reproduces the plot 1. It reads the file given as argument to it.
plot1 <- function(filelocation) {
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
        ## Creating histogram as per the question
        hist(
                twoDaysData$Global_active_power, main = paste("Global Active Power"), col =
                        "red", xlab = "Global Active Power (kilowatts)"
        )
        # The following codes are for copying the plot to the png file plot1.png
        dev.copy(png, file = "plot1.png", width = 480, height = 480)
        dev.off()
        ## Informing the location of the png file
        cat("Plot saved to the file plot1.png in : ", getwd())
}
