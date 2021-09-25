#PLOT 4

###EXTRACTION####

library(tidyverse)

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

#checking if the exists, and download and unzip it case it don't exists
if (!file.exists("dataset.zip")){
  download.file(url, destfile = "dataset.zip")
  unzip("dataset.zip")
}
df <- read.table("household_power_consumption.txt")

###TRANSFORMATION###

#tiding the data set
colNames <- strsplit(df[1,], ";")
df <- separate(df, V1, into = unlist(colNames), sep = ";", convert = TRUE)[-1,]
row.names(df) <- NULL

#Converting all variables, except Date and Time, to numeric
int <- unlist(colNames[[1]][-c(1,2)])
df[, int] <- sapply(df[, int], as.numeric)

#Converting the Date and Time variables
df$Date <- as.Date(df$Date,"%d/%m/%Y")
df <- filter(df, Date >= "2007-02-01" & Date <= "2007-02-02" )
df <- mutate(df, Time = as.POSIXlt(paste(Date, format(as.POSIXlt(Time, format = "%H:%M:%S"), '%T'))))

###LOADING###

png(filename = "plot4.png", width = 480, height = 480, units = "px")
par(mfrow = c(2,2))

#subplot 1
x <- df$Time
y <- df$Global_active_power
plot(x,y, type = "l", xlab = "", ylab = "Global Activer Power (kilowatts)")

#subplot 2
y <- df$Voltage
plot(x,y, type = "l", xlab = "datetime", ylab = "Voltage")

#subplot 3
y1 <- df$Sub_metering_1
y2 <- df$Sub_metering_2
y3 <- df$Sub_metering_3
plot(x,y1, type = "l", xlab = "", ylab = "Energy sub metering", ylim = c(0,30))
lines(x,y2, col = "red")
lines(x,y3,col = "blue")
legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black","red","blue"),lty=1, bty = "n")

#subplot 4
y <- df$Global_reactive_power
plot(x,y, type = "l", xlab = "datetime", ylab = "Global_reactive_power", ylim = c(0.0,0.5))

dev.off()