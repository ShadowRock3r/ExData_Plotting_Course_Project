#PLOT 1
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

png(filename = "plot1.png", width = 480, height = 480, units = "px")
hist(df$Global_active_power, main = "Global Active Power", xlab = "Global Active Power(kilowatts)", col = "red")
dev.off()

