#function for get subset data for assignment of Exploratory Data Analysis class
#get text file include 1/2/2007 - 2/2/2007 data from original text file
getSubData <- function(fileName="household_power_consumption.txt"){
need_read = TRUE
each_read_step = 5000
origin_file <- file(fileName,open="rt")
header = readLines(origin_file,n=1)
subLines = c(header)
while(need_read){
each_step_lines = readLines(origin_file,n=each_read_step)
if(length(each_step_lines) < each_read_step){
need_read = FALSE
}
each_subLines_Index = grep("^[12]/2/2007",each_step_lines)
each_subLines = each_step_lines[each_subLines_Index]
subLines = c(subLines,each_subLines)
}
close(origin_file)
data =read.csv2(textConnection(subLines,open="rt"),colClasses=c("character"))
return(data)
}

#read data from txt file
data = getSubData()

#create datetime column form Date and Time Column
data$datetime = strptime(paste(data$Date,data$Time),format="%d/%m/%Y %H:%M:%S")

#covert data from string to numeric
data$Global_active_power = as.numeric(data$Global_active_power)
data$Global_reactive_power = as.numeric(data$Global_reactive_power)
data$Voltage = as.numeric(data$Voltage)
data$Sub_metering_1 = as.numeric(data$Sub_metering_1)
data$Sub_metering_2 = as.numeric(data$Sub_metering_2)
data$Sub_metering_3 = as.numeric(data$Sub_metering_3)

#create plot4
png(file="plot4.png")
par(mfrow =c(2,2))
# first plot
plot(data$datetime,data$Global_active_power,type="l",xlab="",ylab="Global Active Power")
#second plot
plot(data$datetime,data$Voltage,type="l",xlab="datatime",ylab="Voltage")
#third plot
plot(data$datetime,data$Sub_metering_1,type="l",xlab="",ylab="Energy sub metering")
lines(data$datetime,data$Sub_metering_2,col="red")
lines(data$datetime,data$Sub_metering_3,col="blue")
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),bty="n",lty=c(1,1,1),col=c("black","red","blue"))
#last plot
plot(data$datetime,data$Global_reactive_power,type="l",xlab="datatime",ylab="Global_reactive_power")
dev.off()
