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

#convert Global_active_power to numeric type
data$Global_active_power = as.numeric(data$Global_active_power)

#create plot2
png(file="plot2.png")
plot(data$datetime,data$Global_active_power,type="l",xlab="",ylab="Global Active Power(kilowatt)")
dev.off()

