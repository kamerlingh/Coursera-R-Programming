##read file into memory
first5rows <- read.table("household_power_consumption.txt", header=TRUE, sep=";", nrows=5, na.strings="?")
classes <- sapply(first5rows, class)
names <- names(first5rows)
household_data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", colClasses=classes, na.strings="?", skip=66636, nrows=2880)
names(household_data) <- names

##convert Date and Time columns into POSIXlt
time <- paste(household_data$Date, household_data$Time, sep=" ")
time <- strptime(time, "%d/%m/%Y %H:%M:%S")

##create png plot of submeter readings
png(filename="plot3.png", width=480, height=480, units="px", bg="transparent")
plot(time, household_data$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
lines(time, household_data$Sub_metering_1, col="black")
lines(time, household_data$Sub_metering_2, col="red")
lines(time, household_data$Sub_metering_3, col="blue")
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, col=c("black", "red", "blue"))
dev.off()