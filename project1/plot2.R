##read file into memory

first5rows <- read.table("household_power_consumption.txt", header=TRUE, sep=";", nrows=5, na.strings="?")
classes <- sapply(first5rows, class)
names <- names(first5rows)
household_data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", colClasses=classes, na.strings="?", skip=66636, nrows=2880)
names(household_data) <- names

##convert Date and Time columns into POSIXlt
time <- paste(household_data$Date, household_data$Time, sep=" ")
time <- strptime(time, "%d/%m/%Y %H:%M:%S")

##create plot of the global active power over time with png as graphics device
png(filename="plot2.png", width=480, height=480, units="px", bg="transparent")
plot(time, household_data$Global_active_power, xlab="", ylab="Global Active Power (kilowatts)", type="l")
dev.off()