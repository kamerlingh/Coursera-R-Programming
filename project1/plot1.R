##read file into memory

first5rows <- read.table("household_power_consumption.txt", header=TRUE, sep=";", nrows=5, na.strings="?")
classes <- sapply(first5rows, class)
names <- names(first5rows)
household_data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", colClasses=classes, na.strings="?", skip=66636, nrows=2880)
names(household_data) <- names

#another way to read in the data:
#all_household_data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", colClasses=classes, na.strings="?")
#subset(all_household_data, all_household_data$Date== '1/2/2007' || all_household_data$Date == '2/2/2007')

##create histogram using png as the graphics device

png(filename="plot1.png", width=480, height=480, units="px", bg="transparent")
hist(household_data$Global_active_power, xlab="Global Active Power (kilowatts)", col="red", main="Global Active Power")
dev.off()