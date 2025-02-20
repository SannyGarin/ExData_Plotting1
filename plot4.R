plot4 <- function() {
    # Specify the path to your zip file
    zip_file <- "exdata_data_household_power_consumption.zip"
    
    # Specify the name of the txt file *inside* the zip archive
    txt_file_inside_zip <- "household_power_consumption.txt" 
    
    # Use `unz` to create a connection to the file within the zip
    con <- unz(zip_file, txt_file_inside_zip)
    
    # Read the txt data using `read.table` 
    df <- read.table(con, sep = ";", header = TRUE)  
    
    # Import dplyr to filter data
    # Install and load dplyr if you haven't already
    
    if(!require(dplyr)){install.packages("dplyr")}
    library(dplyr)
    
    # Keep rows where NONE of the columns have "?"
    df <- df %>%
        filter(!if_any(everything(), ~ . == "?"))
    
    # Convert Date and Time column into appropriate classes
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
    df$Time <- strptime(paste(df$Date,df$Time), format = "%Y-%m-%d %H:%M:%S")
    
    
    #Convert the rest of the columns as numeric
    cols_to_convert <- names(df[,3:9])
    df[cols_to_convert] <- lapply(df[cols_to_convert], as.numeric)
    # df <- na.omit(df)
    
    #Filter the data on specified dates  "2007-02-01" to "2007-02-02" only
    
    df <- df[df$Date >= "2007-2-1"  & df$Date <= "2007-2-2",]
    
    # Use hist function for create the the first plot and annotate it accordingly
    
    par(mfrow = c(2, 2), mar = c(4,4,5,1))
    
    with(df, plot (Time,Global_active_power, type = "l" , 
                   ylab = "Global Active Power (kilowatts)",
                   xlab = "",
                   xaxt = "n" ))
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    
    with(df, plot (Time,Voltage, type = "l" , 
                   ylab = "Voltage",
                   xlab = "",
                   xaxt = "n" ))
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    
    with(df , plot(Time,Sub_metering_1 , type = "n", xlab = "",  ylab = "Energy sub metering", xaxt = "n"))
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    with(df, lines(Time, Sub_metering_1 ))
    with(df, lines(Time, Sub_metering_2, col = "red" ))
    with(df, lines(Time, Sub_metering_3, col = "blue" ))
    legend("topright",lty = 1, col=c("black","red","blue"),
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    with(df, plot (Time,Global_reactive_power, type = "l" , 
                   ylab = "Global_reactive_power",
                   xlab = "",
                   xaxt = "n" ))
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    

    
    
    # Open a PNG device
    png("plot4.png", width = 480 , height = 480 , units = "px") # Adjust parameters as needed
    
    # # Re-create the line chart (important!)
    par(mfrow = c(2, 2), mar = c(4,4,5,1))
    
    with(df, plot (Time,Global_active_power, type = "l" , 
                   ylab = "Global Active Power (kilowatts)",
                   xlab = "",
                   xaxt = "n" ))
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    
    with(df, plot (Time,Voltage, type = "l" , 
                   ylab = "Voltage",
                   xlab = "",
                   xaxt = "n" ))
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    
    with(df , plot(Time,Sub_metering_1 , type = "n", xlab = "",  ylab = "Energy sub metering", xaxt = "n"))
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    with(df, lines(Time, Sub_metering_1 ))
    with(df, lines(Time, Sub_metering_2, col = "red" ))
    with(df, lines(Time, Sub_metering_3, col = "blue" ))
    legend("topright",lty = 1, col=c("black","red","blue"),
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    with(df, plot (Time,Global_reactive_power, type = "l" , 
                   ylab = "Global_reactive_power",
                   xlab = "",
                   xaxt = "n" ))
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    
    
    dev.off()
}