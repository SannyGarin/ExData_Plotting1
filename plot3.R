plot3 <- function() {
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
    
    plot( df$Time, df$Sub_metering_1 , type = "n", xlab = "",  ylab = "Energy sub metering", xaxt = "n")
    
    # To change the access form "Feb 1 00:00" , "Feb 2 00:00" , "Feb 3 00:00", to  "Thu","Fri","Sat"
    # We need to determine how to get these number 1, 1441, 2880
    # df rows are 2880, I reset the index using rownames(df) <- NULL
    # then I query df[df$Time == "2007-02-01",]  equal to index 1
    #               df[df$Time == "2007-02-02",]  equal to index 1441
    #               df[df$Time == "2007-02-03",]  equal to index 2880
    
    #For the axis
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    # For the 3 lines
    with(df, lines(Time, Sub_metering_1 ))
    with(df, lines(Time, Sub_metering_2, col = "red" ))
    with(df, lines(Time, Sub_metering_3, col = "blue" ))
    # For Legend
    legend("topright",lty = 1, col=c("black","red","blue"),
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    # Open a PNG device
    png("plot3.png", width = 480 , height = 480 , units = "px") # Adjust parameters as needed
    
    # # Re-create the line chart (important!)
    plot( df$Time, df$Sub_metering_1 , type = "n", xlab = "",  ylab = "Energy sub metering", xaxt = "n")
    axis(side = 1, at = c(as.numeric(df$Time[1]),  
                          as.numeric(df$Time[1441]),
                          as.numeric(df$Time[2880])),labels = c("Thu","Fri","Sat"))
    with(df, lines(Time, Sub_metering_1 ))
    with(df, lines(Time, Sub_metering_2, col = "red" ))
    with(df, lines(Time, Sub_metering_3, col = "blue" ))
    legend("topright",lty = 1, col=c("black","red","blue"),
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    dev.off()
}