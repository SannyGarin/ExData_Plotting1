plot1 <- function() {
    # Specify the path to your zip file
    zip_file <- "exdata_data_household_power_consumption.zip"
    
    # Specify the name of the txt file *inside* the zip archive
    txt_file_inside_zip <- "household_power_consumption.txt" 
    
    # Use `unz` to create a connection to the file within the zip
    con <- unz(zip_file, txt_file_inside_zip)
    
    # Read the txt data using `read.table` 
    df <- read.table(con, sep = ";", header = TRUE)  
    
    # Import dplyr to filter data
    library(dplyr)
    
    # Convert Date and Time column into appropriate classes
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
    df$Time <- strptime(df$Time, format = "%H:%M:%S")
    
    #Convert the rest of the columns as numeric
    cols_to_convert <- names(df[,3:9])
    df[cols_to_convert] <- lapply(df[cols_to_convert], as.numeric)
    
    #Filter the data on specified dates  "2007-02-01" to "2007-02-02" only
    filtered_df  <- filter(df, Date >= "2007-02-01" & Date <= "2007-02-02")
    
    # Use hist function for create the the first plot and annotate it accordingly
    hist(filtered_df$Global_active_power , col = "Red" , xlab = "Global Active Power (Kilowatts)", main = "Global Active Power" )
    
    # Open a PNG device
    png("plot1.png", width = 480 , height = 480 , units = "px") # Adjust parameters as needed
    
    # Re-create the histogram (important!)
    hist(filtered_df$Global_active_power , col = "Red" , xlab = "Global Active Power (kilowatts)", main = "Global Active Power" )
    dev.off()
}