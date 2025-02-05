####### Setup ######

data(airquality) #load the data to see it better


###### 1. Rows With Missing Values ######

#complete cases checks if a row has any NA, returns false if NA
missing_rows <- airquality[!complete.cases(airquality), ] #Find all NA rows
print(missing_rows) #Prints the actual missing rows
rownames(missing_rows) #This gives all the rows as characters
missing_rows_numbers <- as.character(rownames(missing_rows)) #Stores as numbers 
print(missing_rows_numbers) # Prints to display 


###### 2. Ozone and Temperature Analysis ######
# Find a way to only use Ozone and Temperature rows with valid values
valid_ozone <- airquality$Ozone[complete.cases(airquality$Ozone)]
print(valid_ozone)

#All temperatures have values, but this is to ensure it (might be useless)
valid_temp <- airquality$Temp[complete.cases(airquality$Temp)]
print(valid_temp)

#Statistical analysis 
mean_ozone <- mean(valid_ozone)
sd_ozone <- sd(valid_ozone)
min_ozone <- min(valid_ozone)
max_ozone <- max(valid_ozone)
ozone_stats <- c(mean_ozone, sd_ozone, min_ozone, max_ozone)

mean_temp <- mean(valid_temp)
sd_temp <- sd(valid_temp)
min_temp <- min(valid_temp)
max_temp <- max(valid_temp)
temp_stats <- c(mean_temp, sd_temp, min_temp, max_temp)

df <- data.frame(ozone_stats, temp_stats)
rownames(df) <- c("Mean", "SD", "Min", "Max")
colnames(df) <- c("Ozone", "Temperature")

print(df)

