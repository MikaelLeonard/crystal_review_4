# BTC1855 CODING IN R
# ASSIGNMENT 4
# CRYSTAL LEE

library(dplyr)
library(tidyr)
library(lubridate)

setwd("~/Desktop/UTM/BTC1855/btc1855-assign4")

# Read the .csv file as a dataframe
ufo <- as.data.frame(read.csv("ufo_subset.csv"))

# Verify that ufo is a data.frame
class(ufo)

# Check the dimensions 
dim(ufo)

# View the structure of the data
str(ufo)

# Separate date and time from datetime column
ufo2 <- ufo %>% separate(datetime, c('date', 'time'), sep = " ")

# Change "date" into date class
ufo2$date <- ymd(ufo2$date)
class(ufo2$date)

# Create two new columns for both hours and minutes
# to standardize units 
# Move new columns after duration.seconds
ufo3 <- ufo2 %>%
  mutate(duration.hours = round(duration.seconds / 3600, 2),
         duration.mins = round(duration.seconds / 60, 2)) %>% 
  relocate(c(duration.mins, duration.hours), .after = duration.seconds)

# Specify select() function from dplyr and remove duration.hours.min column
ufo3 <- dplyr::select(ufo3, -duration.hours.min)

# Inspect column to see if there's any inconsistency with date formatting
table(ufo3$date_posted)
# Check for NAs in the column
sum(is.na(ufo3$date_posted))

# Convert dates to YYMMDD format, quiet = TRUE deals with errors to parse silently
ufo4 <- ufo3 %>%
  rename(date.posted = date_posted) %>%
  mutate(
    date.posted = ifelse(!is.na(date.posted), 
                         format(dmy(date.posted, "%d-%m-%Y", quiet = TRUE), "%Y-%m-%d"), 
                         NA_character_))

# Remove the country name inside brackets in the city column and place "ca" in 
# country column instead if it's blank for Canadian cities
ufo5 <- ufo4 %>%
  mutate(
    # Search for cities that contain a bracket and characters inside of it
    # If so, remove the bracket and substitute with nothing
    # If no bracket, keep the city name the way it is 
    city = ifelse(grepl("\\(.*\\)", city), sub("\\s*\\(.*\\)", "", city), city),
    # Update country to CA if city contains (canada)
    country = ifelse(grepl("\\(canada\\)", city, ignore.case = TRUE) & country != "ca", "ca", country))

# Remove sightings that are hoaxes using key words indicating a hoax
# tolower() converts all characters to lower case 
ufo6 <- ufo5 %>%
  filter(!grepl("hoax|fake|prank|not real|joke|trick|spoof", tolower(comments)))

# Create report_delay column and compute time difference in days
ufo7 <- ufo6 %>%
  mutate(report_delay = as.numeric(interval(date, date.posted), "days")) %>%
  # Keep rows where the time difference is positive, indicating the reported
  # date was after the date of sighting
  filter(report_delay > 0)

# Create table for average report_delay per country 
# Convert blank entries to NA, remove NA from table
mean_delay_country <- ufo7 %>% 
  mutate(country = ifelse(country == "", NA, country)) %>% 
  group_by(country) %>% 
  summarise(mean_delay = mean(report_delay)) %>% 
  filter(!is.na(country))

View(mean_delay_country)

# Check for outliers using boxplot 
boxplot(ufo7$duration.seconds)

# Define quantiles 
Q1 <- quantile(ufo7$duration.seconds, 0.25)
Q3 <- quantile(ufo7$duration.seconds, 0.75)
IQR <- Q3 - Q1

# Define the limits for non-outlier data
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Filter the dataset to exclude outliers based on quantiles
ufo_filtered <- ufo7[ufo7$duration.seconds >= lower_limit & 
                       ufo7$duration.seconds <= upper_limit, ]

# Plot the histogram 
hist(ufo_filtered$duration.seconds, 
     main = "Frequency of UFO Sighting Duration",
     xlab = "Duration (seconds)",
     ylab = "Frequency",
     col = "aquamarine")

