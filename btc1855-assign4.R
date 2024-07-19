# BTC1855 CODING IN R
# ASSIGNMENT 4 - MARTIANS
# CRYSTAL LEE

library(dplyr)
library(tidyr)
library(lubridate)

setwd("~/Desktop/UTM/BTC1855/btc1855-assign4")

# Read the .csv file as a dataframe
ufo <- read.csv("ufo_subset.csv")

# Verify that ufo is a data.frame
class(ufo)

# Check the dimensions 
dim(ufo)

# View the structure of the data
str(ufo)

# Check for duplicate entries
# No duplicates
any(duplicated(ufo))

# Separate date and time from datetime column
ufo2 <- ufo %>% separate(datetime, c('date', 'time'), sep = " ")

# Change "date" into Date class
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

# Convert dates to YY-MM-DD format, quiet = TRUE deals with warnings and errors quietly
ufo4 <- ufo3 %>%
  rename(date.posted = date_posted) %>%
  mutate(
    # If the %d-%m-%Y format is TRUE, convert it to %Y-%m-%d
    # If FALSE, keep it to NA character
    date.posted = ifelse(!is.na(date.posted), 
                         format(dmy(date.posted, "%d-%m-%Y", quiet = TRUE), "%Y-%m-%d"), 
                         NA_character_))

# Inspect rows where the country column is missing or empty
missing_country_rows <- ufo4 %>%
  filter(is.na(country) | country == "") %>%
  filter(grepl("\\(.*\\)", city))

View(missing_country_rows)

# See how many blank or NA entries each column contains
# There are no NAs but blanks in state, country, and, shape
missing_summary <- ufo4 %>%
  summarise_all(list(
    blanks = ~ sum(. == "", na.rm = TRUE),
    nas = ~ sum(is.na(.), na.rm = TRUE)))

View(missing_summary)

# Create vectors for Canadian provinces and American states
canadian_provinces <- c("ab", "bc", "mb", "nb", "nl", "nt", "ns", "nu", "on", "pe", "qc", "sk", "yt")
us_states <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga", "hi", 
                     "id", "il", "in", "ia", "ks", "ky", "la", "me", "md", "ma", "mi", "mn", 
                     "ms", "mo", "mt", "ne", "nv", "nh", "nj", "nm", "ny", "nc", "nd", "oh", 
                     "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", 
                     "wv", "wi", "wy")

# Cleaning up the city and country column, deal with blank entries
# I will focus on cleaning up and re-organizing Canadian and American sightings as it takes up
# majority of the observations
ufo5 <- ufo4 %>%
  mutate(
    # Update country to ca if the state contains any Canadian province abbreviation and country is blank
    country = ifelse(country == "" & tolower(state) %in% canadian_provinces, "ca", country),
    # Update country to us if state contains any American state abbreviation and country is blank
    country = ifelse(country == "" & tolower(state) %in% us_states, "us", country),
    # Update country to ca if city contains (canada)
    country = ifelse(grepl("\\(canada\\)", city, ignore.case = TRUE) & country != "ca", "ca", country),
    # Search for cities that contain a bracket and characters inside of it
    # If TRUE, remove the bracket and substitute with nothing
    # If FALSE, keep the city name the way it is 
    city = ifelse(grepl("\\(.*\\)", city), sub("\\s*\\(.*\\)", "", city), city),
    # Replace blank entries with "other" so we have a broad category of countries that have less entries
    country = ifelse(country == "", "other", country),
    # Replace blank entries in state and shape with "unknown"
    state = ifelse(state == "", "unknown", state),
    shape = ifelse(shape == "", "unknown", shape)
    ) %>% 
    # Region is a broader term that includes other countries that aren't the US
    rename(region = state)

# Keep rows that do not contain hoax indicator words and keep rows with "not a hoax"
# tolower() converts all characters to lower case 
ufo6 <- ufo5 %>%
  filter(grepl("not a hoax", tolower(comments)) | 
           !grepl("hoax|fake|not real|joke|trick|spoof|prank", tolower(comments)))

# Create report_delay column and compute time difference in days
ufo7 <- ufo6 %>%
  mutate(report_delay = as.numeric(interval(date, date.posted), "days")) %>%
  # Keep rows where the time difference is positive, indicating the reported
  # date was after the date of sighting
  filter(report_delay > 0)

# Create table for average report_delay per country 
mean_delay_country <- ufo7 %>% 
  group_by(country) %>% 
  summarise("mean_delay (days)" = mean(report_delay)) %>% 
  # Place other at the end
  # All FALSE entries (non-other) are less than TRUE (other) values, so they will come first
  arrange(country == "other", country)

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

