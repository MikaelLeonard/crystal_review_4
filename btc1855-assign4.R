# BTC1855 CODING IN R
# ASSIGNMENT 4
# CRYSTAL LEE

library(dplyr)

# Read the .csv file as a dataframe
ufo <- as.data.frame(read.csv("ufo_subset.csv"))

# Verify that ufo is a data.frame
class(ufo)

# Check the dimensions 
dim(ufo)

# View the structure of the data
str(ufo)

glimpse(ufo)


## separate date and time
## change date into date
## durations.hours.min in characters, standardize units
## date posted is not in dates