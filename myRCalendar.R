### Calendar



## Spring 2019:
# Overview - read and go to Johnson's office about missing next Monday
# Applied Bayesian - nothing due
# Math Stats - **email Ding a time to get help**
# Genetic Epi - ***read***
# Biostats II - HW done

devtools::install_github("jayjacobs/ggcal")
library(ggplot2)
library(ggcal)
library(dplyr)
library(readxl)
library(lubridate)

mycal = readxl::read_xlsx("/Users/jamescutler/Desktop/Data_Course_cutler/Calendar_R.xlsx",
                          col_types = c("date","text"))

# Dates were imported as POSIXct objects, but ggcal wants date objects:
mycal = mutate(mycal, Day = as.Date(Day)) # badabing 

# Find the last date in the file:
lastDay = max(mycal$Day)
# Get last and first days of the month that contains the dates of my activities:
begThisMonth = as.Date(cut(lastDay, "month"))
endThisMonth = as.Date(cut(lastDay, "month")) + months(1) - 1
# Create a single-column dataframe with all dates of that month:
allDates = data.frame(Day = seq.Date(begThisMonth,endThisMonth, by = "1 day"))

# Now join the dataframes (awesome step!):
mycal = left_join(allDates, mycal)

# Now for ggcal (vector of dates, vector of values for colors):
ggcal(mycal$Day, mycal$Activity) +
  scale_fill_manual(name = "Things due",
                    values = c(
                    "biostats hw due" = "green",
                    "gen epi reading due" = "forestgreen",
                    "math stats hw due" = "red",
                    "overview reading due" = "steelblue"),
                    na.value = "grey88") +
  theme(legend.title = element_text())

# HOW THE HECK DO I GET GGCAL TO PUT TWO THINGS INTO THE SAME DAY??



