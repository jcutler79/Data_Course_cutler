### Epidemiology I ###

# %d     day as a number (0-31)     01-31
# %a     abbreviated weekday        Mon
# %A     unabbreviated weekday      Monday

# %m     month (00-12)              00-12
# %b     abbreviated month          Jan
# %B     unabbreviated month        January

# %y     two-digit year
# %Y     four-digit year
################################################################
# %H    hours as decimal number (01-24)
# %I    hours as decimal number (01-12 or 1-12)
# %M    minute as decimal number (00-59 or 0-59)
# %S    second as decimal number (00-59 or 0-59)
# %p    AM/PM indicator, used in conjunction with %I and not %H

# Libraries:
library(lubridate)
library(scales) # for date_breaks inside scale_x_datetime in ggplot
library(ggplot2)
library(dplyr) # for the 'between' function



# Outbreak data from in-class exercise:

out = read.csv("/Users/jamescutler/Desktop/Epidemiology_I/outbreak.csv", header = FALSE)

# This dataset gives us a %d-%b date
# It also gives us a time %I:%M %p but with periods

colnames(out) = c("ID","Name","Sex","Age","Ill","D_onset","T_onset",
                  "Hot_dog",
                  "Chicken",
                  "Baked_po",
                  "Roll",
                  "Green_b",
                  "Corn",
                  "Salad",
                  "Ice_cream",
                  "Choc_cake",
                  "Banana_s_c",
                  "Ice",
                  "Kool_aid",
                  "Coke",
                  "Sprite")
ill = out[which(out$Ill == "Y"),]
healthy = out[which(out$Ill == "N"),]

# CREATE THE RISK RATIO TABLE:
foods = data.frame(matrix(NA, nrow = 14, ncol = 9))
colnames(foods) = c("ill","well","total","attack.rate",
                 "ill2","well2","total2","attack.rate2",
                 "risk.ratio")
rownames(foods) = colnames(out)[8:21]
for (i in 8:21){
  foods[i-7,1] = length(which(ill[,i] == "Y"))
}
for (i in 8:21){
  foods[i-7,2] = length(which(healthy[,i] == "Y"))
}
for (i in 8:21){
  foods[i-7,5] = length(which(ill[,i] == "N"))
}
for (i in 8:21){
  foods[i-7,6] = length(which(healthy[,i] == "N"))
}
foods[,3] = foods[,1] + foods[,2] # total (ate the food)
foods[,4] = foods[,1] / foods[,3] # attack rate (ate the food)
foods[,7] = foods[,5] + foods[,6] # total (didn't eat)
foods[,8] = foods[,5] / foods[,7] # attack rate (didn't eat)
foods[,9] = foods[,4] / foods[,8] # risk ratio

# FOR FUN CREATE A PLOT TO VISUALIZE HOW HIGH BANANA SPLIT'S RISK RATIO IS:
ggplot(foods, aes(1:14,risk.ratio)) +
  geom_bar(stat = "identity", fill = "red") +
  scale_x_continuous(breaks = seq(1,14,1), 
                     labels = rownames(foods)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9)) +
  labs(title = "Risk Ratios for each food", x = "", y = "Risk Ratio")


# NOW CREATE THE TIME SERIES BAR PLOT:

# For ggplot to use the date.time column, I have to limit the data frame to just the ill rows:
ill$D_onset = as.character(ill$D_onset)
ill$T_onset = as.character(ill$T_onset)
ill$date.time = paste(ill$D_onset,ill$T_onset, sep = " ")
ill$date.time = gsub("\\.","",ill$date.time)
ill$date.time = strptime(ill$date.time, "%d-%b %I:%M %p") # THIS STEP CONVERTS THE CHARACTER REPRESENTATION TO A DATE TIME
year(ill$date.time) = 2003
class(ill$date.time) # "POSIXlt" "POSIXt"
ill$date.time = as.POSIXct(ill$date.time)
class(ill$date.time)

# WHERE THE COOL STUFF HAPPENS

# CREATE THE SEQUENCE OF TIME INTERVALS TO ACT AS THE WALLS OF THE HISTOGRAM BINS:
ints = seq.POSIXt(ISOdatetime(2003,03,19,3,0,0, tz = "UTC"), 
                  ISOdatetime(2003,03,25,3,0,0, tz = "UTC"), 
                  by = "8 hours")      
length(ints)
nints = as.numeric(ints)
tt = ill$date.time
ntt = as.numeric(tt)
nints
ntt
length(ntt); length(nints)
# AN ENTIRE DAY TO FIGURE THIS OUT!! (CREATE A MATRIX SO I CAN SUM THE COLUMNS; THE COLUMN SUMS = THE BAR HEIGHTS)
M = data.frame(matrix(NA, nrow = 22, ncol = 18))
for (i in 1:length(ntt)){
  for (j in 2:length(nints)){
    M[i,j] = ifelse(between(ntt[i],nints[j-1],nints[j]),1,0) 
  }
}
sum(colSums(M)) # THIS IS HOW YOU KNOW THERE IS AN EXTRA EVENT! IT SAYS 23! NOT 22! SHOULD BE 22!

M[,1] = 0 # Instead of doing the for loop above, you could just know that that column is all zeros and do it this way.  
colSums(M)
length(colSums(M)) # WHY DID IT CREATE AN EXTRA EVENT OUT OF THIN AIR????
M[,4] = 0 # Now I have to remove the extra event.

# TO SHIFT BARS TO THE LEFT (they would be too far to the right otherwise):
class(ints)
?as.POSIXlt
lt.ints = as.POSIXlt(ints) # What is as.POSIXlt? Documentation: 'Character input is first converted to class "POSIXlt" by strptime: numeric input is first converted to "POSIXct". Any conversion that needs to go between the two date-time classes requires a time zone: conversion from "POSIXlt" to "POSIXct" will validate times in the selected time zone.'
lt.ints$hour = lt.ints$hour - 4 # Shifting 4 hours to the left makes the graph look perfect.
lt.ints
backints = as.POSIXct(lt.ints)

# NOW TO CREATE THE DF FOR THE PLOT, AND THE PLOT:
newdata = data.frame(tallies = colSums(M),
                     times = backints)

ggplot(newdata, aes(x = times, y = tallies)) +
  geom_bar(stat = "identity", fill = "#99CCFF") + 
  theme_bw() +
  scale_x_datetime(breaks = date_breaks("8 hours"),
                   limits = c(as.POSIXct("2003-03-18 21:00:00"), 
                              as.POSIXct("2003-03-24 21:00:00"))) +
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9)) +
  ggtitle("Cases of sickness following the birthday party") + 
  ylab("Cases") + xlab("")

ggplot(outbreak, aes(x = intervals, y = tallies)) +
  geom_bar(stat = "identity", fill = "#99CCFF") + 
  theme_bw() +
  scale_x_datetime(breaks = date_breaks("8 hours"),
                   limits = c(as.POSIXct("2003-03-18 21:00:00"), 
                              as.POSIXct("2003-03-24 21:00:00"))) +
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9)) +
  ggtitle("Cases of sickness following the birthday party") + 
  ylab("Cases") + xlab("")
######################################################################################
######################################################################################







######################################################################################
######################################################################################

####################### Time-series outbreak barplot practice ########################


out = read.csv("/Users/jamescutler/Desktop/Epidemiology_I/outbreak.csv", header = FALSE)

# This dataset gives us a %d-%b date
# It also gives us a time %I:%M %p but with periods

colnames(out) = c("ID","Name","Sex","Age","Ill","D_onset","T_onset",
                  "Hot_dog",
                  "Chicken",
                  "Baked_po",
                  "Roll",
                  "Green_b",
                  "Corn",
                  "Salad",
                  "Ice_cream",
                  "Choc_cake",
                  "Banana_s_c",
                  "Ice",
                  "Kool_aid",
                  "Coke",
                  "Sprite")
head(out,4)

raw.data.to.RR.table = function(dfChar,illcolChar,colStart,colEnd,Yindicator,Nindicator){
  NumofExp = colEnd-colStart + 1
  iminus = colStart-1
  dfGood = eval(parse(text = dfChar))
  myindex = eval(parse(text = paste(dfChar,illcolChar,sep = "$"))) 
  ill = dfGood[which(myindex == Yindicator),]
  well = dfGood[which(myindex == Nindicator),]
  rrtable = data.frame(matrix(NA, nrow = NumofExp, ncol = 9))
  colnames(rrtable) = c("ill_exp","well_exp","total_exp","attack.rate_exp",
                        "ill_unexp","well_unexp","total_unexp","attack.rate_unexp",
                        "risk.ratio")
  rownames(rrtable) = colnames(dfGood)[colStart:colEnd]
  for (i in colStart:colEnd){
    rrtable[i-iminus,1] = length(which(ill[,i] == Yindicator)) # df's exposures are columns (on rhs) and rrtable's exposures are rows (on lhs)
  }
  for (i in colStart:colEnd){
    rrtable[i-iminus,2] = length(which(well[,i] == Yindicator))
  }
  for (i in colStart:colEnd){
    rrtable[i-iminus,5] = length(which(ill[,i] == Nindicator))
  }
  for (i in colStart:colEnd){
    rrtable[i-iminus,6] = length(which(well[,i] == Nindicator))
  }
  rrtable[,3] = rrtable[,1] + rrtable[,2] # total (exposed)
  rrtable[,4] = rrtable[,1] / rrtable[,3] # attack rate (exposed)
  rrtable[,7] = rrtable[,5] + rrtable[,6] # total (unexposed)
  rrtable[,8] = rrtable[,5] / rrtable[,7] # attack rate (unexposed)
  rrtable[,9] = rrtable[,4] / rrtable[,8] # RR
  return(rrtable)
}
rrtable = raw.data.to.RR.table(dfChar="out",illcolChar = "Ill",colStart = 8,colEnd = 21,
                     Yindicator = "Y",Nindicator = "N")

# Bar plot of risk ratios:
barplot(rrtable$risk.ratio, names.arg = rownames(rrtable), las = 2, col = "red")

ggplot(rrtable, aes(1:nrow(rrtable),risk.ratio)) +
  geom_bar(stat = "identity", fill = "red") +
  scale_x_continuous(breaks = seq(1,14,1),
                     labels = rownames(rrtable)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = .9, vjust = .9),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major.y = element_line(colour = "gray")) +
  labs(title = "Risk Ratios for each food", x="", y="Risk Ratio")


# 8 STEPS TO GET MY OUTBREAK PLOT:

# Since from here on out I'm just using the people who got ill, 
## I'll create a new dataframe with just them (and cut out a lot of other columns):
ill = out[which(out$Ill == "Y"),c(1,2,5,6,7)]

# Create a new column with the date and time combined, and then converted to a POSIXct:
ill$D_onset = as.character(ill$D_onset)
ill$T_onset = as.character(ill$T_onset)
ill$date.time = paste(ill$D_onset,ill$T_onset, sep = " ")
ill$date.time = gsub("\\.","",ill$date.time)
head(ill,3)
ill$date.time = strptime(ill$date.time, "%d-%b %I:%M %p") # makes the date and time usable
year(ill$date.time) = 2003 # class should be "POSIXlt" "POSIXt"
ill$date.time = as.POSIXct(ill$date.time) # Why do we want it since the UNIX epoch?

# Create time intervals as bins and convert to numeric: 
# (first just conver the times to numeric):
numtimes = as.numeric(ill$date.time)
# How many seconds are in 8 hours? - That's going to be my interval in the breaks:
mybreaks = seq(numtimes[1],numtimes[length(numtimes)]+28800,28800)
hist(numtimes, breaks = mybreaks) # LOOKS EXACTLY THE WAY MY PLOT WILL LOOK.

## First of all, how do we know when to start the bins, and end them? 
## Let's sort the data from earliest to latest:
sort(ill$date.time) # So, let's start on March 19, around 3 AM, and end on the 25th:
intervals = seq.POSIXt(ISOdatetime(2003,03,19,2,59,59, tz = "UTC"),
                       ISOdatetime(2003,03,25,2,59,59, tz = "UTC"),
                       by = "8 hours")
numintervals = as.numeric(intervals)

# Manual histogram code:
M = data.frame(matrix(NA, nrow = length(numtimes), ncol = length(numintervals)-1))
for (i in 1:length(numtimes)){
  for (j in 2:length(numintervals)){
    M[i,j-1] = ifelse(between(numtimes[i],lower=numintervals[j-1],upper=numintervals[j]),1,0)
  }
}
sum(colSums(M))

# Subtract 4 hours to make the bars in the plot fit between and not on the intervals:
intervals = as.POSIXct(intervals) # WHY IS THIS STEP NECESSARY? WHY NOT JUST CONVERT STRAIGHT TO lt?
lt.intervals = as.POSIXlt(intervals)
lt.intervals$hour = lt.intervals$hour - 4
lt.intervals
new.intervals = as.POSIXct(lt.intervals)

# Data frame with an extra colSum of 0 added at the end to match the number of intervals:
outbreak = data.frame(tallies = c(colSums(M),0),
                      intervals = new.intervals)

# Plot:
ggplot(outbreak, aes(intervals,tallies)) +
  geom_bar(stat = "identity", fill = "#99CCFF") +
  scale_x_datetime(breaks = date_breaks("8 hours"),
                   limits = c(as.POSIXct("2003-03-18 21:00:00"),
                              as.POSIXct("2003-03-24 24:00:00")),
                   labels = date_format("%d-%b %I-%p")) +
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "lightgray"),
        axis.line = element_line(colour= "black")) +
  labs(title = "Cases of sickness following the birthday party", x="",y="Number of cases")

####### GUESS WHAT THOUGH!?!? ALL OF THIS MANUAL COUNTS-CREATION CAN BE AVOIDED SIMPLY 
####### BY DOING THIS: hist(numtimes, breaks = mybreaks, plot = FALSE)$counts




################################################################
################################################################

# Crude, specific, and adjusted (standardized) rates

UK = 66e6
Japan = 126e6
Germany = 83e6
France = 67e6
Finland = 5.5e6
Norway = 5e6
Sweden = 10e6
Denmark = 5.7e6
Belgium = 11e6
Ireland = 4.7e6
Italy = 60e6
Austria = 9e6
Canada = 25e6
Netherlands = 17e6
Switzerland = 8e6


OECD = "UK = 66e6
Japan = 126e6
Germany = 83e6
France = 67e6
Finland = 5.5e6
Norway = 5e6
Sweden = 10e6
Denmark = 5.7e6
Belgium = 11e6
Ireland = 4.7e6
Italy = 60e6
Austria = 9e6
Canada = 25e6
Netherlands = 17e6
Switzerland = 8e6"
oecd = unlist(strsplit(OECD, "\n"))
oecd = gsub("\\d","",oecd); oecd
oecd = gsub("[[:punct:]]","",oecd); oecd
oecd = gsub(" e","",oecd); oecd
oecd = gsub(" ","",oecd); oecd
oecd = paste(oecd,collapse = "+"); oecd     # genius

oecd1 = eval(parse(text = oecd))

prettyNum(oecd1, big.mark = ",", scientific = FALSE)

.021*75000





