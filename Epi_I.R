### Epidemiology I ###

# %d     day as a number (0-31)     01-31
# %a     abbreviated weekday        Mon
# %A     unabbreviated weekday      Monday

# %m     month (00-12)              00-12
# %b     abbreviated month          Jan
# %B     unabbreviated month        January

# %y
# %Y
################################################################
# %H    hours as decimal number (01-24)
# %I    hours as decimal number (01-12 or 1-12)
# %M    minute as decimal number (00-59 or 0-59)
# %S    second as decimal number (00-59 or 0-59)
# %p    AM/PM indicator, used in conjunction with %I and not %H


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
# FOR FUN CREATE A PLOT TO *VISUALIZE* HOW HIGH BANANA SPLIT'S RISK RATIO IS:
ggplot(foods, aes(1:14,risk.ratio)) +
  geom_point(col = "red") +
  scale_x_continuous(breaks = seq(1,14,1), labels = rownames(foods)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9))

# NOW CREATE THE TIME SERIES BAR PLOT:
library(lubridate)

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

########################## WHERE THE COOL STUFF HAPPENS: #############################
######################################################################################
library(scales)
library(ggplot2)
library(dplyr) # for the 'between' function
# CREATE THE SEQUENCE OF TIME INTERVALS TO ACT AS THE WALLS OF THE HISTOGRAM BINS:
ints = seq.POSIXt(ISOdatetime(2003,03,19,3,0,0, tz = "UTC"), 
                  ISOdatetime(2003,03,25,3,0,0, tz = "UTC"), 
                  by = "8 hours")      
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
# counts = vector()
# for (i in 2:length(nints)){
#   counts[j] = ifelse(between(ntt[1],nints[j-1],nints[j]),1,0) # THIS IS BECAUSE FOR SOME REASON THE FIRST COLUMN OF 'M' WAS NAs      
# }
# M[,1] = na.omit(counts)
M[,1] = 0 # Instead of doing the for loop above, you could just know that that column is all zeros and do it this way.  
colSums(M)
length(colSums(M)) # WHY DID IT CREATE AN EXTRA EVENT OUT OF THIN AIR????
M[,4] = 0 # Now I have to remove the extra event.
# TO SHIFT BARS TO THE LEFT (they would be too far to the right otherwise):
class(ints)
lt.ints = as.POSIXlt(ints)
lt.ints$hour = lt.ints$hour - 4 # Shifting 4 hours to the left makes the graph look perfect.
lt.ints
backints = as.POSIXct(lt.ints)
# NOW TO CREATE THE PLOT:
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
######################################################################################
######################################################################################











##########################################################################################
k = matrix(1:5, nrow = 5, ncol = 1, byrow = TRUE)
l = matrix(1:3, nrow = 1, ncol = 3, byrow = TRUE)
m = matrix(NA, nrow = 5, ncol = 3, byrow = TRUE)
for (i in 1:5){
  for (j in 1:3){
    m[i,j] = k[i,1]*l[1,j]
  }
}
m
k%*%l
# Looks like crap (histogram bars lack vertical proportions), 
# but at least the x axis tick labels are right!!! And why does it take 30 seconds!?!?!?
# ggplot(ill, aes(x = date.time)) +
#   geom_histogram(binwidth = 8, color = "green") +
#   scale_x_datetime(breaks = date_breaks("8 hours"),
#                    limits = c(as.POSIXct("2003-03-18 21:00:00"), as.POSIXct("2003-03-24 21:00:00"))) +
#   theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9))

## LOOKS WEIRD (BECAUSE THIS METHOD IS FOR QUANTITIES ALREADY SPECIFIED--NOT TO BE TALLIED UP), 
## BUT NOT TOTALLY USELESS DATA VISUALIZATION:
# ggplot(out, aes(date.time,ill.count)) +
#   stat_summary(fun.y = sum, geom = "bar") +
#   scale_x_datetime(labels = date_format("%m-%d %I:%M %p"),
#                    date_breaks = "8 hours")

# EXPERIMENT IN xts ... it worked up to a certain point. Still not out of the tunnel. xts sucks.
# library(xts)
# library(zoo)
# 
# econ2 = economics
# epop = econ2[,c(1,3)]
# class(epop)
# epop = as.data.frame(epop)
# xtsible(epop)
# class(epop)
# popx = xts(epop[,-1,drop = FALSE], order.by = epop[,1])
# head(popx,10)
# # newpop = as.xts(epop, dateFormat = "Date Time")
# ep = endpoints(popx, on = "years", k = 1)
# ?period.apply
# period.apply(popx[,"pop"], INDEX = ep, FUN = mean)
# mean(epop[7:18,2])
# 
# 
# cases = ill[,22:23]
# xcase = xts(cases[,-1, drop = FALSE], order.by = cases[,1])
# head(xcase,10)
# cp = endpoints(xcase, on = "hours", k = 8)
# freqs = period.apply(xcase[,"cases"], INDEX = cp, FUN = sum)
# 
# ggplot(freqs, aes(x = Index, y = cases)) +
#   geom_bar(stat = "identity") + 
#   theme_bw() +
#   scale_x_datetime(breaks = date_breaks("8 hours"),
#                    limits = c(as.POSIXct("2003-03-18 21:00:00"), as.POSIXct("2003-03-24 21:00:00"))) +
#   theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9))
##########################################################################################



