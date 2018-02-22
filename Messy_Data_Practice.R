### 22 February 2018 - BIOL 490R messy data ###

# Always load up these three packages:
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)

df = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/data/BioLog_Plate_Data.csv")
levels(df$Sample.ID)
unique(df$Rep)
unique(df$Dilution)
levels(df$Substrate)

sub1 = subset(df, Substrate == "D-Cellobiose")

absorb = c(sub1$Hr_24,sub1$Hr_48,sub1$Hr_144)
the.rest = sub1[,-c(6,7,8)] # DON'T NEED TO DO THIS

?gather
sub1_long = gather(sub1, key = "Time", value = "Abs", c("Hr_24","Hr_48","Hr_144"))

which(sub1_long$Time == "Hr_24")
?plyr::mapvalues
sub1_long$Time = plyr::mapvalues(sub1_long$Time, from = c("Hr_24","Hr_48","Hr_144"), to = c(24,48,144))

sub1_long$Time = as.numeric(sub1_long$Time)

# NOW if I plot it, the time will be an actual number, and it will not be out of order any more
ggplot(sub1_long, aes(x = Time, y = Abs, col = Sample.ID)) + 
  geom_point() + stat_smooth()

# Now do it to the whole df data frame:
df_long = gather(df, key = "Time", value = "Abs", c("Hr_24","Hr_48","Hr_144"))
df_long$Time = as.numeric(plyr::mapvalues(df_long$Time, from =c("Hr_24","Hr_48","Hr_144"), 
                                          to = c(24,48,144)))


sbstrt.lvls = levels(df_long$Substrate)

df_short = df_long[which(df_long$Substrate == c("L-Serine","L-Arginine","D-Xylose")),] # HOLY FREAK this did not work
df_short2 = subset(df_long, Substrate %in% c("L-Serine","L-Arginine","D-Xylose")) # This did work!

# YAY THIS FOR LOOP FREAKING WORKS!!! ALL BECAUSE I ACTUALLY TOLD IT TO FREAKING PRINT! 
for (i in levels(df_long$Substrate)){
  sub1 = subset(df_long, Substrate == i)
  stuff = ggplot(sub1, aes(x = Time, y = Abs, col = Sample.ID)) +
    geom_point() + stat_smooth() + ggtitle(i)
  print(stuff)
}


# This is money: 
substrates = levels(df_long$Substrate)
substrate.plot = function(x){
  ggplot(df_long[df_long$Substrate == x,], aes(x = Time, y = Abs, col = Sample.ID)) + geom_point() +
    stat_smooth() + ggtitle(x)
}
lapply(substrates,substrate.plot) # The vector or list you want to apply the function to
# goes first, then the function (already stored in R, or created and called previously by 
# you), goes next. Just like in tapply! 






