### MASTER LIST OF ALL THE COOLEST R CODE I LEARN AND QUESTIONS I HAVE ###
# (Usually in alphabetical order)

# QUESTIONS:
## 1. What can you do about 2 or more data points in a scatter plot of x vs y 
# that are exactly the same? They look like a single data point--you can't 
# tell there's more than one data point there.
## 2. Is there a way to color a single data point in a scatter plot? Say I wanted
# to highlight the US in the chol vs HDI scatter plot by coloring it red or something.
## 3. Is there a way to pick a dot from a scatter plot and figure out which item it 
# represents? For example, say I see an outlier on the scatter plot. How can I find 
# out which item in a data.frame it represents?
## 4. How can I add a balloon color column with the four colors matching the numbers?



## Boxplots - How to create a boxplot by converting column values (e.g. years) to as.factor
# Example from Biol-490R:
DNA_Conc_blah2$year.fac = as.factor(DNA_Conc_blah2$year)
plot(DNA_Conc_blah2$year.fac, DNA_Conc_blah2$Katy, xlab = "YEAR", 
     ylab = "DNA concentration", main = "Katy's Extractions")
# Boxplot with colors in boxes (example from Stats 4100):
battery$TYPEBAT.fac = as.factor(battery$TYPEBAT)
color.list = c("pink","yellow","orange","blue")
plot(battery$TYPEBAT.fac, battery$LPUC, xaxt="n", xlab = "battery type", 
     ylab = "life per unit cost", col = color.list)
axis(1, at = 1:4, labels = color.list)
# Another way to do a boxplot (CO2 data set example):
boxplot(df$uptake ~ df$Treatment*df$Type, col = c("Lightgrey","White"))


## cbind vs rbind - binding columns vs binding rows
sport = c("Hockey", "Baseball", "Football")
league = c("NHL", "MLB", "NFL")
trophy = c("Stanley", "Comissioner's", "Lombardi")
bind.columns1 = cbind(sport, league, trophy); bind.columns1 # It makes sense!
bind.rows1 = rbind(sport, league, trophy); bind.rows1 # This makes sense too!


## CO2 data set - money code I learned from the CO2_instructor R script:
# A money boxplot idea (df = CO2):
boxplot(df$uptake ~ df$Treatment*df$Type, col = c("Lightgrey","White")) # THIS IS MONEY!
# THE ULTIMATE GGPLOTS:
p1 = ggplot(df, aes(x=conc, y=log10(uptake), col=Treatment)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake")
p1 # THIS ONE IS GOOD

p2 = ggplot(df, aes(x=conc, y=log10(uptake), col=Type)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake")
p2 # AND THIS ON IS GOOD, BUT ...

p3 = ggplot(df, aes(x=conc, y=log10(uptake), col=Treatment)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake") +
  facet_grid(facets = ~ Type)
p3 # ... THIS ONE IS THE MONEY PLOT OF ALL PLOTS! YOU CAN SEE THE DIFFERENCE BETWEEN CHILLED
# AND NON-CHILLED, ***AND*** THE DIFFERENCE BETWEEN QUEBEC AND MISSISSIPPI!
# TO BE TOTALLY CLEAR, THIS IS NOT A JUXTOPOSITION OF THE TWO PREVIOUS PLOTS--THE PREVIOUS
# TX-COLOR-CODED PLOT SHOWED *ALL* PLANTS, IN MISS AND QUEBEC, MIXED TOGETHER, AND THEIR
# RESPONSE TO TX. THE PLACE-COLOR CODED PLOT DID THE SAME--IT SHOWED *ALL* PLANTS MIXED
# TOGETHER, TREATED AND NON-TREATED (CHILLED AND NON-CHILLED), AND THEIR RESPONSE BASED
# ON PLACE. BUT THIS SHOWS HOW QUEBEC PLANTS RESPONDED TO TX VS NO TX, RIGHT NEXT TO
# MISS PLANTS' RESPONSE TO TX VS NO TX.


## Columns - How to add a column (example)
df$Total_Points = rowSums(df[,3:17])


## Column names - How to change them
# An example from the cholesterol data:
colnames(M2) <- c("Country","m.t.chol","HDI","meat","milk","eggs","fish","an.fats")
names(M2)


## Cool code (miscellaneous):
# %in% # This does something cool


## Draw function plots:
curve((1/sqrt(2*pi))*exp((-x^2)/2), from = -4, to = 4,main = "the normal curve",xlab = "x",ylab = "y")
func.stand = function(x) ((1/sqrt(2*pi))*exp((-x^2)/2))
integrate(func.stand, lower = -5, upper = 2)
curve((3*x^3 - 4*x^2 + x -1)/((x^2 + 1)*(x^2 + 2)), from = -30, to = 10, n = 1000,
      xlab = "x", ylab = "y")
curve(x-x, from = -30, to = 10, add = TRUE)
# I can add the x axis but how do you add the y axis??? x=0 doesn't work
# or you could use plot.function() which works the same exact way


### expand.grid (example):
# Automatically create a data frame in a cool way (cyclic repeats of first vector):
dudes = expand.grid(height = seq(60,80,5), peso = seq(100,300,50), sex = c("male","female"))
# For every time the first sequence prints (60,65,70,75,80), the second sequence lines up a
# column of 100s. Then the first sequence repeats, this time lined up with five 150s. Then
# another cycle from 60 to 80 with 200s, etc. till it goes through the 300s. All of those
# are matched with a column of 25 males. Then 25 females (same thing in the first two columns).
## Another example:
de = expand.grid(x = 1:3, a = c("male","female","intersex")); de

### for loops - the mysteries and wonders of for loops:
## GET THE FREAKING OUTPUT OF A FOR LOOP FOR CRYING OUT LOUD AND FREAKING PUT IT IN A DATA FRAME!
# rbind/cbind and for loops are a weird mix:
dube = NULL
for (i in 1:10){
  what = c(1,2,3)
  butt = c(3,2,1)
  dube = rbind(dube, data.frame(what,butt)) # RBIND! IT WORKS!
}
dube # FOR SOME ODD REASON IT WORKS!
######################################
rollin = NULL
for (i in 1:10){
  tut = c(4,5,6)
  king = c(1,2,3)
  rollin = cbind(rollin, data.frame(tut,king)) # CBIND DOESN'T WORK!
}
rollin # DOESN'T FREAKING WORK!!?!!?!?!? WHY!!?????? IT'S BACKWARDS! RBIND DOES WORK, AND 
# BINDS WHAT AND BUTT INTO COLUMNS!!! CBIND DOESN'T WORK, AND APPARENTLY DOESN'T BIND
# THEM INTO COLUMNS!
## a for loop for creating cyclicly repeating values:
myfunc = function(){
  for (i in 1:10){
    print(1)
    print(2)
    print(3)
  } 
}
## compound interest for loops:
beg = 0
for (i in 1:12){
  beg = (beg + 1000)*1.06
  print(beg)
} # This gets the right result for compounded interest on 1000 added each month
for (i in 1:12){
  beg = 1000
  ultimo = beg*1.06^i
  print(ultimo)
} # Gives you the monthly breakdown of how much it grows with no added money after the initial 1000
1000*1.06^12

  
## ggplot - How to do a ggplot graph that's pretty okay
# An example from the thatch ant colonies data (dat, copied as dat2):
ggplot(dat2, mapping = aes(x = Headwidth, y = Mass, col = Colony)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)


## Integrals - How to integrate definite integrals
func = function(x) (x^2 + 2*x + 3)
integrate(func, lower = 0, upper = 2)
# e.g.
func5 = function(y) (2*pi*y*sqrt(1 + (1+3*y^2)^2))
integrate(func5, lower = 0, upper = 1)
# e.g. that's not possible to integrate by hand (actually, it is! Ha I'm an idiot):
func1 = function(x) ((3*x^3 - 4*x^2 + x -1)/((x^2 + 1)*(x^2 + 2)))
integrate(func1, lower = -30, upper = 1)
# Probability example (IQ score):
func.IQ = function(x) ((1/(15*sqrt(2*pi)))*exp((-(x-100)^2)/(2*15^2)))
integrate(func.IQ, lower = 85, upper = 115)
# Probability standard normal:
func.stand = function(x) ((1/sqrt(2*pi))*exp((-x^2)/2))
integrate(func.stand, lower = -2, upper = 2)


## Matrices - creating matrices
# Simple example:
n = 2
input = matrix(c(1,2,3,4,5,6), ncol = n, byrow = TRUE); input # fills up by row  
input2 = matrix(c(1,2,3,4,5,6), ncol = n, byrow = FALSE); input2 # fills up by column


## Messy_Data_Practice coolest things I learned:
# 1. Don't forget to load these:
library(ggplot2)
library(dplyr)
library(tidyr) # Needed for the gather() function
library(MASS)
####### BIGGEST LESSON OF ALL: HOW TO TURN SEPERATE COLUMNS INTO ONE COLUMN "KEY" WITH 
# VALUES IN THE COLUMN BESIDE THEM MATCHED TO THEIR "KEY":
df_long = gather(df, key = "Time", value = "Abs", c("Hr_24","Hr_48","Hr_144"))
# Then convert the characters to numerics with mapvalues (from the plyr library):
df_long$Time = as.numeric(plyr::mapvalues(df_long$Time, from =c("Hr_24","Hr_48","Hr_144"), 
                                          to = c(24,48,144)))
# How to plot shiz by substrate in that data set with 32 substrates:
# YAY THIS FOR LOOP FREAKING WORKS!!! ALL BECAUSE I ACTUALLY TOLD IT TO FREAKING PRINT!: 
for (i in levels(df_long$Substrate)){
  sub1 = subset(df_long, Substrate == i)
  stuff = ggplot(sub1, aes(x = Time, y = Abs, col = Sample.ID)) +
    geom_point() + stat_smooth() + ggtitle(i)
  print(stuff)
}
# How to do the same thing as the for loop with a function and lapply:
# This is money: 
substrates = levels(df_long$Substrate)
substrate.plot = function(x){
  ggplot(df_long[df_long$Substrate == x,], aes(x = Time, y = Abs, col = Sample.ID)) + geom_point() +
    stat_smooth() + ggtitle(x)
}
lapply(substrates,substrate.plot) # The vector or list you want to apply the function to
# goes first, then the function (already stored in R, or created and called previously by 
# you), goes next. Just like in tapply! 
###### How to create a smaller data frame with just a few substrates that I choose:
df_short = df_long[which(df_long$Substrate == c("L-Serine","L-Arginine","D-Xylose")),] # HOLY FREAK this did not work
df_short2 = subset(df_long, Substrate %in% c("L-Serine","L-Arginine","D-Xylose")) # This did work!


## Random number generator - How to generate random numbers within a range
# How to do decimals:
sprintf("%.2f",runif(1,min=.289,max=.91)) # fixes the number of decimal spaces to 2 only
# How to do integers:
?sprintf
?runif
runif(10,min=1,max=155) # This will NOT give you integers
?sample
sample(1:155, each=10)
sample(1:155,10) # THIS IS HOW YOU DO IT! SIMPLE!!! 10 RANDOM NUMBERS SELECTED BETWEEN 1 AND 155
# An example: 
trial1 = rnorm(100,mean = 23,sd = 10); trial1
trial1b = rnorm(50, mean = 27, sd = 10); trial1b
trial1c = c(trial1,trial1b); mean(trial1c)
trial1c
mean(c(mean(trial1),mean(trial1b)))
mean(trial1b)
mean(22.467+28.59)


## tapply - example from rpm data (Stats 4100):
# Two methods for doing the same thing--calculating the means of the txs (true response):
for (i in 1:5){
  stuff = mean(with(rpm, subset(liters.minute, level.fac == i)))
  print(stuff)
}
rpm.mean.vector = tapply(rpm$liters.minute, rpm$level, mean); rpm.mean.vector

#########







