rm(list = ls())

# Load the libraries that you use here:

library(ggplot2)
library(tidyr)
library(dplyr)
library(fitdistrplus)
library(modelr)



############# Part 1 - Preparing wide data ################## ---------------- (30 points possible)

# read in salaries.csv
# This is faculty salary information from 1995 - Split up by university, state, faculty rank, and university tier
salaries = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/exam2/salaries.csv")



# convert to usable format so we can look at salaries as a dependent variable (10 points)
sal.long = gather(salaries, key = "rank", value = "salaries", c(AssistProf,AssocProf,FullProf))



# create boxplot of salary by University Tier, colored by Faculty Rank (10 points)
# x-axis = Tier
# y-axis = Salary
# Boxplot fill color = Rank
# Title = "Faculty Salaries - 1995"
colnames(sal.long)
class(sal.long$Tier)
plot(sal.long$salaries ~ sal.long$Tier, main = "Faculty Salaries - 1995",
     xlab = "Tier", ylab = "Salaries") # Not the one you're looking for
ggplot(sal.long, aes(x = Tier, y = salaries, col = rank)) + geom_boxplot() + 
  ggtitle(label = "Faculty Salaries - 1995") + 
  xlab(label = "Tier") + ylab(label = "Salary") # This better be what you're looking for, because
# the wording of your instructions isn't specific enough to rule this out as a valid answer. Plus,
# it really seems like this is what you're asking for. But for whatever reason that I don't have
# time to explore during this test, it gives a warning message that says "removed 128 rows
# containing non-finite values"

# export this boxplot to a file in your personal repository named "LASTNAME_exam2_plot1.jpeg" (10 points)
jpeg(filename = "/Users/jamescutler/Desktop/Data_Course_cutler/CUTLER_exam2_plot1.jpeg")
ggplot(sal.long, aes(x = Tier, y = salaries, col = rank)) + geom_boxplot() + 
  ggtitle(label = "Faculty Salaries - 1995") + 
  xlab(label = "Tier") + ylab(label = "Salary")
dev.off()



################# PART 2 ################### ------------ (70 points possible)

# read in atmosphere.csv
# this data frame has microbial diversity values over time found in atmospheric observation station air filters
# sampling date and two environmental variables [CO2] and [Aerosols] are reported for each measurement
# "Diversity" is the dependent variable
atmos = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/exam2/atmosphere.csv")



# First, check whether your response variable is normally distributed (5 points)
# preliminaries:
hist(atmos$Diversity)
plot(density(atmos$Diversity))
mean(atmos$Diversity) # mean = 809.7 units

# checking normality:
qqnorm(atmos$Diversity)
plot(fitdist(atmos$Diversity, distr = "norm")) # Empirical theoretical looks good, the others
# not quite as good.
plot(fitdist(log10(atmos$Diversity), distr = "norm")) # The log10 transform doesn't really make
# any of the plots look better.
plot(fitdist(atmos$Diversity, distr = "logis")) # Overall looks about as good as norm.
# plot(fitdist(atmos$Diversity, distr = "gamma")) # throws an error (?)
# It looks "normal enough" based on what you taught us in class about what looks normal enough.



# Next, convert "Year" to a factor...just because (5 points)
colnames(atmos)
class(atmos$Year)
atmos$Year = as.factor(atmos$Year)
class(atmos$Year)


# Create a simple ANOVA model with "Year" as the only explanatory variable (5 points)
model.atmos1 = aov(atmos$Diversity ~ atmos$Year)
summary(model.atmos1)
# Aerosol is a continuous variable ...
qqnorm(atmos$Aerosol_Density)
hist(atmos$Aerosol_Density)

# Now, create an ANOVA model that incorporates "Year", "Aerosol_Density", and their interaction (5 points)
class(atmos$Aerosol_Density) 
model.atmos2 = aov(atmos$Diversity ~ atmos$Year*atmos$Aerosol_Density)
summary(model.atmos2)

# Compare the two models mean-squared difference method to see which is better at making predictions 
# (20 points)
atmos = add_predictions(model = model.atmos1, data = atmos, var = "model1")
atmos = add_predictions(model = model.atmos2, data = atmos, var = "model2")
# mean squared differences:
mean((atmos$Diversity - atmos$model1)^2)
mean((atmos$Diversity - atmos$model2)^2)
# the square root of the mean squared differences (you should give me extra credit for this):
sqrt(mean((atmos$Diversity - atmos$model1)^2)) # 533.4
sqrt(mean((atmos$Diversity - atmos$model2)^2)) # 70.6 - WAY better than model 1


# Export the summary ANOVA table of the better model to a text file in your repository named:
# "LASTNAME_exam2_table1.txt" (10 points)
summary(model.atmos2)
capture.output(summary(model.atmos2), file = "/Users/jamescutler/Desktop/Data_Course_cutler/CUTLER_exam2_table1.txt")
               
# use this model to predict what diversity should be for the following hypothetical conditions:
# note: only include the conditions that are part of your chosen model! (10 points)

# You never showed us how to do this, but I'll spend the last hour of this test searching
# the internet for a 15-yo substitute teacher's answer.

# My chosen model only has year and aerosol density:

#### Year = 2007
# Quarter = "Q4"
# Month = August
# Mday = 10
# BarcodeSequence = "CTCTCTATCAGTGAGT"
#### Aerosol_Density = 1000,
# CO2_Concentration = 384
new_data = c(2007,1000)
# prdct.on.new.data = predict(model.atmos2, new_data) # NOPE.

atmos2 = atmos[,c(3,8)] # training data frame
atmos3 = rbind(atmos2,c(2007,1000))
atmos4 = atmos3[299,] # crossing data frame
# cross.preds = add_predictions(model = model.atmos2, data = atmos4) # doesn't work either
# So I'll go with a linear model, which makes more sense, and use it to tell you what my 
# prediction is:
good.model = lm(atmos$Diversity ~ atmos$Aerosol_Density) # adding interaction with year would be too complicated
good.model
prediction = 2.612*1000 + 59.245
prediction # 2671.245

# Now you can't say I didn't try! 



# Now, make a pretty plot to the following specifications:
# x-axis = Day
# y-axis = Aerosol_Density
# point transparency based on values of "Diversity"
# Title: "Decadal Aerosol Density"
# Subtitle: "More aerosols contribute to greater microbial diversity in the atmosphere"
colnames(atmos)
ggplot(atmos, aes(x = Day, y = Aerosol_Density, alpha = Diversity)) +
  geom_point() + ggtitle(label = "Decadal Aerosol Density", 
                         subtitle = "More aerosols contribute to greater microbial diversity in the atmosphere")


# Save this plot in your repository as "LASTNAME_exam2_plot2.jpeg" (10 points)
jpeg(filename = "/Users/jamescutler/Desktop/Data_Course_cutler/CUTLER_exam2_plot2.jpeg")
ggplot(atmos, aes(x = Day, y = Aerosol_Density, alpha = Diversity)) +
  geom_point() + ggtitle(label = "Decadal Aerosol Density", 
                         subtitle = "More aerosols contribute to greater microbial diversity in the atmosphere")
dev.off()


#### When you are all finished, push the files, including this R script, onto your GitHub repo
#### I will look at your script and look for the three properly named files that you generated


