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
%in% # This does something cool

  
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
# e.g. that's not possible to integrate by hand:
func1 = function(x) ((3*x^3 - 4*x^2 + x -1)/((x^2 + 1)*(x^2 + 2)))
integrate(func1, lower = -30, upper = 1)
# Probability example (IQ score):
func.IQ = function(x) ((1/(15*sqrt(2*pi)))*exp((-(x-100)^2)/(2*15^2)))
integrate(func.IQ, lower = 140, upper = 250)
# Probability standard normal:
func.stand = function(x) ((1/sqrt(2*pi))*exp((-x^2)/2))
integrate(func.stand, lower = -2, upper = 2)


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


## tapply - example from rpm data (Stats 4100):
# Two methods for doing the same thing--calculating the means of the txs (true response):
for (i in 1:5){
  stuff = mean(with(rpm, subset(liters.minute, level.fac == i)))
  print(stuff)
}
rpm.mean.vector = tapply(rpm$liters.minute, rpm$level, mean); rpm.mean.vector
