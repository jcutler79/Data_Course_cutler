# MASTER LIST OF ALL THE COOLEST R CODE I LEARN AND QUESTIONS I HAVE

# QUESTIONS:
# 1. What can you do about 2 or more data points in a scatter plot of x vs y 
# that are exactly the same? They look like a single data point--you can't 
# tell there's more than one data point there.


# How to change column names
# An example from the cholesterol data:
colnames(M2) <- c("Country","m.t.chol","HDI","meat","milk","eggs","fish","an.fats")
names(M2)

# How to do a ggplot graph that's pretty okay
# An example from the thatch ant colonies data (dat, copied as dat2):
ggplot(dat2, mapping = aes(x = Headwidth, y = Mass, col = Colony)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## How to generate random numbers within a range
# How to do decimals:
sprintf("%.2f",runif(1,min=.289,max=.91))

# How to do integers:
?sprintf
?runif
runif(10,min=1,max=155) # This will NOT give you integers
?sample
sample(1:155, each=10)
sample(1:155,10) # THIS IS HOW YOU DO IT! SIMPLE!!! 10 RANDOM NUMBERS SELECTED BETWEEN 1 AND 155
