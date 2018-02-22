# Dr. Zahn's way of analyzing the mushroom growth data set:

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(modelr) # for add_predictions! Don't forget that!

df = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/data/mushroom_growth.csv")

glimpse(df)

ggplot(df, aes(x = GrowthRate)) + geom_histogram()

plot(fitdist(df$GrowthRate, distr = "norm"))

ggplot(df, aes(x = Nitrogen, y = GrowthRate, col = Species)) + geom_point() +
  facet_grid(facets = ~ Humidity) # He thinks of graphing stuff in this really helpful way first.
# Now add stat smooth:
ggplot(df, aes(x = Nitrogen, y = GrowthRate, col = Species)) + geom_point() + stat_smooth() + 
  facet_grid(facets = ~ Humidity) # Interaction between humidity and species (cornucopiae)

ggplot(df, aes(x = Light, y = GrowthRate, col = Species)) + geom_point() + stat_smooth() + 
  facet_grid(facets = ~ Humidity)


# Sequential sum of squares is the default method R uses; left-over error moved on to next item
# in sequence. So the sequence of terms entered in below matters? Weird.
my.shroom.model1 = aov(GrowthRate ~ Light*Species*Humidity*Nitrogen, data = df)
summary(my.shroom.model1)
# What I'm doing when I run a statistical test, is, I'm asking "Does this vary, and how much 
# does it vary?" It's showing us how unlikely/likely it is that this distribution is real. We're
# asking if it comes from a distribution which we specify.

P.cornu = subset(df, Species == "P.cornucopiae")
shroom.model2 = aov(GrowthRate ~ Light*Humidity*Nitrogen, data = P.cornu)
summary(shroom.model2)
# So far we have ignored temperature. We should add that in to a model later on!

ggplot(P.cornu, aes(x = Nitrogen, y = GrowthRate, col = Humidity)) + geom_point() + 
  stat_smooth(method = "lm") # WHY WON'T AN ANOVA CATCH THE NITROGEN INFLUENCE AT 20-25? BECAUSE 
# ANOVAs ARE JUST A BUNCH OF THESE LINEAR MODELS!!!! HOLY COW.

shroom.model3 = aov(GrowthRate ~ Light*Humidity, data = P.cornu)
anova(shroom.model2,shroom.model3) # Remember that anova in R compares two models!

new.stuff = data.frame(Light = 10, Humidity = "High")
predict(object = shroom.model3, newdata = new.stuff)

# Why WOULDN'T we want to use a non-linear model even when the relationship doesn't appear to be
# linear? Because we don't have enough data! Physicists can use non-linear models to predict things
# sometimes when they have trillions or quadrillions of data points! WTF!?!?!?!?

# Let's try a temperature model:
shroom.model4 = aov(GrowthRate ~ Light*Humidity*Temperature, data = P.cornu)
summary(shroom.model4)

shroom.model3.pred = add_predictions(P.cornu, model = shroom.model3)
shroom.model4.pred = add_predictions(P.cornu, model = shroom.model4)

mean((shroom.model3.pred$pred - shroom.model3.pred$GrowthRate)^2)
mean((shroom.model4.pred$pred - shroom.model4.pred$GrowthRate)^2)
# The mean squared difference between obesrved and predicted in model 4 is smaller (3630.006)

# Plot some temperature, humidity, and species facets.

# Cool idea:
hist(P.cornu$GrowthRate)
check.this = subset(P.cornu, GrowthRate > 500); check.this # This will show you that the Light level
# for all these values is always 20, the Nitrogen is between 20 and 35, the Humidity is always high, 
# and the Temperature is always 20. But there are only 3 mushrooms that grew to over 500 growth units
# anyway.

# ANOTHER REALLY COOL WAY TO SAVE SUMMARIES AND ANOVA TABLES AND WHATEVER CONSOLE OUTPUT YOU WANT
# AS A TXT FILE OR SOMETHING:
sink("/Users/jamescutler/Desktop/Data_Course_cutler/anova_table_mushrooms.txt")
summary(shroom.model4)
sink(NULL)

# Here's an important question: How do I graph light, temp, and humidity without using a 3D graph?
ggplot(P.cornu, aes(x = Light, y = GrowthRate, col = Humidity, shape = factor(Temperature))) +
  geom_point() + stat_smooth(method = "lm")

# or
ggplot(P.cornu, aes(x = Light, y = GrowthRate, col = Humidity, linetype = factor(Temperature))) +
  geom_point(alpha = .5, size = 4) + stat_smooth(method = "lm", se = FALSE) +
  labs(y = "Growth Rate", title = "Model 4") + theme_bw()

# or
ggplot(P.cornu, aes(x = Light, y = GrowthRate, col = Humidity, linetype = factor(Temperature))) +
  geom_jitter(width = 1, alpha = .5, size = 4) + stat_smooth(method = "lm", se = FALSE) + # jitter is cool
  labs(y = "Growth Rate", title = "Model 4") + scale_color_discrete(name = "Humidity") +
  scale_linetype_discrete(name = "Temperature", labels = c("1","2")) + # legend stuff
  theme_bw()

# This mushroom data set is very tidy and factorial. That's why it was easy to make these graphs with.
# gapminder is much hairier:

library(gapminder)
gpmder = gapminder

















