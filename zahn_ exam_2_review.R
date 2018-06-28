######## TEST REVIEW for EXAM II BIOL-490 ##########

library(tidyr)

x = rnorm(10)
y = rnorm(10)
z = rnorm(10)
obs = 1:10
df = data.frame(obs = obs, x = x, y = y, z = z)
df.long = gather(df, key = "stock", value = "PriceChange", c("x","y","z")) # But x y and z don't have to be in quotes!!!!!

spread(df.long, stock, PriceChange) # undoes gather (they're opposites) 

aov1 = aov(PriceChange ~ stock*obs, data = df.long)
summary(aov1)

TukeyHSD(aov1) # obs is not a factor! That's the warning. But it will still do pairwise comparisons
# for x y and z


library(dplyr)
df %>% mutate(total = x+y+z, minimum = apply(df[,2:4],1,min)) # I LEARNED SOMETHING REALLY IMPORTANT ABOUT APPLY!

select(df, c(x,y))
df %>% select(c(x,y)) # does the same thing! df is the first argument in select here.
df %>% filter(x>-.3 & x<.3) # that is so much more efficient than my "between" code!
df %>% filter(x>-.3 | x<.3) # the vertical bar is the "or" symbol. WHOA ... 
df %>% filter(x<-.3 | x>.3)
# !!!!!!!!!! select picks columns and filter picks rows !!!!!!!!!!!!
.21 > .3
.21 < -.3
df.long %>% group_by(stock) %>% summarise(mean.pchnge = mean(PriceChange,),
                                          sum.it = sum(PriceChange),
                                          min.it = min(PriceChange),
                                          N = n(),
                                          stdev = sd(PriceChange)) # ALL OF THIS IS HUGELY IMPORTANT!

my.iris = iris
my.iris %>% group_by(Species) %>% summarise(the.mean = mean(Petal.Length),
                                            the.var = var(Petal.Length),
                                            the.n = n(),
                                            the.median = median(Petal.Length))


df2 = iris
plot(df2) # just to get a bird's eye view
mod1 = aov(Petal.Length ~ Petal.Width*Species, data = df2)
mod2 = aov(Petal.Length ~ Petal.Width+Species, data = df2)
summary(mod1)
summary(mod2)
anova(mod1,mod2) # whoa ... interesting
library(ggplot2)
# A good money plot:
plot1 = ggplot(df2) + geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species)) + 
  geom_smooth(aes(x = Petal.Width, y = mod1, col = Species), method = "lm") + 
  geom_smooth(aes(x = Petal.Width, y = mod2, col = Species), method = "lm", linetype = 5)

library(modelr)
df2 = add_predictions(model = mod1, data = df2, var = "mod1")
df2 = add_predictions(model = mod2, data = df2, var = "mod2")
mean((df2$Petal.Length - df2$mod1)^2) # mod1 is .12544, better than mod2
mean((df2$Petal.Length - df2$mod2)^2) # mod2 is .13889
sqrt(mean((df2$Petal.Length - df2$mod1)^2)) # .35
sqrt(mean((df2$Petal.Length - df2$mod2)^2)) # .37

plot(df2$Petal.Length ~ df2$Species)

plot1 + stat_ellipse(aes(x = Petal.Width, y = Petal.Length, col = Species)) # HELL YEAH

ggplot(df2, aes(x = Species, y = Petal.Length, fill = Species)) + geom_violin() # PAY CAREFUL NOTICE,
# VIOLIN PLOTS, LIKE BOXPLOTS, DON'T WORK WITHOUT FACTORS ON THE X AXIS. HENCE CALLING SPECIES AS X









