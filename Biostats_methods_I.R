### Biostatistics Methods I

## Exam I - Thursday 27 September - Chapters 1-5
# Look over review problems for this test. 
# Review at 2:30-5:30 on Tuesday 25 September

# Ch. 1 - 6, 7

# Ch. 2 - 2.3.1, 2.3.6
#       - 2.5.1, 2.5.4;                    RQ 12, 14

# Ch. 3 - 3.4.4, 3.4.5, 3.4.6, 3.4.7;      RQ 4, 8

# Ch. 4 - 4.3.1, 4.3.2, 4.3.3a&b, 4.3.8
#       - 4.6.2, 4.6.7, 4.6.10, 4.6.14
#       - 4.7.2, 4.7.4;                    RQ 16, 27, 35

# Ch. 5 - 5.3.4, 5.3.6
#       - 5.5.2, 5.5.4;                    RQ 11, 12, 17, 18, 21

# Ch. 6 - 6.3.1, 6.3.2, 6.3.4
#       - 6.4.6
#       - 6.5.4
#       - 6.6.4;                           RQ 19, 26, 30

# Ch. 7 - 7.2.1, 7.2.4, 7.2.7, 7.2.17, 7.2.18, 7.2.19
#       - 7.3.2, 7.3.6, 7.3.8, 7.3.10 
#       - 7.4.3, 7.4.4
#       - 7.5.2, 7.5.4, 7.6.2, 7.6.4
#       - 7.7.2, 7.7.4, 7.8.4, 7.8.5;      RQ 18-31 (just ID which test and hypotheses)

# Ch.12 - 12.3.6
#       - 12.4.2, 12.4.4
#       - 12.5.2, 12.5.4;                  RQ 18, 20, 22, 23, 24-28

# Ch. 9 - 9.7.4, 9.7.6 (10 step hypothesis test; don't do CI for rho)
#       - 9.3.6, 9.3.7 (a through e)
#       - 9.4.1



## Always start with descriptive summaries, THEN go to hypothesis testing.

## The standard error of the mean is always smaller than standard deviation. 
# Why do some researchers report s.e.m. instead of s.d.? To present a tighter
# data set? S.e.m. helps show more clearly a difference between two different
# groups. With s.d. there's more spread for each group, thus more overlap
# between them, so researchers like to use s.e.m. to clear that apparent
# similarity up by reducing that overlap.
# STANDARD ERROR OF MEAN = SD/SQRT(n)

## When data are skewed right or left, use IQR, etc. When not, use mean & sd.
# WHEN YOU SEE OUTLIERS IN A BOXPLOT/SEMI-BOXPLOT THEN REPORT THE MEDIAN!
## If you want to tell how skewed a data set is, compare mean and median duh!!

## When would cumulative frequency be useful? If you have an ordinal response!
# E.g. satisfaction ranking ???
## Don't confuse cumulative frequency (out of total n) and cumulative relative 
# frequency (out of one).

## It's not appropriate to assign numerical values to ordinal data like satisfaction:
# e.g. You could ask people to self-evaluate their general health as poor, fair, good,
# very good, and excellent. Could you assign numbers to those ordinal values? Poor = 1,
# fair = 2, good = 3, etc.? NO. THAT IS VERY BAD SCIENCE. It makes no sense to think of
# fair as twice as good as poor (2 = 1*2) and 2/3 as good as good, etc. That's what happens
# when you try to convert qualitative data into quantitative data. Not a good idea.

## What makes the female-male smoker graph from class bad? The bottom cut off was obvious 
# to me. I didn't even realize the other thing that should have been obvious, that the "no" 
# bars were totally unnecessary! Duh!! Haha that is funny.


##########################################################################################


# CHAPTER 2

# Ch. 2 - 2.3.1, 2.3.6
#       - 2.5.1, 2.5.4;                    RQ 12, 14

# RQ 12:
wchair = data.frame(control = c(131, 115, 124, 131, 122, 117, 88, 114, 150, 169),
                    SCI =     c(60, 150, 130, 180, 163, 130, 121, 119, 130, 148),
                    rnums = 1:10)

plot(wchair$rnums,wchair$control, ylim = c(50,190))
points(wchair$rnums,wchair$SCI, col = "red")

mean(wchair$control)
median(wchair$control)
var(wchair$control)
sqrt(var(wchair$control)); sd(wchair$control)

mean(wchair$SCI)
median(wchair$SCI)
var(wchair$SCI)
sd(wchair$SCI)

library(tidyr)
wc.long = gather(wchair, key = "group", value = "ischial_pressure", c(control,SCI))
wc.long$group = as.factor(wc.long$group)
plot(wc.long$ischial_pressure ~ wc.long$group)
wc.long$gnums = c(rep(1,10),rep(2,10))
plot(wc.long$gnums,wc.long$ischial_pressure, xlim = c(0,3))
# mod1 = aov(ischial_pressure ~ group, data = wc.long); summary(mod1) # ONLY TWO GROUPS!!
t.test(wchair$control, wchair$SCI)
t.test(wchair$control, wchair$SCI, conf.level = .75)
t.test(wchair$control, wchair$SCI, conf.level = .5)

# Leave out outlier from SCI group, and corresponding 1st obs. from control:
wc.out = data.frame(control = c(115, 124, 131, 122, 117, 88, 114, 150, 169),
                    SCI =     c(150, 130, 180, 163, 130, 121, 119, 130, 148),
                    rnums = 1:9)

plot(wc.out$rnums,wc.out$control, ylim = c(50,190))
points(wc.out$rnums,wc.out$SCI, col = "red")

mean(wc.out$control)
median(wc.out$control)
var(wc.out$control)
sqrt(var(wc.out$control)); sd(wc.out$control)

mean(wc.out$SCI)
median(wc.out$SCI)
var(wc.out$SCI)
sd(wc.out$SCI)

out.long = gather(wc.out, key = "group", value = "ischial_pressure", c(control,SCI))
out.long$group = as.factor(out.long$group)
plot(out.long$ischial_pressure ~ out.long$group)
out.long$gnums = c(rep(1,9),rep(2,9))
plot(out.long$gnums, out.long$ischial_pressure, xlim = c(0,3))

t.test(wc.out$control, wc.out$SCI) # Still not significant!! But much closer!! Fascinating.

###########################################

# RQ 14:
deaths = c(393, 514, 460, 341, 365, 616, 618)
plot(1:7, deaths)
plot(1:7, deaths, ylim = c(0,650))
ages = c("25-34","35-44","45-54","55-64","65-74","75-84","85-94")
df = data.frame(X = as.factor(1:7), Y = deaths) # THE 'as.factor(1:7)' IS KEY! OTHERWISE THE SCALE_X_DISCRETE LABLES DON'T PRINT
ggplot(data = df, mapping = aes(x = X, y = Y)) +
  geom_point() +
  coord_cartesian(ylim = c(0,650)) +
  scale_x_discrete(labels = ages)

cum.freq = cumsum(deaths)

rel.freq = vector()
for (i in 1:length(deaths)){
  rel.freq[i] = deaths[i]/max(cum.freq)
}
rel.freq

cum.rel.freq = vector()
for (i in 1:length(cum.freq)){
  cum.rel.freq[i] = cum.freq[i]/max(cum.freq)
}
cum.rel.freq

df3 = data.frame(ages = ages, freq = deaths, 
                 rel_freq = rel.freq, cum_freq = cum.freq, cum_rel_freq = cum.rel.freq)
df3[,1:5]

mypermfunc = function(n,r){
  permu = factorial(n)/(factorial(n-r))
  print(prettyNum(permu, big.mark = ",", scientific = FALSE))
}
mypermfunc(52,5)
factorial(52)

##################################################################################
##################################################################################

### Thursday 30 August

## What kind of descriptive statistics is appropriate for categorical data?
# Counts and percentages instead of mean, median, spread.

### What concerns do we have regarding missing data?
## Selection bias.
## Concerned about lack of power. We wouldn't have sufficient information to 
# allow us to make inferences. Power = ability to detect associations or estimate
# precisely those parameters that we are interested in. 

## What would we want to present if we wanted to talk about premature y/n and smoking?
# Among those who smoked, how many were premature, among those who didn't, how many were?


NCbirths = read.csv("/Users/jamescutler/Desktop/Biostats_I/North_Carolina_Births.csv")
colnames(NCbirths) = c("ID",
                       "Plurality",
                       "Sex",
                       "M_age",
                       "G_age",
                       "M_status",
                       "Race",
                       "Ethnicity",
                       "Weight_gain",
                       "Smoker",
                       "Alcohol",
                       "W_ounces",
                       "W_grams",
                       "Low_W",
                       "Premature")
unique(NCbirths$Smoker)
unique(NCbirths$Race)
length(which(NCbirths$Race == 1))
length(which(NCbirths$Race == 2))
length(which(NCbirths$Race == 3))
length(which(NCbirths$Race == 4))
length(which(NCbirths$Race == 7)) # only 1 of these
length(which(NCbirths$Race == 8))

length(which(NCbirths$Sex == 1))
length(which(NCbirths$Sex == 2))

length(which(NCbirths$Plurality == 1))
length(which(NCbirths$Plurality == 2))/length(which(NCbirths$Plurality == 1)) # This is about the same as what the internet says the percentage of births are twin births

plot(NCbirths$G_age, NCbirths$W_grams)
w.by.a = lm(NCbirths$W_grams ~ NCbirths$G_age)
abline(w.by.a, col = "red")
# cor(NCbirths$G_age,NCbirths$W_grams) # THIS DOESN'T WORK BECAUSE THERE ARE NAs
which(is.na(NCbirths$G_age))
# theNAs = vector()
# for (i in 2:3){
#   theNAs[i] = which(is.na(NCbirths[,i]))
# } # DOESN'T WORK BECAUSE SOME (MOST) OF THEM ARE "integer(0)"
which(is.na(NCbirths[,1:15]))
as.matrix(colnames(NCbirths))
which(is.na(NCbirths$Weight_gain))
# ifelse(which(is.na(NCbirths[,2])) < 1,0,1) # doesn't give me what I was hoping for

############# HERE'S WHAT I'M LOOKING FOR: ############# 
NCnew = NCbirths[-which(is.na(NCbirths$Weight_gain)),]

# Predict birth weight based on gestational age:
plot(NCnew$G_age, NCnew$W_grams)
w.by.a = lm(NCnew$W_grams ~ NCnew$G_age)
abline(w.by.a, col = "red")
cor(NCnew$G_age, NCnew$W_grams) # .56 correlation coefficient



########################################################

length(which(NCbirths$Premature.Birth == 1))/nrow(NCbirths)
length(which(NCbirths$Smoke.During.Pregnancy == 1))




median(NCbirths$Maternal.Age..years.)

hist(NCbirths$Gestational.Age..weeks., breaks = 20, xlim = c(20,45))

length(which(NCbirths$Gestational.Age..weeks. <= 26))/nrow(NCbirths)


# Smokers vs non-smokers chance of premature birth:
sum(NCbirths[which(NCbirths$Smoke.During.Pregnancy == 0),15])/length(which(NCbirths$Premature.Birth == 0))
sum(NCbirths[which(NCbirths$Smoke.During.Pregnancy == 1),15])/length(which(NCbirths$Premature.Birth == 1))

mean(as.numeric(NCbirths$Gestational.Age..weeks.))
class(NCbirths$Gestational.Age..weeks.)
median(as.numeric(NCbirths[1:500,5]))
ages = c(40,37,39,39,39,43,39,42,41,39); mean(as.integer(ages))
NCbirths$Gestational.Age..weeks. = as.character(NCbirths$Gestational.Age..weeks.)
NCbirths$Gestational.Age..weeks. = as.numeric(NCbirths$Gestational.Age..weeks.)
mean(NCbirths$Gestational.Age..weeks.)
class(NCbirths$Gestational.Age..weeks.)
which(is.na(NCbirths$Gestational.Age..weeks.))
ages = NCbirths$Gestational.Age..weeks.
ages = ages[-214]
mean(ages)
median(ages)


ggplot(NCbirths, aes(factor(Premature.Birth),Gestational.Age..weeks.)) +
  geom_violin()

plot(NCbirths$Maternal.Age..years.,NCbirths$Gestational.Age..weeks.)

ggplot(NCbirths, aes(Maternal.Age..years.,Gestational.Age..weeks.)) +
  geom_point(alpha = .1) + 
  theme_classic()

ggplot(NCbirths, aes(factor(Smoke.During.Pregnancy),Gestational.Age..weeks.)) +
  geom_violin()

ggplot(NCbirths, aes(factor(Smoke.During.Pregnancy),Gestational.Age..weeks.)) +
  geom_boxplot()

plot(NCbirths$Maternal.Age..years. ~ factor(NCbirths$Smoke.During.Pregnancy))


ggplot(NCbirths, aes(factor(Low_W),W_grams)) +
  geom_violin()

plot(NCbirths$Weight_gain,NCbirths$W_grams)
reg.weight.gain = lm(NCbirths$W_grams ~ NCbirths$Weight_gain)
abline(reg.weight.gain, col = "red")
# Now, to get correlation:
w.gain = NCbirths$Weight_gain
which(is.na(w.gain))
w.gain2 = w.gain[-which(is.na(w.gain))]
w.grams = NCbirths$W_grams
which(is.na(w.grams)) # None
vec.rem = which(is.na(w.gain)); vec.rem
w.grams = w.grams[-vec.rem]; length(w.grams)
length(w.gain2)
length(w.grams)
cor(w.gain2,w.grams) # .222 correlation coefficient


ggplot(NCbirths, aes(factor(M_status),W_grams)) +
  geom_boxplot()

birth.weights.summary = summary(NCbirths$W_grams)
# Bingo! (except the max is on the bottom, and min on top):
as.matrix(birth.weights.summary)



##################################################################################
##################################################################################

### Thursday 6 September


# probability rules



##################################################################################
##################################################################################

### Thursday 13 September

### Binomial distribution 
# (there are a lot of applications for the binomial distribution in medicine - dead or alive, success or failure of treatment, etc.)

## Example: We are told the success rate of a given intervention is 45%. This intervention
# is tried on 8 people in an experiment. What is the probability that only 1 success is
# observed in this experiment?
dbinom(1,8,.45) # .055
# What is the probability that no successes are observed in this experiment?
dbinom(0,8,.45) # .0084
# What is the probability that 1 or fewer success are observed?
dbinom(1,8,.45) + dbinom(0,8,.45) # .063
# P(7 or more successes) = ?
dbinom(7,8,.45) + dbinom(8,8,.45) # .018
pbinom(6,8,.45, lower.tail = FALSE) # .018 - you have to drop it down to 6 because it doesn't include what you enter (leaves the 6 out, and just computes 7 and 8)

## A therapeutic agent has 90% response rate. What is the probability that 3 of the next
# 5 patients will respond?
dbinom(3,5,.9) # .0729
# You could also do:
dbinom(2,5,.1) # same answer: .0729

# Nerve repair laser treatment study:

x = rbinom(1000,100,.15); x
hist(x)
length(which(x >= 24))/length(x)
pbinom(23,100,.15, lower.tail = FALSE)


## Practice problems:

# 1. The probability that a person suffering from migraine headaches will
# obtain relief with a particular drug is 0.73. Five randomly selected
# sufferers from migraine headaches are given the drug. Find the
# probability that the number obtaining relief will be:

# a) exactly 3

# b) at least 4

# c) find the mean and variance of this distribution


# 2. Suppose it is known that in a certain population 10 percent of the
# population is color blind. If a random sample of 15 people is drawn from
# this population, find the following probabilities:

# a) One or fewer will be color blind (use the formula)

# b) Exactly 2 will be color blind (use the formula)


# 3. For ages between 18 and 24 years, serum cholesterol levels have a mean
# of 178.1 and a standard deviation of 40.7.

# a) Find the z score corresponding to a male who has a serum cholesterol
# level of 250.

# b) Is this level unusually high?


# 4. Suppose the ages at time of onset of a certain disease are approximately
# normally distributed with a mean of 11 and a standard deviation of 3
# years. A child has just come down with the disease. What is the
# probability that the child is:

# a) Between the ages of 8 and 14?

# b) Over 10 years of age?


# 5. Given a normally distributed population with a mean of 75 and a variance
# of 625 find:

# a) P(50 ≤ x ≤ 110)

# b) P(X>90)

# c) P(X>x)=.10 (Find x)


























