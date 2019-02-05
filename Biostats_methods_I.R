### Biostatistics Methods I

# grades
Q = 1
E1 = 1.02
E2 = .98
FINAL = .68
sum(20*Q,25*E1,25*E2,30*FINAL)


### Master list of basic functions for statistics:

## P value tests:
t.meandiff.pooled.var = function(xbar1,xbar2,n1,n2,var1,var2){
  sp.squared = ( var1*(n1-1) + var2*(n2-1) )/(n1 + n2 - 2)
  t.statistic = (xbar1-xbar2)/sqrt(sp.squared*(1/n1 + 1/n2))
  df = n1+n2-2
  pvalue = pt(t.statistic,df)
  print(sprintf("The p value is %.4f for left tail or %.4f for right tail",pvalue,1-pvalue))
  print(sprintf("The t test statistic is %.4f",t.statistic))
  print(sprintf("The pooled variance is %.4f",sp.squared))
}

## Confidence intervals:
t.CI.data.mean = function(df.data,t.CI){
  t.alpha = 1-t.CI
  xbar = mean(df.data)
  t = qt(p = 1-t.alpha/2, df = length(df.data)-1)
  se = sd(df.data)/sqrt(length(df.data))
  me = t*se
  ub = xbar + me
  lb = xbar - me
  print(sprintf("The sample mean is %.4f",xbar))
  print(sprintf("We are %s percent confident that the population mean is between %.4f and %.4f",t.CI*100,lb,ub))
}

CI.mean = function(CI,xbar,sd,n,t.or.z){
  alpha = 1 - CI
  p = 1 - alpha/2
  if (t.or.z == "t"){
    t = qt(p, df = n-1)
    se = sd/sqrt(n)
    me = t*se
    ub = xbar + me
    lb = xbar - me
    print(sprintf("We are %s percent confident that the population mean is between %.4f and %.4f",CI*100,lb,ub))
  } else{
    z = qnorm(p)
    se = sd/sqrt(n)
    me = sd/sqrt(n)
    ub = xbar + me
    lb = xbar - me
    print(sprintf("We are %s percent confident that the population mean is between %.4f and %.4f",CI*100,lb,ub))
  }
}

CI.propdiff = function(CI,prop1,prop2,n1,n2){
  alpha = 1 - CI
  z = qnorm(1 - alpha/2)
  me = z*sqrt( prop1*(1-prop1)/n1 + prop2*(1-prop2)/n2 )
  ub = (prop1 - prop2) + me
  lb = (prop1 - prop2) - me
  print(sprintf("We are %s percent confident that the true difference between the two population proportions is between %.4f and %.4f",CI*100,lb,ub))
}

t.CI.meandiff.pooled.var = function(CI,xbar1,xbar2,n1,n2,var1,var2){
  alpha = 1 - CI
  df = n1 + n2 - 2
  t = qt(1 - alpha/2, df)
  sp.squared = ( (n1 - 1)*var1 + (n2 - 1)*var2 )/(n1+n2-2)
  me = t*sqrt( sp.squared/n1 + sp.squared/n2 )
  ub = xbar1 - xbar2 + me
  lb = xbar1 - xbar2 - me
  print(sprintf("We are %s percent confident that the true difference between the two population means is between %.4f and %.4f",CI*100,lb,ub))
  print(sprintf("t is %.4f",t))
  print(sprintf("pooled variance is %s",sp.squared))
}

t.CI.data.meandiff.pooled.var = function(CI,df.data1,df.data2){
  alpha = 1 - CI
  n1 = length(df.data1)
  n2 = length(df.data2)
  df = n1 + n2 - 2
  t = qt(1 - alpha/2, df)
  sp = ( (n1 - 1)*var(df.data1) + (n2 - 1)*var(df.data2) )/(n1 + n2 - 2)
  me = t*sqrt( sp/n1 + sp/n2 )
  xbar1 = mean(df.data1)
  xbar2 = mean(df.data2)
  ub = xbar1 - xbar2 + me
  lb = xbar1 - xbar2 - me
  print(sprintf("We are %s percent confident that the true difference between the two population means is between %.4f and %.4f",CI*100,lb,ub))
}

## Book table of contents:
# Ch. 1 - intro
# Ch. 2 - descriptive statistics
# Ch. 3 - basic probability concepts
# Ch. 4 - probability distributions
# Ch. 5 - some important SAMPLING distributions: (xbar - mean)/s.e.
# Ch. 6 - estimation
# Ch. 7 - hypothesis testing
# Ch. 8 - analysis of variance
# Ch. 9 - simple linear regression and correlation
# Ch. 10 - multiple regression and correlation
# Ch. 11 - regression analysis: some additional techniques
# Ch. 12 - the chi-square distribution and the analysis of frequencies
# Ch. 13 - non-parametric and distribution-free statistics
# Ch. 14 - survival analysis

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



#######################################################################################
#######################################################################################
######################## NORTH CAROLINA BIRTH DATA EXPLORATION ########################

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



#######################################################################################
#######################################################################################

### Thursday 13 September:

### Binomial distribution 
# (there are a lot of applications for the binomial distribution in medicine - dead or alive, success or failure of treatment, etc.)

## Quiz questions on this material:

## We know 25% of adults are overweight. In a random sample of 20 adults, what are the chances
# that EXACTLY 3 are overweight?
?dbinom
dbinom(3,20,.25)
# Of 15 adults, the odds of getting EXACTLY 5:
dbinom(5,15,.25)
ncol(combn(15,5))*(.25^5)*(.75^10) # SAME ANSWER. SO THIS IS THE SLOW WAY
# Of 20 adults, the odds of getting EXACTLY 8 if the percent is 27% instead of 25:
dbinom(8,20,.27)
# 25%, 20 adults, odds of getting between 4 and 6 inclusive:
odds = vector()
for (i in 4:6){
  odds[i] = dbinom(i,20,.25)
}
sum(odds)
odds = odds[4:6]; odds
sum(odds)

## What is the probability that standard normal random variable z will be between
# -1.37 and .27
pnorm(.27) - pnorm(-1.37)

## Baby weighs greater than 8 pounds when average is 7.25 and stdev = 1 pound
?pnorm
pnorm(8.5, mean = 7.25, sd = 1, lower.tail = TRUE)

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


#######################################################################################
#######################################################################################

### EXAM I REVIEW PROBLEMS

## 1. - easy

## 2. make a frequency distribution table of the ages of 30 patients with ...
# frequency
# relative frequency
# cumulative frequency
# cumulative relative frequency

## Steps:
# create breaks using seq
# make your cuts using cut
# cbind a table of your cuts variable
# get your relative, cumulative, and cum rel frequencies with cnew[1:end]/N and cumsum
# create dataframe
ages = c(35,32,21,43,39,60,36,12,54,45,37,53,45,23,64,10,34,22,
                          36,45,55,44,55,46,22,38,35,56,45,57)
length(ages)
# breaks = seq(from = min(ages), to = max(ages), by = 10); breaks # doesn't work for this one, cuz it stops at 60 and there's a person aged 64 in the list who gets removed!
breaks = seq(10,70,10); breaks
mytable = cut(ages, breaks = breaks, right = FALSE, include.lowest = TRUE)
newtable = table(mytable)
sum(newtable[1:6])
cnew = cbind(newtable); cnew
rel.freq = cnew[1:6]/30; rel.freq # YOU WOULDN'T THINK THE [1:6] MATTERS, BUT IT DOES MAKE IT SO THE COLUMN NAME IS WHAT YOU WANT IT TO BE IN THE DATA FRAM BELOW, INSTEAD OF "newtable"
cumsum(rel.freq) # Cumulative relative frequency
cumsum(cnew) # Cumulative frequency
levels(mytable)
cnew[1:6]
freq.table = data.frame(Age_interval = levels(mytable),
                        Frequency = cnew[1:6],
                        Relative.Freq = rel.freq,
                        Cumulative.Freq = cumsum(cnew),
                        Cum.Rel.Freq = cumsum(rel.freq))

weights = c(62,53,57,55,69,64,60,59,60,60)
order(weights)
sort(weights)
median(weights)
ncol(combn(5,2))*(.1^2)*.9^3
.9914 - .9185

1 - pbinom(23, 100, prob = .15, lower.tail = TRUE)


### Some binomial distribution histograms:

rbinom(8,12,.27)
x = rbinom(10000,2,.5)
length(which(x == 0))
length(which(x == 1))
length(which(x == 2))
a = rbinom(100000,12,.27); a
hist(a, breaks = 10)
length(which(a >= 8))/length(a)


#######################################################################################
#######################################################################################

### Chapter 6 - ESTIMATION

## THE RULE OF THUMB FOR DECIDING WHETHER VARIANCES IN TWO SAMPLES ARE SAME OR NOT:
# IF Sone^2 / Stwo^2 IS LESS THAN 2 then they're the same.

## e.g. The population variance of the variable is known to be 45, variable is normal.
# A sample of 10 individuals yielded a mean of 22. What is the 95% CI for the true
# population mean?

n = 10
xbar = 22
qt(.95, 777)

# What is the margin of error? t-alpha or z-alpha * s.e., which is sd/sqrt(n)

## Practice using JMP:
NC = read.csv("/Users/jamescutler/Desktop/Biostats_I/North_Carolina_Births.csv")
colnames(NC) = c("ID",
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
for (i in 1:ncol(NC)){
  print(c(colnames(NC[i]),which(is.na(NC[,i]))))
}
bad = which(is.na(NC$Weight_gain))
NC = NC[-bad,]

hist(NC$Weight_gain)

xbar = mean(NC$Weight_gain); xbar
t = qt(.975,nrow(NC)-1); t # This is key. The t coefficient is t(1 - alpha/2). 
# If I want a 95% CI, then my alpha is .05, .05/2 is .025, so 1-.025 = .975!!! 
# The two tail area for this t(.975) is .05!!
se = sd(NC$Weight_gain)/sqrt(nrow(NC)); se
me.CI95 = t*se; me.CI95
ub = xbar + me.CI95; ub # gives the same answer as JMP!
lb = xbar - me.CI95; lb # gives the same answer as JMP!

CI.data = function(df.data,t.CI){
  t.alpha = 1-t.CI
  xbar = mean(df.data)
  t = qt(p = 1-t.alpha/2, df = length(df.data)-1)
  se = sd(df.data)/sqrt(length(df.data))
  me = t*se
  ub = xbar + me
  lb = xbar - me
  print(sprintf("The sample mean is %.4f",xbar))
  print(sprintf("We are %s percent confident that the population mean is between %.4f and %.4f",t.CI*100,lb,ub))
}
CI.data(NC$Weight_gain,.95)


#######################################################################################
#######################################################################################

### Chapter 6 quiz

## We wish to estimate the mean serum indirect bilirubin level of 4-day-old infants. 
# The mean for a sample of 14 infants was found to be 5.8 mg/100 cc and the standard 
# deviation was found to be 3.5 mg/100 cc for the sample. Find the 99% confidence 
# interval for the population mean. Assume a normal distribution for the serum indirect 
# bilirubin level.
p = 1-.01/2
t = qt(p, df = 13)
se = 3.5/sqrt(14)
me = t*se
ub = 5.8 + me; ub
lb = 5.8 - me; lb
## If it's a sample size of 20, with a 95% CI:
CI = .95
alpha = 1-CI
p = 1-alpha/2
n = 20
t = qt(p, df = n-1)
se = 3.5/sqrt(n)
me = t*se
ub = 5.8 + me; ub
lb = 5.8 - me; lb

CIquiz = function(CI,xbar,sd,n,t.or.z){
  alpha = 1 - CI
  p = 1 - alpha/2
  if (t.or.z == "t"){
    t = qt(p, df = n-1)
    se = sd/sqrt(n)
    me = t*se
    ub = xbar + me
    lb = xbar - me
    print(sprintf("We are %s percent confident that the population mean is between %.4f and %.4f",CI*100,lb,ub))
  } else{
    z = qnorm(p)
    se = sd/sqrt(n)
    me = sd/sqrt(n)
    ub = xbar + me
    lb = xbar - me
    print(sprintf("We are %s percent confident that the population mean is between %.4f and %.4f",CI*100,lb,ub))
  }
}

CIquiz(.99,5.8,3.5,20,"t")

## A sample of 10 twelve-year old girls had a mean weight of 80.0 pounds and a 
# standard deviation of 15 pounds (estimated from the data). What is the appropriate 
# reliability coefficient (t or z value), if we're interested in a 90% confidence 
# interval around the population mean weight? Assume a normal distribution for the weight.
p = 1 - .1/2; p
t = qt(p, df = 9); t

## Systolic blood pressure of 12 patients has a mean of 120 mm/Hg. If the population 
# variance of the systolic blood pressure is 90 and the SBP is approximately normally 
# distributed in the population, find the reliability coefficient for a 99% confidence 
# interval for a single population mean.
p = 1 - .01/2; p
qnorm(p)

## 
p = 1-.1/2; p
t = qt(p, df = 13); t


#######################################################################################
#######################################################################################

### Quiz for week 8 (variance and F and chi square):

p = 1-.02/2; p
t = qt(p,946); t
p = 1-.1/2; p
t = qt(p,587); t
sd1 = 22; v1 = sd1^2
sd2 = 14; v2 = sd2^2
v2/v1
v1/v2
29-2
p = 1-.01/2; p
t = qt(p,514); t


#######################################################################################
#######################################################################################

### JMP cereal - potassium

cereal = read.csv("/Users/jamescutler/Desktop/Biostats_I/cereal_JMP.csv")
unique(cereal$shelf)
length(which(cereal$shelf == 2))

shelf1 = cereal[which(cereal$shelf == 1),]
which(is.na(shelf1$potass))
shelf1 = shelf1[-which(is.na(shelf1$potass)),]

shelf2 = cereal[which(cereal$shelf == 2),]
shelf2 = shelf2[-which(is.na(shelf2$potass)),]

# mean and median for shelf 1:
median(shelf1$potass)
mean(shelf1$potass)

# mean and sd for shelf 2:
mean(shelf2$potass)
sd(shelf2$potass)

# t distribution 95% CI for shelf2 potassium mean:
CIquiz(.95,87.42857,sd(shelf2$potass),nrow(shelf2),"t")

# re-create shelf1 and 2 for hot and cold comparisons (leaving in the potassium NAs):
shelf1 = cereal[which(cereal$shelf == 1),]
shelf2 = cereal[which(cereal$shelf == 2),]
h1 = length(which(shelf1$type == "H"))/nrow(shelf1)
h2 = length(which(shelf2$type == "H"))/nrow(shelf2)

## 95% CI for difference between two proportions:
z = qnorm(1 - .05/2)
# ub1 = h1 + z*sqrt(h1*(1-h1)/nrow(shelf1)); ub1
# lb1 = h1 - z*sqrt(h1*(1-h1)/nrow(shelf1)); lb1
ub = (h1 - h2) + z*sqrt( h1*(1-h1)/nrow(shelf1) + h2*(1-h2)/nrow(shelf2) ); ub
lb = (h1 - h2) - z*sqrt( h1*(1-h1)/nrow(shelf1) + h2*(1-h2)/nrow(shelf2) ); lb

t.CI.meandiff.pooledvar = function(CI,xbar1,xbar2,n1,n2,var1,var2){
  alpha = 1 - CI
  df = n1 + n2 - 2
  t = qt(1 - alpha/2,df)
  sp = ( (n1 - 1)*var1 + (n2 - 1)*var2 )/(n1+n2-2)
  me = t*sqrt( sp/n1 + sp/n2 )
  ub = xbar1 - xbar2 + me
  lb = xbar1 - xbar2 - me
  print(sprintf("We are %s percent confident that the true difference between the two population means is between %.4f and %.4f",CI*100,lb,ub))
}

## 95% CI for difference in mean using pooled variance:
hot = cereal[which(cereal$type == "H"),]
cold = cereal[which(cereal$type == "C"),]
hot = hot[-which(is.na(hot$potass)),]
# which(is.na(cold$potass)) # no NAs
hm = mean(hot$potass); hm
cm = mean(cold$potass); cm
hm - cm
sp = ( (nrow(hot) - 1)*var(hot$potass) + (nrow(cold) - 1)*var(cold$potass) )/(nrow(hot) + nrow(cold) - 2); sp
df = nrow(hot) + nrow(cold) - 2; df
t = qt(.975,df); t
ub = (hm - cm) + t*sqrt(sp/nrow(hot) + sp/nrow(cold)); ub
lb = (hm - cm) - t*sqrt(sp/nrow(hot) + sp/nrow(cold)); lb

t.CI.datameandiff.pooledvar = function(CI,df.data1,df.data2){
  alpha = 1 - CI
  n1 = length(df.data1)
  n2 = length(df.data2)
  df = n1 + n2 - 2
  t = qt(1 - alpha/2, df)
  sp = ( (n1 - 1)*var(df.data1) + (n2 - 1)*var(df.data2) )/(n1 + n2 - 2)
  me = t*sqrt( sp/n1 + sp/n2 )
  xbar1 = mean(df.data1)
  xbar2 = mean(df.data2)
  ub = xbar1 - xbar2 + me
  lb = xbar1 - xbar2 - me
  print(sprintf("We are %s percent confident that the true difference between the two population means is between %.4f and %.4f",CI*100,lb,ub))
}
t.CI.datameandiff.pooledvar(.95,hot$potass,cold$potass) # heck yeah


#######################################################################################
#######################################################################################

### week 10 ppt III

sx = data.frame(subject = 1:10,
                pre = c(350,700,356,362,361,304,675,367,387,535),
                post = c(321,483,336,447,214,285,480,330,325,325)
                )
sx$diff = sx$pre - sx$post
mean(sx$diff)
sd(sx$diff)
t.test(sx$pre,sx$post,paired = TRUE)
?qt()
qt(.025,40)
qt(.975,28)


#######################################################################################
#######################################################################################

### EXAM II REVIEW PROBLEMS:

## 1.
t.CI.meandiff.pooled.var(CI=.95,xbar1=21,xbar2=12.1,n1=13,n2=17,var1=4.9^2,var2=5.6^2)

## 2.
qnorm(.975)
55/125
me = 1.96*sqrt(.44*.56/125)
.44 + me
.44 - me

## 3.
qnorm(.95)
q1 = 1 - 58/215; p1 = 58/215
q2 = 1 - 217/1140; p2 = 217/1140
n1 = 215
n2 = 1140
me = sqrt(p1*q1/n1 + p2*q2/n2); me
p1-p2 + 1.645*me
p1-p2 - 1.645*me

## 4. d)
(3.3^2)/(1.2^2)

## 5.
t = (104 - 100)/(20.2/sqrt(87)) # NOT Z!!!
1 - pt(t,86)

## 6.
r = 40/25

## 7.
1-pt(2,19)
alpha = .05
PT = 1-alpha/2; PT
qt(PT,19)

## 8.
n1 = 57
xbar1 = 70.63
s1 = 16.27

n2 = 43
xbar2 = 64.33
s2 = 12.99

t.meandiff.pooled.var = function(xbar1,xbar2,n1,n2,var1,var2){
  sp.squared = ( var1*(n1-1) + var2*(n2-1) )/(n1 + n2 - 2)
  t.statistic = (xbar1-xbar2)/sqrt(sp.squared*(1/n1 + 1/n2))
  df = n1+n2-2
  pvalue = pt(t.statistic,df)
  print(sprintf("The p value is %.4f for left tail or %.4f for right tail",pvalue,1-pvalue))
  print(sprintf("The t test statistic is %.4f",t.statistic))
  print(sprintf("The pooled variance is %.4f",sp.squared))
}
t.meandiff.pooled.var(xbar1=70.63,xbar2=64.33,n1=57,n2=43,var1=16.27^2,var2=12.99^2)

## 9. 
1 - pt(1.498,24)
1 - pt(4.39,60)



#######################################################################################
#######################################################################################

wheel = data.frame(fn = c(rep("fallers",131+52),rep("nonfallers",14+36)),
                   chair = c(rep("yes",131),rep("no",52),
                             rep("yes",14),rep("no",36) ) )

# w2 = wheel[,2:1]
# table(w2)

w.tab = table(wheel)
chisq.test(w.tab, correct = FALSE)

qchisq(.05,1, lower.tail = FALSE)
qchisq(.05,3*4, lower.tail = FALSE)
pchisq(6.01,2, lower.tail = FALSE)
### Test for independence

library(tables) # for the function 'tabular'
library(MASS) # for the dataset 'survey'

smoke = 
  data.frame(school = 
               c(rep("c_grad",75),rep("HS",150),rep("nothing",75)),
             policy = 
               c(rep("no_reg",5),rep("d_only",44),rep("no_smk",23),rep("no_opin",3),
                 rep("no_reg",15),rep("d_only",100),rep("no_smk",30),rep("no_opin",5),
                 rep("no_reg",15),rep("d_only",40),rep("no_smk",10),rep("no_opin",10)
                              ) )
smk.tab = table(smoke)

# booyah = margin.table(smk.tab, 1) # row totals
# bvec = vector()
# for (i in 1:3){
#   bvec[i] = booyah[[i]]
# }
# sum(bvec) # Is this the only way to get the grand total??

margin.table(smk.tab, 1) # row totals
margin.table(smk.tab, 2) # column totals

round(prop.table(smk.tab), 2) # cell proportions
round(prop.table(smk.tab, 1), 2) # row proportions
round(prop.table(smk.tab, 2), 2) # column proportions

chisq.test(smk.tab)
##############################
##############################
s = survey
help(survey)
names(s)
s.g_s = table(s$Sex, s$Smoke)
s.g_e = table(s$Sex, s$Exer); s.g_e

margin.table(s.g_e, 1) # row totals (row margin)
margin.table(s.g_e, 2) # column totals (column margin)

round(prop.table(s.g_e), 2) # cell proportions
round(prop.table(s.g_e, 1), 2) # row proportions (proportions of genders in each exercise column)
round(prop.table(s.g_e, 2), 2) # column proportions (proportions of exercise levels in each gender row)

chisq.test(s.g_e)
##############################
plot(s$Pulse ~ s$Exer)

##############################

# example of tables package used on Fisher's iris data:
t = tabular((Species+1) ~ 
              (n=1) + 
              Format(digits=2)*(Sepal.Length + Sepal.Width)*(mean+sd), data = iris)



#######################################################################################
#######################################################################################

# CORRELATION

BW = data.frame(b.weight = c(2150,
                             2050,
                             1000,
                             2300,
                             900,
                             2450,
                             2350,
                             2350,
                             1900,
                             2400,
                             1700,
                             1950,
                             1250,
                             1700,
                             2000,
                             920,
                             1270,
                             1550,
                             1500,
                             1900,
                             2800,
                             3600,
                             3250,
                             3000,
                             3000,
                             3050),
                PAI.2 =    c(185,
                             200,
                             125,
                             25,
                             25,
                             78,
                             290,
                             60,
                             65,
                             125,
                             122,
                             75,
                             25,
                             180,
                             170,
                             12,
                             25,
                             25,
                             30,
                             24,
                             200,
                             300,
                             300,
                             200,
                             200,
                             230))
plot(BW$PAI.2,BW$b.weight, xlim = c(0,400), ylim = c(0,4000), 
     xlab = "PAI-2", ylab = "BW", pch = 18, col = "blue")
mod1 = lm(b.weight ~ PAI.2, data = BW); mod1
abline(mod1, col = "red")
r = cor(BW$PAI.2, BW$b.weight); r # r = .753
cor.test(BW$PAI.2, BW$b.weight, alternative = "two.sided", method = "pearson")
# WHY IS THE T STATISTIC AND DF THE SAME UNDER GREATER AND TWO.SIDED, BUT THE P-VALUE IS DIFFERENT?????
t = r*sqrt((nrow(BW) - 2)/(1 - r^2)); t
t2 = .71*sqrt((nrow(BW) - 2)/(1 - .71^2)); t2
x.diff = vector()
for (i in 1:nrow(BW)){
  x.diff[i] = BW[i,1] - mean(BW$b.weight)
}
# x.diff
y.diff = vector()
for (i in 1:nrow(BW)){
  y.diff[i] = BW[i,2] - mean(BW$PAI.2)
}
# y.diff
Sxy = sum(x.diff*y.diff)/25; Sxy
Sx = sd(BW$b.weight); Sx
Sy = sd(BW$PAI.2); Sy
r2 = Sxy/(Sx*Sy); r2
r




#######################################################################################
#######################################################################################

# Quiz 8

qf(.05,3,20,lower.tail = FALSE) # correct

smoke = data.frame(A_nonsmoke = c(12,10,11,13,9,9),
                   B_light = c(9,8,5,9,9,10),
                   C_mod = c(9,4,7,9,5,7),
                   D_heavy = c(3,2,1,5,4,6))
# library(tidyr)
s.long = gather(smoke, key = "type", value = "HDL", 
                c("A_nonsmoke","B_light","C_mod","D_heavy"))

mod1 = aov(HDL ~ factor(type), data = s.long); summary(mod1)
pf(16.15,3,20, lower.tail = FALSE) # same as the p-value in summary(mod1)
attach(s.long)
plot(factor(type),HDL, ylab = "HDL")






#########

# Quiz 9
BP = data.frame(systolic = c(138,130,135,140,120,125,120,130,144,143,140,130,150),
                diastolic = c(82,91,100,100,80,90,80,80,98,105,85,70,100))
attach(BP)

plot(systolic,diastolic)
rBP = lm(diastolic ~ systolic); rBP
abline(rBP,col = "red")
r = cor(systolic,diastolic); r
64.7/(9.3*10.7)
beta1 = r*(sd(diastolic)/sd(systolic))
betanot = mean(diastolic) - beta1*mean(systolic); betanot

###############

cotin = data.frame(cigs = c(30,10,4,15,10,1,20,8,7,10,10,20),
                   cotinine = c(179,283,75.6,174,209,9.5,350,1.9,43.4,25.1,408,344))
sd(cotin$cigs)
r = cor(cotin$cigs,cotin$cotinine); r
Sy = sd(cotin$cotinine)
Sx = sd(cotin$cigs)
r*Sx*Sy

Xdiffs = vector()
for (i in 1:nrow(cotin)){
  Xdiffs[i] = cotin[i,1] - mean(cotin$cigs)
}

Ydiffs = vector()
for (i in 1:nrow(cotin)){
  Ydiffs[i] = cotin[i,2] - mean(cotin$cotinine)
}
sum(Xdiffs*Ydiffs)/11


#######################################################################################
#######################################################################################

# Independence vs Homogeneity ... vs Goodness of Fit

## Independence and Homogeneity are the two that seem exactly the same to me. You could
# be told you're looking at a homogeneity problem, with a contingency, and another 
# person could be looking at exactly the same contingency table and asked a question
# that is worded slightly differently about it, and you could do one and he could do
# the other approach, and you'd be doing exactly the same thing, using exactly the same
# formulas and the exact same chi square test, and both get the same right answer!

## Here's what socratic.org says: - https://socratic.org/questions/what-is-the-difference-between-a-chi-square-test-of-independence-and-a-chi-squar 
# chi square test of independence - helps us to find whether 2 or more attributes are associated or not.e.g. whether playing chess helps boost the child's math or not. It is not a measure of the degree of relationship between the attributes. it only tells us whether two principles of classification are significantly related or not, without reference to any assumptions concerning the form of relationship.
# chi square test of homogeneity - is an extension of chi square test of independence...tests of homogeneity are useful to determine whether 2 or more independent random samples are drawn from the same population or from different populations. instead of one sample- as we use with independence problem, here we have two or more samples.
# Both the types of tests are concerned with cross classified data. both use the same testing statistics. However they are different from each other.
# Test for independence is concerned with whether one attribute is independent of the other and involves a single sample from the population.
# On the other hand, test of homogeneity tests whether different samples come from same population. It involves 2 or more independent samples-one from each of the populations in question.

## OF COURSE, THIS SOCRATIC.ORG EXPLANATION ONLY MAKES ME MORE SUSPICIOUS THAT THEY'RE
# EXACTLY THE SAME. SAME DATA STRUCTURE, SAME OPERATIONS, SAME ANSWER.



## Stattrek examples - websites: 
# https://stattrek.com/chi-square-test/goodness-of-fit.aspx  
# https://stattrek.com/chi-square-test/homogeneity.aspx
# https://stattrek.com/chi-square-test/independence.aspx 

## Goodness of Fit: 
# Acme claims 30% of cards are rookies, 60% are normal veterans, 10% are all stars
# random sample of 100 cards has 50 rookies, 45 vets, 5 all stars:
cards = c(50,45,5)
chisq.test(cards, p = c(.3,.6,.1))
# EASY AS THAT.


## Test for Independence:
# A public opinion poll surveyed a simple random sample of 1000 voters. Respondents 
# were classified by gender (male or female) and by voting preference 
# (Republican, Democrat, or Independent). Results are shown in the contingency table 
# below.

voters = data.frame(gender = rep(c("male","female"), c(400,600)),
                    pref = c(rep(c("Repub","Democ","Indep"),c(200,150,50)),
                             rep(c("Repub","Democ","Indep"),c(250,300,50)) )
                    )
t.vote = table(voters); t.vote
chisq.test(t.vote) 
# EASY AS THAT.


## Homogeneity:
# In a study of the television viewing habits of children, a developmental 
# psychologist selects a random sample of 300 first graders - 100 boys and 200 girls. 
# Each child is asked which of the following TV programs they like best: 
# The Lone Ranger, Sesame Street, or The Simpsons. 
# Results are shown in the contingency table below.

tv = data.frame(gender = rep(c("boy","girl"),c(100,200)),
                shows = c(rep(c("L Ranger","Sesame","Simpsons"),c(50,30,20)),
                          rep(c("L Ranger","Sesame","Simpsons"),c(50,80,70)) )
                )
t.tv = table(tv); t.tv
chisq.test(t.tv)
# EASY AS THAT.




