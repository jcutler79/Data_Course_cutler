# Familial Adenomatous Polyposis
## From, Giardiello et al. - "Treatment of Colonic and Rectal Adenomas with Sulindac in Familial Adenomatous Polyposis" (NEJM 1993)
## https://www.nejm.org/doi/full/10.1056/NEJM199305063281805


# Libraries:
library(ggplot2)
library(dplyr)
library(car)
library(lsmeans)
library(emmeans)
library(data.table)


fap = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/polyposis.csv")

# Polishing up the data frame and adding some columns better suited for OpenBUGS:
head(fap,4)
summary(fap$baseline)
summary(fap$number3m)
length(which(fap$treatment == "placebo"))
length(which(fap$treatment == "active"))

fap$bayestx = rep(NA,nrow(fap))
fap[which(fap$treatment == "active"),7] = 1
fap[which(fap$treatment != "active"),7] = 0 # WHY DID THIS WORK THOUGH? BECAUSE I'M CREATING A NEW COLUMN, NOT CHANGING THE ORIGINAL ONE

fap$bayesex = rep(NA,nrow(fap))
fap[which(fap$sex == "male"),8] = 1
fap[which(fap$sex == "female"),8] = 0
fap$perch = round((fap$number3m-fap$baseline)/fap$baseline,4)
fap = fap[,-1]



# Descriptive statistics:

## Distribution of age by sex:
# plot(fap$age ~ fap$sex)
ggplot(fap, aes(x=sex,y=age)) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(.2)) +
  labs(title = "Distribution of age by sex",
       x="", y="Age in years")

## Distribution of age by treatment:
# plot(fap$age ~ factor(fap$treatment))
ggplot(fap, aes(x=factor(treatment),y=age)) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(.2)) +
  labs(title = "Distribution of age by treatment",
       x="", y="Age in years")

## Distribution of treatment by sex:
ggplot(fap, aes(x=factor(sex),fill=factor(treatment))) +
  geom_bar(stat = "count") +
  scale_y_continuous(breaks = seq(0,15,1)) +
  labs(title = "Distribution of treatment by sex",
       x="", y="Number of people")

tx.props = 
  fap %>%
  group_by(sex,treatment) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))      # INTERESTING SUCCESS
# tx.props
# summary(tx.props)

barplot(tx.props$prop, col = tx.props$sex,
        names.arg = tx.props$treatment)
legend("top",
       legend = c("female","male"),
       fill = c("black","red"))

## Outcome by treatment group:
plot(fap$perch ~ factor(fap$treatment),
     main = "Percent change in number of polyps\n by treatment group",
     xlab = "", ylab = "Percentage change in number of polyps")
abline(h = 0, col = "gray", lty = 2)
tx.means = tapply(fap$perch, fap$treatment, mean)
points(tx.means, pch = 18, col = "red")

## Outcome by age:
plot(fap$age,fap$perch,
     main = "Percent change in number of polyps\n by age",
     xlab = "Age in years", ylab = "Percentage change")
abline(h = 0, col = "gray", lty = 2)
mod.age = lm(perch ~ age, data = fap)
cor(fap$age,fap$perch)
abline(mod.age, col = "red")
summary(mod.age)

## Outcome by sex:
plot(fap$perch ~ factor(fap$sex),
     main = "Percent change in number of polyps\n by sex",
     xlab = "", ylab = "Percentage change")
abline(h = 0, col = "gray", lty = 2)
sx.means = tapply(fap$perch, fap$sex, mean)
points(sx.means, pch = 18, col = "red")



# Frequentist statistical analysis:

## ANOVA
three.way.complete = aov(perch ~ factor(sex)*age*factor(treatment), data = fap)
summary(three.way.complete) # no significant interactions

partial.ints = aov(perch ~ sex+age+treatment+sex*age, data = fap)
summary(partial.ints) # still no significant interaction, which puzzles me because of the ggplot boxplot of the age distribution by sex

main.effects = aov(perch ~ sex+age+treatment, data = fap)
summary(main.effects)
######################## SKIP #############################
# fap %>% 
#   select(treatment,sex) %>% 
#   filter(sex == "female") %>% 
#   count(treatment == "active")
# 
# fap %>% 
#   select(treatment,sex) %>% 
#   filter(sex == "male") %>% 
#   count(treatment == "active")
# 
# head(fap)
# ad = data.table(fap)
# ad[sex == "female" & treatment == "active", length(sex)]
# ad[sex == "male" & treatment == "active", length(sex)]
# summary(fap$sex)
###########################################################


# means = tapply(fap$age, fap$treatment, mean)
# means
# SDs = tapply(fap$age, fap$treatment, sd)
# SDs
# 
# summary(fap$age)
# sort(fap$age)
# hist(fap$age)
# 
# # teens = 
# # fap %>%
# #   select(age) %>%
# #   filter(age < 20)
# # length(teens$age)/22
# 
# # from data.table:
fapt = fread("/Users/jamescutler/Desktop/Data_Course_cutler/polyposis.csv")
# teen.tx = fapt[age < 20 & treatment == "active", .N]
# teen.tx
# teen.tot = fapt[age<20, .N]
# teen.tot
# teen.tx/teen.tot # NOT BAD. I WISH THERE WAS A SLIGHTLY MORE EFFICIENT WAY THOUGH ...
# # ... like, prop.table or something.
# 
# qqnorm((fap$number3m-fap$baseline)/fap$baseline)
# qqline((fap$number3m-fap$baseline)/fap$baseline)
# 
# hist((fap$number3m-fap$baseline)/fap$baseline)
# plot(x=(fap$number3m-fap$baseline)/fap$baseline,rep(1,nrow(fap)))
# 
# tx = fapt[treatment == "active",round((number3m-baseline)/baseline,4)]
# pl = fapt[treatment == "placebo",round((number3m-baseline)/baseline,4)]
# plot(x=pl,y=rep(1.1,length(pl)), xlim = c(-1,1))
# points(x=tx,y=rep(1,length(tx)), col = "red")
# points(mean(pl),1.1, pch = 18, col = "orange")
# points(mean(tx),1, pch = 18, col = "green")
# 
# length(pl)
# length(tx)
# 
# sd(tx)
# 1/var(tx)
# mean(pl)
# 1/var(pl)
# sd(pl)
# 
# cat(tx, sep = ",")
# cat(pl, sep = ",")

###############################################

# Bayesian stuff:

## Prior for age:

# Mean and standard deviation of age of patients from Giardiello et al.:
mean(fap$age)
sd(fap$age)

# Mean and standard deviation of age of patients from Giardiello et al.:
mean(fap$age)
sd(fap$age)

hist(fap$age, probability = TRUE,
     main = "Histogram of age in Giardiello et al.",
     xlab = "Age in years",
     col = "turquoise")
lines(density(fap$age), col = "red", lwd = 2)
set.seed(5)
x = rnorm(1000,mean = 24, sd = 9)
lines(density(x), col = "orange", lty = 2, lwd = 3)
legend("topright",c("Actual data","Proposed prior distribution"),
       col = c("red","orange"), lty = 1:2, cex = .7)



###########

# Other studies age distributions:

labayle = c(24,39,52,36,32,29,46,45,30,35)
mean(labayle)
sd(labayle)

# Cruz.Correa = 37 years (mean)
# Nugent = 45 years (mean); Range: 27-70  

# So, 
## Giardiello: 24 (9)
## Labayle:    37 (8)
## CruzCorrea: 37 (11) - (21-53)
## Nugent:     45      - (27-70)


correa = data.frame(age = c(32,42,52,25,36,21,22,24,35,33,46,45),
                    before = c(19,47,10,7,80,16,80,29,16,17,19,7),
                    month12 = c(4,0,0,0,25,14,28,0,0,7,2,2))
correa$per.change = (correa$month12 - correa$before)/correa$before

plot(correa$age, correa$per.change)
mod.correa = lm(per.change ~ age, data = correa)
abline(mod.correa, col = "red")
summary(mod.correa)

cat(fap$perch, sep = ",")
length(fap$perch)

cat(fap$age, sep = ",")
1/18
1/.0556
rm(fapt)
fdt = data.table(fap)
fdt[sex == "male", mean(perch)]
1/fdt[sex == "male", var(perch)]


fdt[sex == "female", mean(perch)]
1/fdt[sex == "female", var(perch)]

cat(fdt[sex=="male",perch], sep = ",")
cat(fdt[sex=="female",perch], sep = ",")
summary(fap$sex)

ages = c(correa$age,fap$age)
qqnorm(ages)
qqline(ages, col = "red")

shapiro.test(ages)
shapiro.test(fap$age)
shapiro.test(correa$age)
qqnorm(correa$age)
qqline(correa$age, col = "red")

fap$tx = NA
fap[which(fap$treatment == "active"),"tx"] = 1
fap[which(fap$treatment == "placebo"),"tx"] = 2
cat(fap$tx, sep = ",")

fap$gender = NA
fap[which(fap$sex == "male"),"gender"] = 1
fap[which(fap$sex == "female"),"gender"] = 2
cat(fap$gender, sep = ",")



age.mod = lm(perch ~ age, data = fap)
summary(age.mod)









