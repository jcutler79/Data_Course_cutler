### Applied Bayesian

# My unchanging three paths:
OpenBUGS.pgm = "/Applications/OpenBUGS323/OpenBUGS.exe"
WINE = "/usr/local/Cellar/wine/3.0.4/bin/wine"
WINEPATH = "/usr/local/Cellar/wine/3.0.4/bin/winepath"

# Libraries:
library(stringr)
library(data.table)
library(MASS)
library(dplyr)
library(ggplot2)
library(stringr)
library(sas7bdat)
library(coda)                 # for gelman.plot, etc.
library(lattice)              # for xyplot and densityplot
# install.packages("mcmcplots")
library(mcmcplots)            # for dinky caterpillar plot
library(bayesplot)            # for mcmc_violin

# install.packages("R2OpenBUGS")
library(R2OpenBUGS)


### The BUGS Book Chapter 11 specialized models (10 example problems):
# Time to event data
# Parametric survival regression
# ***Icelandic volcano eruptions: predicting event times
# Time series models: autoregressive (AR)
# ***Sunspots
# ***Tuna stocks catch per unit effort
# Spatial models
# Intrinsic conditionally autoregressive (CAR) models
# ***Mapping lip cancer in Scotland
# Multivariate CAR models
# Poisson-gamma moving average models
# Geostatistical models
# ***Estimating radioactivity levels on Rongelap Island
# Meta-analysis
# ***Differential equation and pharmacokinetic models: Cadralazine differential equations
# Finite mixture and latent class models
# ***Eyes: peak sensitivity wavelengths for individual microspectrophotometric records on the eyes of monkeys
# Zero-inflated Poisson
# ***Stagnant water: change point function
# Spline models
# Semiparametric survival models
# ***Leukemia: survival models with piecewise-constant hazards
# Bayesian nonparametric models
# ***Galaxy clustering: Dirichlet process mixture models






### Counting techniques
## How many possible outcomes are there in tossing a coin four times?
# 2 possible outcomes per toss, four tosses: 2^4 = 16
## How many possible outcomes with rolling 2 dice?
# 6 possible outcomes per roll, two rolls: 6^2 = 36

coins = expand.grid(first = c("H","T"), second = c("H","T"), third = c("H","T"), fourth = c("H","T"))
dice = expand.grid(first = c(1,2,3,4,5,6), second = c(1,2,3,4,5,6))
class(dice)
sum(dice[1,1],dice[1,2])
dice$sums = dice$first + dice$second

## I'M GOING TO TRY TO CREATE A FIFTH COINS COLUMN WITH THE 4 COLUMNS CONDENSED INTO 1 STRING:
# coins[5,]
# onerow = vector()
# for (i in 1:4){
#   onerow[i] = coins[5,i]
# }
# onerow
# newrow = cat(onerow,sep = "")
# newrow
# cat(as.vector(coins[5,]))
# cat(coins[11,1],coins[11,2], sep = "") 
## AFTER ALL THESE STUPID ATTEMPTS, I FINALLY FIGURED IT OUT!!! SEE BELOW:
paste(coins[11,], collapse = "") # THAT'S IT!!!!
# NOW TO TRY IT OUT ON THE WHOLE DATA FRAME AT ONCE:
mypaste = function(yourvector){
  return(paste(yourvector,collapse = ""))
}
coins$all = apply(coins,1,mypaste) # IT ONLY WORKS WITH A CUSTOM FUNCTION BECAUSE YOU 
# CAN'T SPECIFY ANYTHING INSIDE YOUR PASTE FUNCTION WHEN IT'S INSIDE THE APPLY FUNCTION. 
# I ALSO CAN'T BELIEVE THIS WORKED.
## THIS IS THE DUMB WAY I TRIED TO DO THE NEXT STEP:
# mycount = 0
# for (i in 1:length(coins$all)){
#   if (gregexpr("THT",coins$all)[[i]][1] > 0){
#     mycount = mycount+1
#     print(mycount)
#   }
# }
## THE REAL WAY:
grep("HTT",coins$all)
grep("THH",coins$all)

"HTTH" %in% coins$all
coins = coins[-nrow(coins),]
"HTTH" %in% coins$all


## NOW LET'S TRY IT WITH 5 COIN TOSSES:
coins5 = expand.grid(first = c("H","T"), 
                     second = c("H","T"), 
                     third = c("H","T"), 
                     fourth = c("H","T"),
                     fifth = c("H","T"))
coins5$all = apply(coins5,1,mypaste)
htt = grep("HTT",coins5$all); htt
tth = grep("THH",coins5$all); tth
# NOW WE HAVE SOME OVERLAP!!
allrows = c(htt,tth); allrows
length(unique(allrows)) # =22
'%nin%' = Negate('%in%')
which(htt %in% tth)
which(tth %in% htt)
length(which(htt %nin% tth)) + length(tth) # =22



#########################################################################
#########################################################################

3814/28616
.967*.0038 + .9962*.015


x = 0:25
plot(x, dbinom(x,25,.3), ylim = range(0,.25), type = "h",
     ylab = "Probability", xlab = "Number of successes", col = "blue")

# gamma distribution:
x = seq(0,40, length.out = 1000)
plot(x, dgamma(x,5,.5),
     ylim = range(0,.15),
     type = "l",
     ylab = "Denisty", xlab = "Possible Outcome Values", col = "blue")



#########################################################################
#########################################################################

# WEEK 3 THURSDAY
prev = 20/68
prev*(1-prev)/68

s = .5/4; v = s^2
a = .5*((.5^2)/v - 1); a
b = .5*((.5^2)/v - 1); b

x = seq(0,1, length.out = 1000); x
dist.beta = dbeta(x,a,b)
plot(x,dist.beta, type = "l", col = "blue")
abline(v = c(.25,.75), lty = 2, col = "red")

x = seq(0,1, length.out = 1000); x
dist.beta = dbeta(x,1.8,4.3)
plot(x,dist.beta, type = "l", col = "blue")

Ex = 1.8/(1.8+4.3); Ex


# NCC model:
nccmodel <- function(){
  # prior:
  theta ~ dbeta(1.66,2.63)
  # likelihood:
  for (i in 1:68){
    x[i] ~ dbern(theta[i])
  }
}
data = list(rep(c(1,0),c(20,48)))



#####################################################################################
#####################################################################################

# Using OpenBUGS on the schools dataset (5 steps):
data("schools")

# Define your data:
J = nrow(schools)
y = schools$estimate
sigma.y = schools$sd
data = list("J","y","sigma.y")

# Define your initial values based on your prior (and write your parameters):
inits = function(){
  list(theta = rnorm(J,0,100),
       mu.theta = rnorm(1,0,100),    # WHY NOT JUST SET MU EQUAL TO 0?
       sigma.theta = runif(1,0,100))
}
parameters = c("theta","mu.theta","sigma.theta")

# Define your BUGS model:
schoolmodel <- function(){
  # prior:
  mu.theta ~ dnorm(0, 1e-6)
  sigma.theta ~ dunif(0,1000)
  tau.theta <- pow(sigma.theta, -2) # SO, THIS CAN BE OUT OF THE MODEL?
  # likelihood:
  for (j in 1:J){
    theta[j] ~ dnorm(mu.theta, tau.theta)
    tau.y[j] <- pow(sigma.y[j], -2)
    y[j] ~ dnorm(theta[j], tau.y[j])
  }
}
# Write it to a text file:
setwd("/Users/jamescutler/Desktop/Bayesian/")
write.model(schoolmodel, "schoolmodel.txt")
model.file = paste(getwd(),"schoolmodel.txt",sep = "/")
file.show("schoolmodel.txt")

# Get your three paths right:
OpenBUGS.pgm = "/Applications/OpenBUGS323/OpenBUGS.exe"
WINE = "/usr/local/Cellar/wine/3.0.4/bin/wine"
WINEPATH = "/usr/local/Cellar/wine/3.0.4/bin/winepath"

# Create your simulation (AND DON'T FORGET TO RUN IT BY TYPING print(school.sim)):
school.sim = bugs(data, inits, parameters.to.save = parameters, model.file = model.file,
                  n.chains = 3, n.iter = 2000,
                  OpenBUGS.pgm = OpenBUGS.pgm,
                  WINE = WINE,
                  WINEPATH = WINEPATH,
                  useWINE = TRUE)



#########################################################################
#########################################################################

### Survey dataset example from R tutorial book:
# http://www.r-tutor.com/bayesian-statistics/openbugs

# library(MASS)
data("survey")

mytbl = table(survey$Smoke); mytbl

# Data:
N = as.numeric(sum(mytbl)); N
y = N - as.numeric(mytbl["Never"]); y
data = list("N","y")

# Initial values (and parameters):
inits = function(){
  list(p = .5)
}
parameters = c("p")

# Model:
smokemodel <- function(){
  # prior
  p ~ dbeta(1,1)
  # likelihood
  y ~ dbin(p,N)
}
setwd("/Users/jamescutler/Desktop/Bayesian/")
write.model(smokemodel, "smokemodel.txt")
model.file = paste(getwd(),"smokemodel.txt",sep = "/")
file.show("smokemodel.txt")

OpenBUGS.pgm = "/Applications/OpenBUGS323/OpenBUGS.exe"
WINE = "/usr/local/Cellar/wine/3.0.4/bin/wine"
WINEPATH = "/usr/local/Cellar/wine/3.0.4/bin/winepath"

smoke.sim = bugs(data, inits, parameters.to.save = parameters, model.file = model.file, 
                 n.chains = 3, n.iter = 10000,
                 OpenBUGS.pgm = OpenBUGS.pgm,
                 WINE = WINE,
                 WINEPATH = WINEPATH,
                 useWINE = TRUE)

all(smoke.sim$summary[,"Rhat"] < 1.1)
smoke.sim$mean["p"]
smoke.sim$sd["p"]

# Now for CODA analysis of MCMC convergence:
smoke.sim = bugs(data, inits, parameters.to.save = parameters, model.file = model.file, 
                 n.chains = 3, n.iter = 10000, codaPkg = TRUE,
                 OpenBUGS.pgm = OpenBUGS.pgm,
                 WINE = WINE,
                 WINEPATH = WINEPATH,
                 useWINE = TRUE)
smoke.coda = read.bugs(smoke.sim)

library(coda)                 # for gelman.plot, etc.
library(lattice)              # for xyplot and densityplot
# install.packages("mcmcplots")
library(mcmcplots)            # for dinky caterpillar plot
library(bayesplot)            # for mcmc_violin
xyplot(smoke.coda)       # not a coda-specific plot
densityplot(smoke.coda)  # not a coda-specific plot
acfplot(smoke.coda)      # not a coda-specific plot
gelman.diag(smoke.coda)
gelman.plot(smoke.coda)
smoke.summary = summary(smoke.coda,
                        q = c(.025, .975))
smoke.summary$statistics["p",]
smoke.summary$quantiles["p",]
cumuplot(smoke.coda)
# panel.bwplot(smoke.coda)               # argument "y" is missing, with no default
# bwplot(smoke.coda)                     # no applicable method for 'bwplot' applied to an object of class "mcmc.list"
# boxplot(smoke.coda)                    # Error in round(thin) : non-numeric argument to mathematical function
# panel.violin(smoke.coda)               # argument "y" is missing, with no default
# boxplot(smoke.coda[[1]][,"p"][1])      # Error in round(thin) : non-numeric argument to mathematical function
# panel.bwplot(smoke.coda[1][,"p"][[1]]) # argument "y" is missing, with no default
# bwplot(smoke.coda[1][,"p"][[1]])       # no applicable method for 'bwplot' applied to an object of class "mcmc"
geweke.plot(smoke.coda)
traceplot(smoke.coda)
densplot(smoke.coda)
smoke.summary$quantiles
smoke.coda[1][,"p"][[1]]
caterplot(smoke.coda)
mcmc_violin(smoke.coda) + yaxis_ticks(on = TRUE) + yaxis_text(on = TRUE)
#########################################################################
#########################################################################


#### CH 20: BAYESIAN DISCRETE INFERENCE
### Fruits in a bag example:
N = 6 # number of fruits
x = 0:N # possible number of apples (a uniform prior is assumed)
x
largo = length(x)
prior = rep(1/largo,largo); prior
names(prior) = x; cbind(prior)

# If there are x apples in the bag, then the likelihood of drawing an apple
## is x/N, which we can denote by another vector, y:
y = x/N; y # THIS IS THE LIKELIHOOD?? I think I get it
cbind(prior, likelihood = y)

# We multiply the Likelihood by the Prior:
LP = prior*y; LP

# The Bayesian magic here is that we can get our denominator simply by summing 
## up all the LP's (and divide the LP's by that sum to get PP - the posterior):
PP = LP/sum(LP); PP # OUR POSTERIOR
cbind(prior, likelihood = y, LP, PP)
plot(0:6,prior, ylim = c(0,1))# BLACK prior
points(0:6,y, col = "red")    # RED likelihood
points(0:6,LP, col = "blue")  # BLUE product of the two
points(0:6,PP, col = "green") # GREEN posterior

# Try this:
1 - x/N # Which one is supposed to represent the chance of drawing an orange on 2nd try?

# Now our posterior from above can become our new prior for drawing the next apple
## out of the bag:
prior1 = PP

# Say we just drew an apple out on first try. 
## Since there are now x-1 apples left in the bag, the probability of drawing an apple
## on the second try is (x-1)/(N-1).
y1 = (x-1)/(N-1); y1 # Likelihood?? How could the first number be negative???

LP1 = prior1*y1; LP1
PP1 = LP1/sum(LP1); PP1
cbind(prior1,y1,LP1,PP1)
sum(PP1*(x-2)/(N-2))
points(0:6,PP1, pch = 8, col = "chartreuse4")
# I DON'T UNDERSTAND HOW THIS TELLS US THE PROBABILITY OF DRAWING AN APPLE VS AN ORANGE 



#### CH 23: BAYESIAN INFERENCE FOR A NORMAL MEAN
### Practice on dataset "survey":
# library(MASS)
data("survey")
plot(survey$Height)
hist(survey$Height)

length(which(is.na(survey$Height))) # 28 NAs in this 
y = na.omit(survey$Height)
n = length(y); n
# Using a uniform prior, the posterior mean of mu is the sample mean:
mean(y)
sd(y)
# However, assume that the student heights fit a normal distribution of N(mu,10^2):
10/sqrt(n)

sd(na.omit(survey$Pulse))
12/sqrt(length(na.omit(survey$Pulse)))
mean(na.omit(survey$Pulse))

### 23.3 INFERENCE FOR NORMAL MEAN WITH KNOWN VARIANCE:
# Using uniform prior, estimate the mean and sd of mu in OpenBUGS:
# Assume height is N(mu,10^2):
y = na.omit(survey$Height); class(y)
n = length(y)
# sigma.y = sd(y); sigma.y # ACTUALLY, WE'RE GOING TO ASSUME IT'S 10:
sigma.y = 10
tau = sigma.y^-2
data = list("y","n","tau")

inits = function(){
  list(mu = 0)
}
parameters = c("mu")

heightsmodel <- function(){
  # prior:
  mu ~ dunif(-1e4, 1e4) # Tried at 1e9, 1e6 -- too slow. Could this be why?
  # likelihood:
  for (i in 1:n){
    y[i] ~ dnorm(mu,tau)
  }
}
setwd("/Users/jamescutler/Desktop/Bayesian/")
write.model(heightsmodel,"heightsmodel.txt")
model.file = paste(getwd(),"heightsmodel.txt",sep = "/")
file.show("heightsmodel.txt")

OpenBUGS.pgm = "/Applications/OpenBUGS323/OpenBUGS.exe"
WINE = "/usr/local/Cellar/wine/3.0.4/bin/wine"
WINEPATH = "/usr/local/Cellar/wine/3.0.4/bin/winepath"

heights.sim = bugs(data, inits, parameters.to.save = parameters, model.file = model.file,
                   n.chains = 3, n.iter = 2000,
                   OpenBUGS.pgm = OpenBUGS.pgm,WINE = WINE, WINEPATH = WINEPATH,
                   useWINE = TRUE)




################

# Week 5 - 


data = list("y","n")



################################################################################
################################################################################

# Week 8


# Tobacco data:
tob = read.csv("/Users/jamescutler/Desktop/Bayesian/6 linear.csv", 
               header = FALSE)

t = as.character(tob[,1])
cat(t,sep = ",")

t2 = as.character(tob[,2]); t2
cat(t2,sep = ",")

tob2 = read.csv("/Users/jamescutler/Desktop/Bayesian/data/6_tobstate.csv", 
                header = FALSE)
ggplot() +
  geom_bar(data = tob2, mapping = aes(1:nrow(tob2),tob2$V1*100), stat = "identity", width = .9, fill = "green") +
  geom_bar(data = tob2, mapping = aes(1:nrow(tob2),tob2$V2), stat = "identity", width = .4, fill = "blue") +
  scale_x_continuous(breaks = 1:nrow(tob2),
                     labels = tob2$V3) +
  ggtitle("Green = smoking prevalence; Blue = strength of laws") +
  xlab("Ones produce tobacco") + ylab("prev and law strength (green and blue)")



############# THIS ATTEMPT FAILED. I PROBABLY HAVE WRONG INITS OR SOMETHING #############
# Data:
# prev = tob[,1]
# laws = tob[,2]
# data = list("prev","laws")
# 
# # Inits:
# inits = function(){
#   list(b0 = .05,
#        b1 = 5,
#        var = 2)
# }
# parameters = c("b0","b1","var")
# 
# # Model:
# tobmod <- function(){
#   # priors:
#   b0~dnorm(0,.01)
#   b1~dnorm(0,.01)
#   prec<-1/var
#   var~dunif(0,500)
#   
#   # likelihood:
#   for (i in 1:10){
#     prev[i]~dnorm(mu[i],prec)
#     mu[i]<-b0+b1*laws[i]
#   }
# }
# setwd("/Users/jamescutler/Desktop/Bayesian/")
# write.model(tobmod,"tobmod.txt")
# model.file = paste(getwd(),"tobmod.txt",sep = "/")
# file.show("tobmod.txt")
# 
# OpenBUGS.pgm = "/Applications/OpenBUGS323/OpenBUGS.exe"
# WINE = "/usr/local/Cellar/wine/3.0.4/bin/wine"
# WINEPATH = "/usr/local/Cellar/wine/3.0.4/bin/winepath"
# 
# tob.sim = bugs(data, inits, parameters.to.save = parameters, model.file = model.file,
#                n.chains = 3, n.iter = 10000,
#                OpenBUGS.pgm = OpenBUGS.pgm,
#                WINE = WINE,
#                WINEPATH = WINEPATH,
#                useWINE = TRUE)
# print(tob.sim)
# 
# tob.coda = read.bugs(tob.sim)
#########################################################################################


tox = data.frame(traffic = rep(c("wreck","no_wreck"),c(11+14,170+526)),
                 test = c(rep(c("pos","neg"),c(11,14)),
                          rep(c("pos","neg"),c(170,526)) ) )
t.tox = table(tox); t.tox
chisq.test(t.tox)

# There is evidence that traffic accidents are associated with RhD test results at the 
## alpha = .05 level of significance.

(11/170)/(14/526)
2460-69
709-17

RhD = data.frame(traffic = rep(c("wreck","no_wreck"),c(17+69,692+2391)),
                 test = c(rep(c("pos","neg"),c(17,69)),
                          rep(c("pos","neg"),c(692,2391)) ) )
tRhD = table(RhD); tRhD
chisq.test(tRhD)

sum(540,181,2460,709) # 3890! Yay!

526+170
69+17
2391+692

# tot = data.frame(traffic = rep(c("wreck","no_wreck"),c(25+86,696+3083)),
#                  test = c(rep(c("neg","pos","neg","pos"),c(14,11,69,17)),
#                           rep(c("neg","pos","neg","pos"),c(526,170,2391,692)) ),
#                  RhD = rep(c("Rneg","Rpos","Rneg","Rpos"),c(25,86,696,3083)) )
# ttot = table(tot); ttot
# chisq.test(ttot) # DARN

# THE CORRECT WAY TO GET A CONTINGENCY TABLE THAT WILL WORK FOR THE CHI SQUARED TEST:
tot = data.frame(traffic = rep(c("wreck","no_wreck"),c(25+86,696+3083)),
                 RhD.tox = c(rep(c("rneg","rpos","Rneg","Rpos"),c(14,11,69,17)),
                             rep(c("rneg","rpos","Rneg","Rpos"),c(526,170,2391,692)) ) )
ttot = table(tot); ttot
chisq.test(ttot)



# Bipolar and toxoplasmosis:
bipo = data.frame(test = rep(c("pos","neg"),c(80+41,30+65)),
                  bipolar = c(rep(c("case","control"),c(80,41)),
                              rep(c("case","control"),c(30,65)) ) )
tbipo = table(bipo); tbipo
chisq.test(tbipo)
80/121


################################################################################
################################################################################

# HW 2 - SECTION II: COMPARING CONTINUOUS OUTCOMES

dcc = read.csv("/Users/jamescutler/Desktop/Bayesian/dcc.csv")
head(dcc,5)
plot(dcc$fc_kid ~ factor(dcc$bleach))

dcc %>%
  filter(fc_kid, bleach == 1) # THIS JUST PRODUCES A NEW DATA FRAME

dcc1 = filter(dcc, bleach == 1) # THIS DOES EXACTLY THE SAME THING
dcc2 = filter(dcc, bleach == 2)
hist(dcc1$fc_kid, col = rgb(1,0,0,.5), breaks = 20,
     main = "# of coliform bacteria on kids \n hands w / wo bleach (blue / red)",
     xlab = "log of # of bacteria found")
hist(dcc2$fc_kid, col = rgb(0,0,1,.5), breaks = 10, add = TRUE)

cat(dcc1$fc_kid, sep = ",")
cat(dcc2$fc_kid, sep = ",")

cat(dcc$fc_kid, sep = ",")
cat(dcc$bleach, sep = ",")

head(dcc,5)
unique(dcc$region)

mod1 = aov(fc_kid ~ factor(region), data = dcc); summary(mod1)
anova(mod1)
TukeyHSD(mod1, conf.level = .95)
means = tapply(dcc$fc_kid, dcc$region, mean); means




################################################################################
################################################################################


# Exam I takehome

CI.for1prop = function(CI,prop1,n1){
  alpha = 1 - CI
  z = qnorm(1 - alpha/2)
  me = z*sqrt( prop1*(1-prop1) / n1 )
  ub = prop1 + me
  lb = prop1 - me
  print(sprintf("We are %s percent confident that the true population proportion is between %.4f and %.4f",CI*100,lb,ub))
}
CI.for1prop(.95,99/609,609)


# With US prior
OWmodel <- function(){
  # likelihood
  overweight ~ dbin(propOver,nTotal)
  
  # prior
  propOver ~ dbeta(8.8,22.4)
  
}

# Data
nTotal = 609
overweight = 99
data = list("overweight","nTotal")

#inits
inits = function(){
  list(propOver = .15)
}
parameters = c("propOver")


setwd("/Users/jamescutler/Desktop/Bayesian/")
write.model(OWmodel,"OWmodel.txt")
model.file = paste(getwd(),"OWmodel.txt",sep = "/")
file.show("OWmodel.txt")

OpenBUGS.pgm = "/Applications/OpenBUGS323/OpenBUGS.exe"
WINE = "/usr/local/Cellar/wine/3.0.4/bin/wine"
WINEPATH = "/usr/local/Cellar/wine/3.0.4/bin/winepath"

OW.sim = bugs(data, inits, parameters.to.save = parameters, model.file = model.file,
                   n.chains = 3, n.iter = 2000, codaPkg = TRUE,
                   OpenBUGS.pgm = OpenBUGS.pgm,WINE = WINE, WINEPATH = WINEPATH,
                   useWINE = TRUE)
# print(OW.sim)
# worked, but didn't print out the stats table, probably because I did codaPkg = TRUE
# Instead, it did:
# 004f:err:ole:CoReleaseMarshalData IMarshal::ReleaseMarshalData failed with error 0x8001011d
# > print(OW.sim)
# [1] "/private/var/folders/25/58m9pz997ds3rr38jpjw1lxh0000gn/T/RtmpIfVOQt/CODAchain1.txt"
# [2] "/private/var/folders/25/58m9pz997ds3rr38jpjw1lxh0000gn/T/RtmpIfVOQt/CODAchain2.txt"
# [3] "/private/var/folders/25/58m9pz997ds3rr38jpjw1lxh0000gn/T/RtmpIfVOQt/CODAchain3.txt"
# ... a trio of coda chain paths or something.
# But after that all the plots with coda and mcmc and lattice worked. So, success!


# Part II

control = c(42.1,36.13,51.82,40.74,38.12,36.7,41.97,41.85,32.21,35.52,42.35,38.44)
tx = c(35.94,45.14,37.56,38.68,36.29,30.53,31.02,38.04,41.14,36.09,40.42,29.40)
FiO2 = data.frame(vals = c(control,tx),
                  grps = rep(c("control","tx"),c(length(control),length(tx)) ) )

plot(FiO2$vals ~ factor(FiO2$grps))
plot(1:length(control),control, type = "b", ylim = c(20,60))
points(1:length(tx),tx, type = "b", col = "red")

t.test(control,tx, alternative = "greater")
sd(control)
sd(tx)
1/.0001
500^2
hist(control)
hist(tx)
qqnorm(control, main = "Control")
qqline(control, col="red")
qqnorm(tx, main = "Treatment"); qqline(tx, col="red")
mean(control)
mean(tx)



################################################################################
################################################################################

# In class - Thursday March 28

anx = data.frame(vals = c(90,88,85,83,88,72,62,43),
                 gender = c(0,0,0,0,1,1,1,1))
plot(anx$vals[1:4], type = "l", col = "blue", ylim = c(40,100))
points(anx$vals[5:8], type = "l", col = "red")




################################################################################
################################################################################

# Teen births (age 15-19) by county - from data.gov

tb = read.csv("/Users/jamescutler/Desktop/Bayesian/NCHS_-_Teen_Birth_Rates_for_Age_Group_15-19_in_the_United_States_by_County.csv")
length(which(tb[,1] == 2015))
nrow(tb)/13
tb15 = tb[which(tb[,1] == 2015),c(1,2,3,7,8,9)]
tb15$Year = 1:nrow(tb15)
colnames(tb15)[1] = "rownum"

# Highest counties by state (df with only highest county in each state):
maxs = tapply(tb15$Birth.Rate,factor(tb15$State),max)
sort(as.matrix(maxs))
par(mar=c(8,4,2,1))
barplot(maxs, las = 2)
rownames(maxs)
statemaxs = data.frame(states = rownames(maxs),
                    birth.rates = maxs)

rownames(states) = 1:nrow(statemaxs)
barplot(statemaxs[order(statemaxs$birth.rates),2],
        names.arg = statemaxs[order(statemaxs$birth.rates),1],
        las = 2)

# County average by state:
means = tapply(tb15$Birth.Rate,tb15$State,mean)
statemeans = data.frame(states = rownames(means),
                        birthrates = means)
rownames(statemeans) = 1:nrow(statemeans)
barplot(statemeans[order(statemeans$birthrates),2],
        names.arg = statemeans[order(statemeans$birthrates),1],
        las = 2)

par(mfrow=c(2,1))
par(mar=c(8,3,1,1))
barplot(statemeans[order(statemeans$birthrates),2],
        names.arg = statemeans[order(statemeans$birthrates),1],
        las = 2)
barplot(statemaxs[order(statemaxs$birth.rates),2],
        names.arg = statemaxs[order(statemaxs$birth.rates),1],
        las = 2)
par(mfrow=c(1,1))
par(mar=c(3,8,1,2))
barplot(statemeans[order(statemeans$birthrates),2],
        names.arg = statemeans[order(statemeans$birthrates),1],
        las = 1, horiz = TRUE)
par(mar=c(5.1,4.1,4.1,2.1))
dems = c("Maine",
  "New Hampshire",
  "Vermont",
  "Massachusetts",
  "Rhode Island",
  "Connecticut",
  "New Jersey",
  "Delaware",
  "Maryland",
  "District of Columbia",
  "New York",
  "Virginia",
  "Illinois",
  "Minnesota",
  "Colorado",
  "New Mexico",
  "Nevada",
  "Washington",
  "Oregon",
  "California",
  "Hawaii")
# which(statemeans$states == dems) # WHAT THE HECK
demindex = vector()
for (i in 1:length(dems)){
  demindex[i] = which(statemeans$states == dems[i])
}

statemeans$pres.col.16 = rep(NA,nrow(statemeans))
statemeans$pres.col.16[demindex] = "Dem"
statemeans$pres.col.16[-demindex] = "Rep"

sm.ord = statemeans[order(statemeans$birthrates),]; head(sm.ord,5)

ggplot(sm.ord, aes(x = reorder(states,-birthrates),birthrates, fill = pres.col.16)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = .9)) +
  labs(title = "Teen (15-19 yrs) pregnancy birth rates by state",
       x = "", y = "Birth rates per 1,000 females aged 15-19") + 
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "gray")) +
  scale_fill_manual(values = alpha(c("blue","red"),.7), # Blue HAS to be first, red SECOND, otherwise the Dems and Reps will have the wrong colors
                    name = "2016 Pres. Elec.") 
  






############# from Vincent R datasets ############# 

# Familial Adenomatous Polyposis
## From, Giardiello et al. - "Treatment of Colonic and Rectal Adenomas with Sulindac in Familial Adenomatous Polyposis" (NEJM 1993)
## https://www.nejm.org/doi/full/10.1056/NEJM199305063281805

fap = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/polyposis.csv")

head(fap,4)
length(which(fap$treatment == "placebo"))

fap$bayestx = rep(NA,nrow(fap))
fap[which(fap$treatment == "active"),7] = 1
fap[which(fap$treatment != "active"),7] = 0

fap$bayesex = rep(NA,nrow(fap))
fap[which(fap$sex == "male"),8] = 1
fap[which(fap$sex == "female"),8] = 0
fap$perch = round((fap$number3m-fap$baseline)/fap$baseline,4)
fap = fap[,-1]

# Outcome by treatment group:
plot((fap$number3m - fap$baseline)/fap$baseline ~ factor(fap$treatment))
abline(h = 0, col = "gray", lty = 2)

# Outcome by age:
plot(fap$age,(fap$number3m - fap$baseline)/fap$baseline)
abline(h = 0, col = "gray", lty = 2)
mod1 = lm((fap$number3m - fap$baseline)/fap$baseline ~ age, data = fap)
cor(fap$age,(fap$number3m - fap$baseline)/fap$baseline)
abline(mod1, col = "red")

# Outcome by sex:
plot((fap$number3m - fap$baseline)/fap$baseline ~ factor(fap$sex))


fap %>% 
  select(treatment,sex) %>% 
  filter(sex == "female") %>% 
  count(treatment == "active")

fap %>% 
  select(treatment,sex) %>% 
  filter(sex == "male") %>% 
  count(treatment == "active")

#############################################
tx.props = 
fap %>%
  group_by(sex,treatment) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))      # INTERESTING SUCCESS

barplot(tx.props$prop, col = tx.props$sex,
        names.arg = tx.props$treatment)
legend("top",
       legend = c("female","male"),
       fill = c("black","red"))
#############################################

means = tapply(fap$age, fap$treatment, mean)
means
SDs = tapply(fap$age, fap$treatment, sd)
SDs

summary(fap$age)
sort(fap$age)
hist(fap$age)

# teens = 
# fap %>%
#   select(age) %>%
#   filter(age < 20)
# length(teens$age)/22

# from data.table:
fapt = fread("/Users/jamescutler/Desktop/Data_Course_cutler/polyposis.csv")
teen.tx = fapt[age < 20 & treatment == "active", .N]
teen.tx
teen.tot = fapt[age<20, .N]
teen.tot
teen.tx/teen.tot # NOT BAD. I WISH THERE WAS A SLIGHTLY MORE EFFICIENT WAY THOUGH ...
# ... like, prop.table or something.

qqnorm((fap$number3m-fap$baseline)/fap$baseline)
qqline((fap$number3m-fap$baseline)/fap$baseline)

hist((fap$number3m-fap$baseline)/fap$baseline)
plot(x=(fap$number3m-fap$baseline)/fap$baseline,rep(1,nrow(fap)))

tx = fapt[treatment == "active",round((number3m-baseline)/baseline,4)]
pl = fapt[treatment == "placebo",round((number3m-baseline)/baseline,4)]
plot(x=pl,y=rep(1.1,length(pl)), xlim = c(-1,1))
points(x=tx,y=rep(1,length(tx)), col = "red")
points(mean(pl),1.1, pch = 18, col = "orange")
points(mean(tx),1, pch = 18, col = "green")

length(pl)
length(tx)

sd(tx)
1/var(tx)
mean(pl)
1/var(pl)
sd(pl)

cat(tx, sep = ",")
cat(pl, sep = ",")



################################################################################
################################################################################

# Logisitic regression notes - hospital information sharing 2 code and R code:

x = seq(0,1,length.out = 1000)
dist.beta = dbeta(x,11,79)
plot(x,dist.beta, type = "l", lwd = 2, col = "blue")


x = seq(0,1,length.out = 100)
dist.beta = dbeta(x,36,16)
plot(x,dist.beta, type = "l", lwd =2, col = "blue")

#####################

# To take stats from OpenBUGS and convert them into a csv, you have to fiddle
## around with them in Word first, and then once they look clean in there you
## can just paste it into Excel and for some reason it works. You might need 
## to fiddle around with the column names a little in Excel. 

# I love this confidence interval / credible interval plotting stuff:
dep = read.csv("/Users/jamescutler/Desktop/Bayesian/Bayesian_meta.analysis_antidepress.csv")
dep = dep[-nrow(dep),-1]
colnames(dep)[1:3] = c("parameter","Mean","SD")

dep2 = dep[3:nrow(dep),]

x = 1:nrow(dep2)
plot(x,dep2$Mean, col = "blue", ylim = c(0,1))
segments(x,dep2$val2.5,x,dep2$val97.5, col = "blue") # AMAZING! 
abline(h = .5, col = "gray")



####################################################################################
####################################################################################

# The real code for this stuff is in the R file called "Bayesian_Adaptive_Clinical_Trials"

n1 = 117
x1 = 110

n2 = 50

alpha = 1
beta = 1

B = 10000

p.star = rbeta(B, x1+alpha, n1-x1+beta)

hist(sample(p.star,100, replace = FALSE),
     breaks = 20)

x2.star = vector()
for (i in 1:B){
  x2.star[i] = rbinom(1,n2,p.star[i])
}

hist(sample(x2.star,100, replace = FALSE))


# CHECK THIS OUT:
qbeta(c(.025,.975), x1+x2.star[1]+alpha, n1+n2-x1-x2.star[1]+beta)                # see what this gives you
as.matrix(qbeta(c(.025,.975), x1+x2.star[1]+alpha, n1+n2-x1-x2.star[1]+beta))     # see what this gives you
t(as.matrix(qbeta(c(.025,.975), x1+x2.star[1]+alpha, n1+n2-x1-x2.star[1]+beta)))  # see what this gives you


BCI = matrix(0,B,2)
for (i in 1:B){
  BCI[i,] = t(as.matrix(qbeta(c(.025,.975), x1+x2.star[i]+alpha,n1+n2-x1-x2.star[i]+beta)))
}





####################################################################################
####################################################################################

# Poisson regression on fecal colliform data:

fc_kid=c(-0.8366969,0.116495,-0.1307197,-0.2091515,0,-0.060206,0.3182129,0.1690196,0.1690196,0.5795202,0.2483658,0.0795202,0.060206,-0.099485,-0.5,-0.060206,-0.0430043,-0.278899,-0.2091742,-0.5376288,-0.279588,-0.4183484,0.0954242,0.0860086,0.2985578,-0.142312,0.0596402,-0.330103,-0.2063144,-0.2357879,-0.3333333,-0.5501717,-0.060206,0.0501717,0.2486232,-0.2,-0.2357879,-0.330103,-0.330103,-0.025817,-0.2357879,-0.0172372,0.3134689,-0.430103,-0.3584192,0.2816066,-0.3072164,-0.1858614,-0.079588,0.180618,0.3225568,0.1147201,-0.2750858,-0.1428571,-0.4126287,-0.4126287,-0.4126287,-0.2357879,-0.2091742,0.3728404,-0.074697,-0.0871465,-0.2750858,0.1003433,0.0954242,-0.1666667,-0.2357879,0.2385606,-0.139794,-0.2357879,-0.2750858,0.2755354,0.0860086,0.4203103,0.1960252,0.0501717,-0.139794,-0.2,0.1541687,-0.2123712,-0.1692088,-0.1742929,-0.7168384,0.2723456,-0.2750858,0,0.3043458,0.24271,0.1988831,0.150515,-0.2357879,0.2385606,0.0795202,-0.1887744,-0.5376288,-0.139794,-0.4183484,-0.1219817,-0.2857143,-0.8252575,-0.2857143,0.1747425,0)

class=c(1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,1,2,1,2,1,2,1,2,1,2,1,2,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)

region=c(1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)

clean=c(2,2,2,2,2,2,2,2,2,2,2,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,2,1,1,2,2,2,2,1,1,2,2,1,2,2,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,2,1,1,2,2,2,2,1,1,1,1,1,1,2,2,1,1,2,2,2,2,2,2,1,1,2,2,2,2,2,2,2,2,2,2,2,2)

bleach=c(2,1,1,1,1,2,2,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,2,1,1,1,1,1,1,2,2,1,1,1,1,1,1,2,2,1,1,1,1,1,1,1,2,2,1,1,1,1,2,2,1,1,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2,2,2,2,2,2,2,2,2,1,1,2,2,2,2,1,1,2,2,2,2,1,1,2,2,1,1,2,2,1,1,2,2)

kid = data.frame(kids = fc_kid,
                 classes = class,
                 regions = region,
                 limpio = clean,
                 cloro = bleach)
plot(kid$kids ~ factor(kid$cloro))
titles = c("","fc_kid by class","fc_kid by region","fc_kid by clean","fc_kid by bleach")
par(mfrow=c(2,2))
for (i in 2:ncol(kid)){
  plot(factor(kid[,i]),kid[,1], main = titles[i])
}
par(mfrow=c(1,1))




####################################################################################
####################################################################################

# Lung cancer example:

cancer = read.delim("/Users/jamescutler/Desktop/Bayesian/lungcancerdata.dat",
                    sep = "\t")

cancer$location.cancer = as.character(cancer$location.cancer)

?substr
cancer$cancer = substr(cancer[,1],2,3)
cancer$location = substr(cancer[,1],1,2)
cancer = cancer[,2:3]





####################################################################################
####################################################################################

# Sunspots time series prediction

counts = c(100.8, 81.6, 66.5, 34.8, 30.6, 7, 19.8, 92.5,
           154.4, 125.9, 84.8, 68.1, 38.5, 22.8, 10.2, 24.1, 82.9,
           132, 130.9, 118.1, 89.9, 66.6, 60, 46.9, 41, 21.3, 16,
           6.4, 4.1, 6.8, 14.5, 34, 45, 43.1, 47.5, 42.2, 28.1, 10.1,
           8.1, 2.5, 0, 1.4, 5, 12.2, 13.9, 35.4, 45.8, 41.1, 30.4,
           23.9, 15.7, 6.6, 4, 1.8, 8.5, 16.6, 36.3, 49.7, 62.5, 67,
           71, 47.8, 27.5, 8.5, 13.2, 56.9, 121.5, 138.3, 103.2,
           85.8, 63.2, 36.8, 24.2, 10.7, 15, 40.1, 61.5, 98.5, 124.3,
           95.9, 66.5, 64.5, 54.2, 39, 20.6, 6.7, 4.3, 22.8, 54.8,
           93.8, 95.7, 77.2, 59.1, 44, 47, 30.5, 16.3, 7.3, 37.3,
           73.9)
plot(1770:1869,counts, type = "b")




