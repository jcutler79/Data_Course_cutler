### Applied Bayesian

# My unchanging three paths:
OpenBUGS.pgm = "/Applications/OpenBUGS323/OpenBUGS.exe"
WINE = "/usr/local/Cellar/wine/3.0.4/bin/wine"
WINEPATH = "/usr/local/Cellar/wine/3.0.4/bin/winepath"

# Libraries:
library(stringr)

# install.packages("R2OpenBUGS")
library(R2OpenBUGS)
library(MASS)


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
smokemodel <- function(){
  # prior
  p ~ dbeta(1,1)
  # likelihood
  y ~ dbin(p,N)
}
# Bayesian HW 1



normmodel <- function(){
  y ~ dnorm(25,4)
}
setwd("/Users/jamescutler/Desktop/Bayesian/")
write.model(smokemodel, "smokemodel.txt")
model.file = paste(getwd(),"smokemodel.txt",sep = "/")
file.show("smokemodel.txt")

OpenBUGS.pgm = "/Applications/OpenBUGS323/OpenBUGS.exe"
WINE = "/usr/local/Cellar/wine/3.0.4/bin/wine"
WINEPATH = "/usr/local/Cellar/wine/3.0.4/bin/winepath"


bernmodel <- function(){
  y ~ dbern(.4)
}

betamodel <- function(){
  y ~ dbeta(25,3)
}


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









