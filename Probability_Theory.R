# Probability Theory


### Probability Theory grades:
sum(110,100,100,150)
sum(8.5,10,10,8,10,10,9,10,10,10,8) # not sure what hw#11 is yet but guessing it's an 8
150*.6 # = 90 on the final
sum(103.5,89,92,128)/460 # This would be a B in the class


### Things to learn to do in R:
## 1. 2x2 tables - custom tables with p-values
## 2. Write a function for JMP-like summary statistics for dataframe data (like NC births)
## 3. 

################################################################################################
################################################################################################
journ = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Journals82.csv", header = FALSE)
################################################################################################
################################################################################################

numerator = .967*.0038; numerator
denominator = (1-.0038)*.015 + .0038*.967; denominator
numerator/denominator



right = vector()
for (i in 1:100000){
  guess = sample(1:4,1)
  right[i] = ifelse(guess == 1,1,0)
}
sum(right)/length(right)

right = vector()
for (i in 1:100000){
  oneortwo = sample(1:2,1) # Offers two possibilities, which are described in the if else set up below:
  if (oneortwo == 2) { # In this possibility, there are two right answers
    correct = sample(1:4,2,replace = FALSE)
    guess = sample(1:4,1)
    right[i] = ifelse(guess %in% correct,1,0)
  }else{ # In this possibility, there is only one right answer
    correct = sample(1:4,1)
    guess = sample(1:4,1)
    right[i] = ifelse(guess == correct,1,0)
  }
} # In both cases, the question is answered at random (there is no strategy here to always choose one of the "duplicated" answers)
sum(right)/length(right)
# I think this code is sound because in this scenario the right answer could be one of the duplicates, 
# or it could be one of the other two answers. 50% of the time it will be the case that one of the
# duplicates is the correct answer (so there will be two choices that will get you a point). The
# other 50% of the time there will be only one of the four choices that is the correct one. This
# is because A, B, C, and D each have a 25% chance of being right. 
## I'm guessing this shows that if you choose answers randomly, you'll be right ~37% of the time. But
## what if you always guess one of the duplicated answers? Would you get ~63% of the answers right?

win = vector()
for (i in 1:10000){
  roll = sample(1:6,4,replace = TRUE)
  win[i] = ifelse(sum(ifelse(roll == 6,1,0)) > 0,1,0)
}
prob.win = sum(win)/10000



roll = sample(1:6,4,replace = TRUE)

win[i] = ifelse(sum(ifelse(roll == 6,1,0)) > 0,1,0)


########################

outcomes = vector()
X = 12
for (i in 1:X-1){
  outcomes[i] = ncol(combn(X,i))
  print(ncol(combn(X,i)))
}
outcomes[X-(X-3)]
plot(outcomes)



for (i in 10:20){
  print(ncol(combn(i,2))) # add the number to its combination and you get the combination of the next number   
}

for (i in 10:20){
  print(ncol(combn(i,3))) # So you're always adding the previous number to the one you get here, and that tells you what the next higher number's combination will be  
}
165+55
220+66

func = function(x) x^3
integrate(func, lower = 1, upper = 5)
.25*5^4 - .25


library(combinat)
ncol(combn(11,6))
ncol(combn(7,4))
6^6
####################################################################################################
best.jobs = data.frame(jobs = c("PhD statistics",
                                "Masters biostatistics",
                                "PhD computer science",
                                "Masters human computer interaction",
                                "PhD physics",
                                "Juris Doctor",
                                "Masters telecom engineering",
                                "Masters applied math",
                                "Masters statistics",
                                "Masters engineering",
                                "Masters computer science",
                                "Masters software engineering",
                                "PhD economics",
                                "MBA",
                                "Masters information science"),
                       salaries = c(131700,
                                    113400,
                                    144800,
                                    115200,
                                    132400,
                                    138200,
                                    119100,
                                    121900,
                                    109700,
                                    117200,
                                    122100,
                                    121300,
                                    122500,
                                    113000,
                                    101800))
worst.jobs = data.frame(jobs = c("Masters interior design",
                                 "Masters educational administration",
                                 "Masters early childhood education",
                                 "Masters criminal justice",
                                 "Masters reading and literacy",
                                 "PhD educational leadership",
                                 "Masters health administration",
                                 "Masters studio art",
                                 "Masters construction management",
                                 "Masters fine arts",
                                 "Masters divinity",
                                 "Masters educational leadership",
                                 "Masters social work",
                                 "Masters leadership",
                                 "Masters curriculum and instruction"),
                        salaries = c(69400,
                                     77100,
                                     48100,
                                     60500,
                                     52300,
                                     88500,
                                     73300,
                                     51300,
                                     99600,
                                     55900,
                                     52100,
                                     72600,
                                     59400,
                                     81600,
                                     58200))
library(ggplot2)
ggplot(best.jobs, aes(1:length(jobs),salaries)) +
  geom_point(col = "green") +
  geom_point(data = worst.jobs, mapping = aes(1:length(jobs),salaries), col = "red") +
  geom_text(data = best.jobs, mapping = aes(1:length(jobs),salaries, label = jobs), size = 3, angle = 20, vjust = -.3, hjust = -.1) +
  geom_text(data = worst.jobs, mapping = aes(1:length(jobs),salaries, label = jobs), size = 3, angle = 20, vjust = -.3, hjust = -.1) +
  scale_y_continuous(breaks = seq(0,150000,10000)) + 
  coord_cartesian(xlim = c(0,18), ylim = c(40000,160000))
#########################################################################################################

permn(6) # Obviously you can't actually do permutations with this.

# MY PERMUTATION FUNCTION:
perm = function(n,r){
  theanswer = factorial(n)/factorial(n-r)
  print(prettyNum(theanswer, big.mark = ",", scientific = FALSE))
}


# MY FUNCTION FOR GETTING THE PERMUTATION AND THE COMBINATION AT THE SAME TIME:
permComb = function(n,r){
  thepermn = factorial(n)/factorial(n-r)
  PrettyPermn = prettyNum(thepermn, big.mark = ",", scientific = FALSE)
  thecombn = factorial(n)/( factorial(r)*factorial(n-r) )
  PrettyCombn = prettyNum(thecombn, big.mark = ",", scientific = FALSE)
  print(sprintf("The permutation is: %s",PrettyPermn))
  print(sprintf("The combination is: %s",PrettyCombn))
  return(c(thepermn,thecombn))
}
permComb(6,2)
permComb(50,2)

ncol(combn(5,3))
factorial(3)
perm(3,3)

smallvector = c(4,3,5)
factorial(smallvector) # IT WORKS!

factorial(10)/36

# MY FUNCTION FOR GETTING THE NUMBER OF POSSIBLE ORDERED SETS OF AN UNORDERED SET:
numofOfU = function(k,vector.of.Ms){
  thenumerator = factorial(k)
  facted.vec = factorial(vector.of.Ms)
  thedenominator = prod(facted.vec)
  theanswer = prettyNum(thenumerator/thedenominator, big.mark = ",", scientific = FALSE)
  print(theanswer)
}
numofOfU(10,c(3,3))


# MY BINOMIAL FORMULA FOR ANSWERING "AT LEAST" QUESTIONS:
bin.vec = vector()
for (i in 1:10){
  bin.vec[i] = ncol(combn(10,i))*((1/5)^i)*(4/5)^(10-i)
}
bin.vec = na.omit(bin.vec)
sum(bin.vec)


over100 = 500000
decade.starting = 2010
for (i in 1:10){
  print(c(prettyNum(decade.starting, big.mark = "", scientific = FALSE), prettyNum(over100, big.mark = ",", scientific = FALSE)))
  over100 = over100*2
  decade.starting = decade.starting + 10
}

at.least.10 = vector()
for (i in 10:20){
  at.least.10[i] = (factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i)
}
sum(at.least.10)

thesum = 0
for (i in 10:20){
  print((factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i))
  thesum = thesum + (factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i)
  print(c(i,thesum))
}

# EXAMPLE OF "NOT IN" CODE:
ac = "A B C D E F G H I J K Q R S T U V"
abc = "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
vac = unlist(strsplit(ac, " "))
vabc = unlist(strsplit(abc, " "))
vabc[! vabc %in% vac] # YOU WANT THE PART OF VABC THAT'S JUST THE VABC THAT'S NOT ALSO IN VAC ==> SO, YOU WANT THE VABC PART THAT'S EXCLUSIVE TO VABC

# Problem 1.30:
ncol(combn(11,6))
?expand.grid
trial1 = expand.grid(first = c(1,2,3), second = c(1,2,3), third = c(1,2,3))
class(trial1)
trial1[1,]
all.samps = expand.grid(first = c(1,2,7,8,14,20),
                        second = c(1,2,7,8,14,20),
                        third = c(1,2,7,8,14,20),
                        fourth = c(1,2,7,8,14,20),
                        fifth = c(1,2,7,8,14,20),
                        sixth = c(1,2,7,8,14,20))
all.samps
themeans = vector()
for (i in 1:nrow(all.samps)){
  themeans[i] = mean(all.samps[i,])
} # DOESN'T WORK
hist(themeans) # (DOESN'T WORK)

all.samps$means = mean(all.samps[,1:6]) # DOESN'T WORK

themeans = apply(all.samps,1,mean)
hist(themeans)
summary(themeans)
mean(all.samps[1,1:6])
mean(all.samps[1,])


# A PDF:
x = seq(-10,10,length.out = 10000); x
pdf.x = exp(-x)/((1 + exp(-x))^2)
plot(x,pdf.x, xlim = c(-20,20), type = "l", col = "blue")

cdf.x = 1/(1 + exp(-x))
plot(x,cdf.x, xlim = c(-20,20), type = "l", col = "red")
points(x,pdf.x, type = "l", col = "blue")



x = seq(0,1,length.out = 1000)
plot(-log(x))
abline(h = 0, v = 0, col = "grey")
y = -log(x)
plot(x,y, type = "l", col = "red")

y = function(x) exp(x)
plot(y, xlim = c(-5,5), ylim = c(-5,50))
abline(h = 0, v = 0, col = "grey")


####################################################################
### Simulating theorem 2.1.10 (Probability Integral Transformation)

x = seq(0,10e8, length.out = 100000)
y = 1 - exp(-x/5)

plot(y)
?rexp
y = rexp(1000, rate = 5)
plot(y)
mean(y)
y = rexp(1000, rate = 1/5)
plot(y)
mean(y)
plot(density(y))
hist(y, breaks = 50)
df = data.frame(X = 1:1000, Y = y)
library(ggplot2)
ggplot(df, aes(X,Y)) + 
  geom_point(alpha = .3, col = "red")

# Now:
cdf = 1 - exp(-y/5)
hist(cdf) # THERE'S THE UNIFORM DISTRIBUTION!!!!!

y = runif(10000)
hist(y)
x = -5*log(1 - y)
hist(x, breaks = 100) # AMAZING. GETS THE EXPONENTIAL DISTRIBUTION FROM THE UNIFORM (REVERSES WHAT WE JUST DID ABOVE)

15*(16/52) + (10+9+8+7+6+5+4+3+2)*(4/52)


#################################################################################

# HGeom:
ncol(combn(6,0))*ncol(combn(25-6,10))/ncol(combn(25,10))



#################################################################################
#################################################################################

# D = diseased
# ND = not diseased
# T = test positive
# I = test negative
pD = .0038
pND = .9962
pTgD = .967
pIgD = 1-.967
pTgND = .015
pIgND = 1-.015

pT = pTgD*pD + pTgND*pND; pT
pDgT = pD*pTgD/pT; pDgT

########

# leaky valve
(.13*.018)/(.13*.018 + .2*.014 + .55*.009 +.12*.003)





##########################################################################################
##########################################################################################

### EXAM II PART (CHAPTER 3 AND 4)

# Chi-square distributions with different df's:
fx = function(x) {
  df = 30
  alpha = df/2
  beta = 2
  (1/gamma(alpha)*beta^alpha)*(x^(alpha-1))*exp(-x/2)
}
fx1 = function(x) {
  df = 40
  alpha = df/2
  beta = 2
  (1/gamma(alpha)*beta^alpha)*(x^(alpha-1))*exp(-x/2)
}
fx2 = function(x) {
  df = 50
  alpha = df/2
  beta = 2
  (1/gamma(alpha)*beta^alpha)*(x^(alpha-1))*exp(-x/2)
}
ub = 1e4 # WHY CAN'T I GET MY WORKING PORTION OF THE DISTN. TO MOVE TO THE RIGHT WITHOUT IT BLOWING UP VERTICALLY?
xub = 500
curve(fx, from = 0, to = xub, ylim = c(0,ub), n = 1000)
curve(fx1, from = 0, to = xub, ylim = c(0,ub), n = 1000, col = "red", add = TRUE)
curve(fx2, from = 0, to = xub, ylim = c(0,ub), n = 1000, col = "green", add = TRUE)


####################################################################################

### HW 7


q = 0
m = 6 # actually, 6 or more, so P(X > x) of lower.tail FALSE would require x to be 5
n = 100
k = 30
phyper(q,m,n,k, lower.tail = TRUE)

myhypergeom = function(N,M,K,x) {
  p = ( factorial(M)/( factorial(x)*factorial(M-x) ) )*( factorial(N-M)/( factorial(K-x)*factorial((N-M)-(K-x)) ) )/( factorial(N)/( factorial(K)*factorial(N-K) ) )
  print(sprintf("The probability of getting a %s -size lot with %s bad items if their sample of size %s has %s bad items in it is %s",N,M,K,x,p))
}
myhypergeom(N=100,M=6,K=5,x=0) # trial and error
myhypergeom(N=100,M=6,K=15,x=0) # trial and error
myhypergeom(N=100,M=6,K=25,x=0) # trial and error
myhypergeom(N=100,M=6,K=35,x=0) # trial and error
myhypergeom(N=100,M=6,K=31,x=0) # just over (0.10056)
myhypergeom(N=100,M=6,K=32,x=0) # just under (0.0918)

# ncol(combn(94,30)) # impossible to compute in R
# prettyNum(ncol(combn(30,11)), big.mark = ",", scientific = FALSE) # takes too long to compute in R

myhygeom.addition = function(N,M,K,x1,x2) {
  p = ( factorial(M)/( factorial(x1)*factorial(M-x1) ) )*( factorial(N-M)/( factorial(K-x1)*factorial((N-M)-(K-x1)) ) )/( factorial(N)/( factorial(K)*factorial(N-K) ) ) +
    ( factorial(M)/( factorial(x2)*factorial(M-x2) ) )*( factorial(N-M)/( factorial(K-x2)*factorial((N-M)-(K-x2)) ) )/( factorial(N)/( factorial(K)*factorial(N-K) ) )
  return(p)
}
myhygeom.addition(N=100,M=6,K=30,x1=0,x2=1)
myhygeom.addition(N=100,M=6,K=20,x1=0,x2=1)
myhygeom.addition(N=100,M=6,K=51,x1=0,x2=1) # bingo


### 3.5
probvec = vector()
for (i in 85:100){
  probvec[i-84] = (factorial(100)/( factorial(i)*factorial(100-i) ))*(.8^i)*.2^(100-i) 
}
probvec
sum(probvec)


### 3.10

newhgeom = function(cN){
  p = ( factorial(cN)/( factorial(4)*factorial(cN-4) ) )*( factorial(496-cN)/( factorial(2)*factorial(496-cN-2) ) )
  print(p)
}
newhgeom(cN=320)

( factorial(496)/( factorial(4)*factorial((496)-4) ) )*(factorial(492)/( factorial(2)*factorial(492-2) ) )

factorial(496)/( factorial(4)*factorial(496-4) )

factorial(492)


############################################################################
############################################################################

# Matrix of Beta distribution plots:
x = seq(0,1,length=100)

par(mfrow=c(5,5), mar=c(3,3,0.5,0.5) )
z = c(0.5,1,2,3,4)
for(i in z){
  for(j in z){
    a = j
    b = i
    y = dbeta(x,a,b)
    
    plot(x,y,ylim=range(0,3.0),type="l",lwd=2,col="blue",main=NULL,ylab=" ",xlab=" ")
    legend("top","center",paste("a=",a,", b=",b),bty="n")
    mtext(expression(paste("p(",theta,")")),2,line=2,cex=.75)
    mtext(expression(theta),1,line=2,cex=.75)
  }
}


#############################################################################
#############################################################################

library(tidyr)

smarties = c(100,99,106,110,100,101,104,102,96,99,
             106,103,103,100,115,99,99,105,105,100,
             104,102,104,100,104,105,106,112,106,119,
             119,105,106,105,102,98,97,101,100,106,
             103,105,98,110,102,106,102,106,92)
length(smarties)
hist(smarties, breaks = 10)
mean(smarties)
sd(smarties)

nsmarties = smarties/100
m = mean(nsmarties)
sd(nsmarties)
v = var(nsmarties)

s = data.frame(matrix(NA, nrow = 100, ncol = 3))
for (i in 1:100){
  s[i,] = sample(nsmarties,3)
}
# slong = gather(s, key = "obs", value = "weights", c("X1","X2","X3"))
# slong$mean = tapply(slong$weights,slong$obs,mean)
# slong = slong[,-3] # This was a dead end! I don't want 3 means! I want 100!

mymeans = vector()
for (i in 1:100){
  mymeans[i] = mean(c(s[i,1],s[i,2],s[i,3]))
}
mymeans
s$means = mymeans

?var() # THIS FUNCTION USES n-1 AS THE DENOMINATOR! SO IT SHOULDN'T BE BIASED!
myvars = vector()
for (i in 1:100){
  myvars[i] = var(c(s[i,1],s[i,2],s[i,3]))
}
myvars
# prettyvars = prettyNum(myvars, scientific = FALSE)
s$vars = myvars
# s$pvars = prettyvars

# THE HISTOGRAM OF THE VARIANCES IS ACTUALLY REALLY INTERESTING (WHAT DISTRIBUTION DOES IT FOLLOW??),
# THE HISTOGRAM OF THE MEANS IS NOT THAT NORMAL FOR A HUNDRED SAMPLE MEANS.
hist(s$means, breaks = 30)
hist(s$vars, breaks = 30)

a = 7
plot(1:a,rep(v,a), ylim = c(0,max(s$vars)), type = "l", lwd = 2)
v1 = sample(s$vars,a); points(1:a, v1, col = "red") #; round(v1,5)
v2 = sample(s$vars,a); points(1:a, v2, col = "green") #; round(v2,5)
v3 = sample(s$vars,a); points(1:a, v3, col = "blue") #; round(v3,5)
v4 = sample(s$vars,a); points(1:a, v4, col = "orange") #; round(v4,5)
v5 = sample(s$vars,a); points(1:a, v5, col = "yellow") #; round(v5,5)
v6 = sample(s$vars,a); points(1:a, v6, col = "purple") #; round(v6,5)
v7 = sample(s$vars,a); points(1:a, v7, col = "pink") #; round(v7,5)

M = data.frame(matrix(NA, nrow = 7, ncol = 7))
for (i in 1:7){
  for (j in 1:7){
    M[i,j] = eval(parse(text = paste0("v",i)))[j]
  }
}
Mmeans = vector()
for (i in 1:7){
  Mmeans[i] = mean(M[,i])
}
Mmeans
points(1:a, Mmeans, pch = 18, col = "red")

thevars = c(v1,v2,v3,v4,v5,v6,v7); thevars
mean(thevars)
points(4,mean(thevars), pch = 22, col = "blue")


#############################################################################
#############################################################################

# 3.36 in Statistical Inference 2nd ed.
curve((63/4)*(x^6 - x^8), from = -1, to = 5.5, 
      n = 1000, ylim=c(0,2))
abline(h = 0, v = 0, col = "gray")
curve((63/4)*((x-3)^6 - (x-3)^8), col = "red",
      n = 1000, add = TRUE)
curve((63/4)*(1/2)*(((x-3)/2)^6 - ((x-3)/2)^8), col = "blue", 
      n = 1000, add = TRUE)
abline(h = .75, col = "gray", lty = 2)


qnorm(.05, lower.tail = FALSE)

pnbinom(5,20,.5)
rnbinom(20,5,.5)



#############################################################################
#############################################################################

sample(1:276,9)
M = as.data.frame(matrix(NA, nrow = 4, ncol = 9))
for (i in 1:nrow(M)){
  M[i,] = sample(1:276, 9)
}
k1 = max(M[1,]); k1
m1 = ncol(M); m1
Nhat1 = ((k1+1)/k1)*m1 - 1; Nhat1
N1b = m1 + m1/k1 - 1; N1b


#############################################################################


dbinom(7,22,.5)
dnbinom(7,22,.5)
pbinom(7,22,.5, lower.tail = TRUE)
pnbinom(7,22,.5, lower.tail = TRUE)

1-(5/6)^4

1 - (35/36)^24







#############################################################################
#############################################################################
#############################################################################
#############################################################################

#############################################################################
################################# MATH STATS ################################ 
#############################################################################

x = qnorm(.2)
(1.28-x)^2
x

(((.84+1.645)*pi/.5)^2)/3


HW = c(48/50,
       33/40,
       64/70,
       58/60,
       34/40,
       36/40,
       37/45,
       28/30,
       33/45); HW = mean(HW); HW = HW*100*.25; HW; HW/.25
E1 = 29/40; E1 = E1*100*.2; E1; E1/.2
E2 = 44/50; E2 = E2*100*.25; E2; E2/.25
E3 = 40/60; E3 = E3*100*.3; E3; E3/.3
grade = sum(HW,E1,E2,E3); grade

sum(1:7)/7
mean(1:7)
gamma(.5)/sqrt(pi)

pnorm(-3)
z = seq(-3,3,1); z
w = vector()
for (i in 1:length(z)){
  w[i] = pnorm(z[i])
}
w[5] - w[3]
w[6] - w[2]
w[7] - w[1]

a = runif(5,0,100); a
b = runif(10,0,100); b
c = runif(50,0,100); c

median(a)
median(b)
median(c)
sample(a,1)
sample(b,1)
sample(c,1)


### FROM TOPIC 2 NOTES, PART ABOUT THE VARIANCE OF THE MEDIAN:
## Compare the means and medians of 1000 random samples of size n = 3 from 
# the uniform(0,1):
mymedians = vector()
mymeans = vector()
for (i in 1:1000){
  x.vec = runif(3,0,1)
  mymedians[i] = median(x.vec)
  mymeans[i] = mean(x.vec)
}
par(mfrow = c(2,1))
hist(mymedians)
hist(mymeans)

var(mymedians) # compare to 1/20
var(mymeans)
mean(mymedians)
mean(mymeans)



#############################################################################
#############################################################################


# See R file for Topic 3 Bernoulli variance (saved in math_stats folder)



#############################################################################
#############################################################################

5/(exp(-1.96/10))
5/(exp(1.96/10))

5/(1 + (1.96/10))
5/(1 - (1.96/10))


#####

# 1/sigma * sum(Xi) vs. sum(Xi/sigma):

(1/3)*sum(1,2,3,4,5,6,7,8)
sum(1/3,2/3,3/3,4/3,5/3,6/3,7/3,8/3)

# prod(Xi^alpha) vs. (prod(Xi))^alpha:

(prod(1,2,3,4,5,6,7,8))^3
prod(1^3,2^3,3^3,4^3,5^3,6^3,7^3,8^3)


#####
n = 8
vec = c(1,2,3,4,5,6,7,8)
Xbar = (1/n)*sum(vec)
vecXbar = vec*Xbar
sum(vecXbar)      # = 162
n*Xbar^2          # = 162!!

(1/n)*n*Xbar
n*vec^2



#####################################################################
#####################################################################

# Chapter 5, Topic 3 - CONVERGENCE

# WEIRD ASYMPTOTICALLY APPROXIMATE CIs THAT BECOME BETTER CENTERED WITH 
## INCREASING SAMPLE SIZE:

5/(1 - (1.96/10)) # upper
5/(1 + (1.96/10)) # lower

# (1:100)*10
uppers = vector()
lowers = vector()
for (i in 1:100){
  uppers[i] = 5/(1 - (1.96/(10*i)))
  lowers[i] = 5/(1 + (1.96/(10*i)))
}
plot(uppers, ylim = c(4,6.3))
points(lowers)
abline(h = 5, col = "red")

# (1:40)*10
uppers = vector()
lowers = vector()
for (i in 1:40){
  uppers[i] = 5/(1 - (1.96/(10*i)))
  lowers[i] = 5/(1 + (1.96/(10*i)))
}
plot(uppers, ylim = c(4,6.3))
points(lowers)
abline(h = 5, col = "red")

# (1:10)*10
uppers = vector()
lowers = vector()
for (i in 1:10){
  uppers[i] = 5/(1 - (1.96/(10*i)))
  lowers[i] = 5/(1 + (1.96/(10*i)))
}
plot(uppers, ylim = c(4,6.3))
points(lowers)
abline(h = 5, col = "red")
updiffs = uppers-5; barplot(updiffs)
lowdiffs = 5-lowers; barplot(lowdiffs)
udf = data.frame(Y = updiffs, index = 1:length(updiffs))
ldf = data.frame(Y = lowdiffs, index = 1:length(lowdiffs))
ggplot() +
  geom_bar(data = udf, mapping = aes(index,Y), width = .8, stat = "identity", fill = "blue") +
  geom_bar(data = ldf, mapping = aes(index,Y), width = .4, stat = "identity", fill = "white") +
  theme_classic() + scale_y_continuous(expand = c(0,0)) # WHAT DOES THE EXPAND = c(0,0) DO??

# mean(c(uppers,lowers)) # doesn't work (only takes mean of first one)
means = vector()
for (i in 1:length(uppers)){
  means[i] = mean(c(uppers[i],lowers[i]))
}
means
plot(uppers, ylim = c(4,6.3))
points(lowers)
points(means, col = "red")



#####################################################################
#####################################################################

# Proving that Stewart Calculus Chapter 11 equation 12 is true:
mysum = 0
for (i in 1:20){
  mysum = mysum + 1/factorial(i-1)
}
mysum
exp(1)
format(mysum, nsmall = 16)
format(exp(1), nsmall = 16)



#####################################################################
#####################################################################

# Proving that the [Taylor expansion?] in Math Stats practice exam I is true:

# exp(-x) = (1 + (-x)/n)^n as n goest to infinity
x = seq(-10,5,.05)

n = 10
y = vector()
for (i in 1:length(x)){
  y[i] = (1 - x[i]/n)^n
}

n1 = 20
y1 = vector()
for (i in 1:length(x)){
  y1[i] = (1 - x[i]/n1)^n1
}

n2 = 40
y2 = vector()
for (i in 1:length(x)){
  y2[i] = (1 - x[i]/n2)^n2
}

n3 = 80
y3 = vector()
for (i in 1:length(x)){
  y3[i] = (1 - x[i]/n3)^n3
}

n4 = 160
y4 = vector()
for (i in 1:length(x)){
  y4[i] = (1 - x[i]/n4)^n4
}

s = .2
plot(  x,y4, col = "red", cex = s); abline(h = 0, v = 0, col = "gray")
points(x,y3, col = "orange", cex = s)
points(x,y2, col = "yellow", cex = s)
points(x,y1, col = "green", cex = s)
points(x,y , col = "blue", cex = s)
curve(exp(-x), add = TRUE)


# Now for exp(x) = (1 + x/n)^n as n goes to infinity:
x = seq(-5,15,.5)

n1 = 20
y1 = vector()
for (i in 1:length(x)){
  y1[i] = (1 + x[i]/n1)^n1
}

n2 = 40
y2 = vector()
for (i in 1:length(x)){
  y2[i] = (1 + x[i]/n2)^n2
}

n3 = 80
y3 = vector()
for (i in 1:length(x)){
  y3[i] = (1 + x[i]/n3)^n3
}

n4 = 160
y4 = vector()
for (i in 1:length(x)){
  y4[i] = (1 + x[i]/n4)^n4
}
plot(x,y1, col = "red", type = "l"); abline(h = 0, v = 0, col = "gray")
points(x,y2, col = "orange", type = "l")
points(x,y3, col = "yellow", type = "l")
points(x,y4, col = "green", type = "l")
curve(exp(x), add = TRUE)

# Now for exp(2x):
curve(exp(2*x), xlim = c(-5,5), ylim = c(-100,10000)); abline(h = 0, v = 0, col = "gray")
x = seq(-5,5,.1); length(x)
n1 = 20
y1 = vector()
for (i in 1:length(x)){
  y1[i] = (1 + 2*x[i]/n1)^n1
}

n2 = 40
y2 = vector()
for (i in 1:length(x)){
  y2[i] = (1 + 2*x[i]/n2)^n2
}
points(x,y1, col = "red", type = "l")
points(x,y2, col = "orange", type = "l")

mean(c(56.53,38.12))
mean(c(47.67,45.75))
#####################################

# THREE DIFFERENT ANSWERS FOR 5.23!

# Chegg 5.23:
Chegg = (exp(1)-exp(1-.5))/(1 - exp(1)); Chegg # [1] -0.6224593
Chegg_comment = (exp(1)-exp(1-.5))/(exp(1) - 1); Chegg_comment # [1] 0.6224593

# Solutions Manual 5.23:
SolMan = (exp(1-.5) - 1)/(exp(1) - 1); SolMan # [1] 0.3775407

# Ding 5.23:
Ding = (exp(-.5))/(1 - exp(-1)); Ding # [1] 0.9595174




#####################################################################
#####################################################################

log(0)

z = c(2,4,8,16,32,64,128,256)
MLE = vector()
MME = vector()
for (i in 1:10){
  MLE[i] = z[i]/((z[i]+2)*(z[i]+1)^2)
  MME[i] = 1/(3*z[i])
}
plot(1:10,MLE, ylim = c(0,.17))
points(1:10,MME, col = "red", cex = .7)




# Exercise 7.10 (length of cuckoos' eggs in hedge sparrows' nests):
rs = c(22,23.9,20.9,23.8,25,24,21.7,23.8,22.8,23.1,23.1,23.5,23,23); min(rs)
bHat = max(rs)
length(rs)
log(prod(rs))
aHat = 14/(14*log(25) - 43.9527); aHat
x = seq(18,28,length.out = 100)
plot(x,(aHat/(bHat^aHat))*x^(aHat-1))
# THIS DOESN'T MAKE SENSE. EGGS CAN'T HAVE A NON-HUMPED DISTRIBUTION THAT GOES OFF TO INFINITY ON ONE SIDE. 





#####################################################################
#####################################################################

# Summation algebra:

sum(1,2,3,4)^2 
X = c(1,2,3,4)
sum(X)^2
sum(X^2)
1*2 + 2*3 + 3*4 + 4
ij = combn(4,2)
class(ij)
prdcts = vector()
for (i in 1:ncol(ij)){
  prdcts[i] = ij[1,i]*ij[2,i]
}
sum(prdcts)

mathisawesome = function(yourvec){
  n = length(yourvec)
  sqrdpart = sum(yourvec^2)
  weirdpart = combn(n,2)
  prdcts = vector()
  for (i in 1:ncol(weirdpart)){
    prdcts[i] = weirdpart[1,i]*weirdpart[2,i]
  }
  finalweird = 2*sum(prdcts)
  sqr.of.sum.of.Xis  = sqrdpart + finalweird
  print(sqr.of.sum.of.Xis)
  print(sum(yourvec)^2)
}
mathisawesome(1:20)
mathisawesome(c(5,6,1,3))
trial = expand.grid(a = c(5,6,1,3), b = c(5,6,1,3))
class(trial)
dups = vector()
for (i in 1:nrow(trial)){
  if (trial[i,"a"] == trial[i,"b"]){
    dups[i] = i
  } else{print("clear")}
}
dups
trial2 = trial[is.na(dups),]
prdcts = apply(trial2,1,prod); prdcts
length(prdcts)
sum(prdcts) + sum(c(5,6,1,3)^2) # it worked



##############################

z = (110 - 100)/(15/sqrt(50))
z
pnorm(z, lower.tail = FALSE)

z = (110 - 100)/15
z
pnorm(z, lower.tail = FALSE)

6783.5/13567

qnorm(.005)

##############################

y = seq(0,10,length.out = 1000)
distn = exp((y^.5)*(1-y^.5))*((y^-.5) + 1 - 2*y^.5)
plot(y,distn, type = "l", lwd = 2, col = "blue")
abline(h = 0, col = "gray")
abline(v = 1, col = "red")
points(y,(2*y^.5)*exp(y^.5 - y), type = "l", col = "green")


theta = seq(0,10,length.out = 1000)
distn = 1 - .5^theta
plot(theta,distn, type = "l", lwd = 2, col = "blue")
abline(h = 0, v = 0, col = "gray")

x = seq(1.1,1.25,.01); x
y = 1 - .5^x
plot(x,y)


x = seq(1.1,4,.01); x
y = 1 - .5^x
plot(x,y)
abline(h = 1, col = "gray")


log(.01)
log(4)/(-3)
-3*log(2)
log(4)

curve(5*x - 6*x)




