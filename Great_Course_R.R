### Great_Course_R

library(ggplot2)
temp = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/EPICA_temp.csv")
ggplot() + geom_line(data = temp, mapping = aes(x = Age, y = dT))
temp2 = temp[1:3785,]
ggplot() + geom_line(data = temp2, mapping = aes(x = Age, y = dT)) + geom_vline(xintercept = 125000)
length(which(temp$Age < 100000))
temp[2493,4]
temp3 = temp[1:2493,]
ggplot() + geom_line(data = temp3, mapping = aes(x = Age, y = dT)) + 
  scale_x_continuous(breaks=seq(0,100000,10000))
length(which(temp$Age < 20000))
temp4 = temp[1:904,] # 20,000 years
plot(temp4$Age, temp4$dT, cex = .1)
ggplot() + geom_line(data = temp4, mapping = aes(x = Age, y = dT)) + 
  scale_x_continuous(breaks=seq(0,20000,1000)) + 
  theme(axis.text.x = element_text(angle = 45)) + labs(title = "temp4")

# GRAPH OF HOW BIG THE TIME INTERVALS ARE BETWEEN ADJACENT CELLS IN THE AGE COLUMN:
jumps = numeric(0)
for (i in 1:nrow(temp)){
  jumps[i] = temp[i+1,4] - temp[i,4]
}
length(jumps)
jdf = data.frame(col1 = 1:5788, col2 = jumps)
ggplot() + geom_line(data = jdf, mapping = aes(x = col1, y = col2)) + 
  scale_x_continuous(breaks = seq(0,6000,500))

# GRAPH OF HOW BIG THE TIME INTERVALS ARE BETWEEN ADJACENT CELLS IN THE AGE COLUMN OF TEMP4:
nrow(temp4)
j4 = numeric(0)
for (i in 1:nrow(temp4)){
  j4[i] = temp4[i+1,4] - temp[i,4]
}
length(j4)
j4df = data.frame(col1 = 1:904, col2 = j4)
ggplot() + geom_line(data = j4df, mapping = aes(x = col1, y = col2))

temp5 = temp[1:length(which(temp$Age < 3000)),]
reg.t5 = lm(dT ~ Age, data = temp5); reg.t5
plot(temp5$Age, temp5$dT, cex = .2)
abline(reg.t5) # FITTING A LINEAR REGRESSION LINE TO THE LAST 3000 YEARS OF TEMPERATURE DATA, IT'S BASICALLY STRAIGHT.
abline(h = 0, col = "red")
ggplot() + geom_line(data = temp5, mapping = aes(x = Age, y = dT)) + 
  scale_x_continuous(breaks = seq(0,3000,100)) + 
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "temp5 (201 rows)")

nrow(temp5)
j5 = numeric(0)
for (i in 1:nrow(temp5)){
  j5[i] = temp5[i+1,4] - temp5[i,4]
}
length(j5)
j5df = data.frame(col1 = 1:201, col2 = j5)
ggplot() + geom_line(data = j5df, mapping = aes(x = col1, y = col2))

library(forecast)
library(tseries)
temp5$ma_5 = ma(temp5$dT, order = 5)
temp5$ma_10 = ma(temp5$dT, order = 10)
t1 = ggplot() + 
  geom_line(data = temp5, aes(Age, dT, colour = "actual data")) + 
  geom_line(data = temp5, aes(Age, ma_5, colour = "5-data-point m. a.")) + 
  geom_line(data = temp5, aes(Age, ma_10, colour = "10-data-point m. a.")) + 
  ylab("dT") 
t1
t1 + theme_bw() # WORKS
t1 + theme_economist_white() # ALSO WORKS. JUST MAKE SURE YOU LOAD ggthemes LIBRARY OR SOMETHING LIKE THAT

# JUST THE 5- AND 10-POINT AVERAGES:
ggplot() + 
  geom_line(data = temp5, aes(Age, ma_5, colour = "5-data-point m. a.")) + 
  geom_line(data = temp5, aes(Age, ma_10, colour = "10-data-point m. a.")) + 
  ylab("dT")
# JUST THE 10-POINT AVERAGE:
ggplot() + 
  geom_line(data = temp5, aes(Age, ma_10, colour = "10-data-point m. a.")) + 
  ylab("dT")
# A 20-POINT AVERAGE:
temp5$ma_20 = ma(temp5$dT, order = 20)
temp5 = temp5[,-8]
ggplot() + 
  geom_line(data = temp5, aes(Age, ma_20, colour = "20-data-point m. a.")) + 
  ylab("dT") + ylim(c(-1,.6))


# HOW MUCH MONEY JOHN AND MARYANNE LOST ON THEIR TOWNHOME 2013-2018:
m.homes = seq(208,327,length.out = 60)
plot(m.homes)
208/327
327/208
2*60+208
1800/1.572115
seq(1100,1800,length.out = 60)
300*12+475*12+650*12+825*12+700*12 # they lost about 35000 bucks
home = data.frame(years = 1:60, rent = seq(1100,1800,length.out = 60))
plot(home$years,home$rent)
mod1 = lm(home$rent ~ home$years); mod1
curve(11.86*x + 1088.14, from = 1, to = 60, ylim = c(500,2000))
abline(h = 800)
fun1 = function(x) 11.86*x + 1088.14
fun2 = function(x) x-x + 800
integrate(fun1, lower = 1, upper = 60)
integrate(fun2, lower = 1, upper = 60)
85542.33 - 47200 # didn't increase rent above 800 a month, so it's high: 38000 bucks lost

327/1.57
1800/1.57



### APPLICATIONS OF STATISTICS IN R, LECETURE 10 - CONFIDENCE INTERVALS:
std.nrm = function(x) (1/sqrt(2*pi))*exp((-x^2)/2)
integrate(std.nrm, lower = -1.96, upper = 1.96)

80+1.96*(2/sqrt(31))
80-1.96*(2/sqrt(31))
qnorm(.05, lower.tail = FALSE)
qnorm(.025)
qnorm(.005, lower.tail = FALSE)


set.seed(343)
milk = 129-rexp(10000,.95) # NEVER SEEN THIS BEFORE. VERY INTERESTING. GOOD WAY TO GET AN UPPER LIMIT WITH RANDOM DEVIATIONS DOWNWARD FROM IT.
class(milk)
# plot(milk)
hist(milk, main = "Histogram of population of milk jugs", col = "red")
# THIS IS WORTH REMEMBERING: hist(milk, main = "Histogram of population of milk jugs", col = "red", breaks = 100, xlim = c(100,130), ylim = c(0,60000), xaxp = c(30,130,20))
true_mean = mean(milk); true_mean
true_sd = sd(milk); true_sd

set.seed(343)
n = 50
s_milk = sample(milk, size = n, replace = TRUE)
s_mean = mean(s_milk); s_mean
s_mean - true_mean
s_mean + 1.96*sd(s_milk)/sqrt(n)
s_mean - 1.96*sd(s_milk)/sqrt(n)
sort(c(127.7155,128.2541,127.943), decreasing = FALSE)
boxplot(milk, s_milk, main = "population vs sample of 50")
hist(s_milk, col = "red")

milk_mean = numeric(0)
for (i in 1:1000){
  milk_mean[i] = mean(sample(milk, 50, replace = TRUE))
}
hist(milk_mean, breaks = 20, main = "a thousand n=50 sample means")
qqnorm(milk_mean)
qqline(milk_mean)

###### AN EXAMPLE OF AN ALTERNATIVE FIRST STEP IN THE PROCESS OF GETTING 20 CONFIDENCE INTERVALS?:
r.s.of.20.of.those.milk.means = numeric(0)
for (i in 1:20){
  r.s.of.20.of.those.milk.means[i] = sample(milk_mean, 20, replace = TRUE) # THIS ISN'T WRONG, IS IT?
}
r.s.of.20.of.those.milk.means
######

twnty.s = numeric(0)
for (i in 1:20){
  twnty.s[i] = mean(sample(milk,50,replace = TRUE))
}
hist(twnty.s)
lines(density(twnty.s), lwd = 3, col = "red")

hndrd.s = numeric(0)
for (i in 1:100){
  hndrd.s[i] = mean(sample(milk,50,replace = TRUE))
}
hist(hndrd.s, freq = FALSE)
lines(density(hndrd.s), lwd = 3, col = "red")

m = 20; n = 50
my.sd = sd(milk)
SE = my.sd/sqrt(n)
alpha = .1
zcrit = qnorm(1-alpha/2)
matplot(rbind(hndrd.s[1:20] - zcrit*SE, hndrd.s[1:20] + zcrit*SE), rbind(1:m,1:m), 
        type = "l", lty = 1, lwd = 2, col = "darkgray", xlab = "Ounces", ylab = "CI's",
        main = "20 different 90% CI's")
abline(v = mean(milk), col = "red", lwd = 3)



############# Unuseful help from the internet on using matplot:
x2 = 1:10
y2 = c(46,46,48.4,49.9,56,57,57,52,54,48.9)
z = lm(y~x)
xx = data.frame(col1 = seq(1,10,.2))
cl = predict(z, newdata = xx$col1, interval = "confidence", level = .9)
cl = as.data.frame(cl)
matplot(xx,cl,lty = c(1,2,2), type = "l", col = c(1,2,2), ylab = "predicted y")
points(xx,y,pch = 22, bg = "white")
# legend("bottomright", legend = c("Data", "Fit", "90% Confidence limit"), pch = c(22,-1,-1), lty = c(-1,1,2), col = c(1,1,2), bg = "white")

xx
length(xx$col1)
length(cl)

rbind(1:m,1:m)
1:10
length(seq(1,10,.2))
#############

# The predict function:
rad = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/radiation_survival.csv")
six = tapply(rad$surv,rad$dose,mean)
rad2 = data.frame(dosage = c(117.5,235,470,705,940,1410), survival = six)
plot(rad2$dosage,rad2$survival)
nlmodel = nls(survival ~ a*dosage^b, data = rad2, start = list(a = 1, b = 1))
nlmodel
p = coef(nlmodel)
curve(p["a"]*x^p["b"], lwd = 2, col = "red", add = TRUE)
p["a"]*705^p["b"]
newbie = data.frame(col1 = seq(150,1000,length.out = 10))
rad.preds = predict(nlmodel, newbie) # IT WORKS
plot(rad.preds) # it works


################################################################
# The rexp function:
the.rexp.thing = data.frame(rates = c(1,.8,.6,.4,.2,.1), 
                            the.means = c(.88,1.17,1.69,2.63,5,10.85),
                            the.sds = c(.75,1.35,1.4,2.32,4.16,8.64))
ggplot() + labs(title = "means (red) and stdrd deviations (green) by rate") +
  geom_point(data = the.rexp.thing, mapping = aes(rates,the.means), col = "blue") + 
  geom_point(data = the.rexp.thing, mapping = aes(rates,the.sds), col = "green") + 
  geom_point(alpha = .5) # TRANSPARENCY DOESN'T WORK
# IF YOU WANT THE TRANSPARENCY FUNCTION TO WORK, YOU HAVE TO TYPE IT IN THIS WAY:
ggplot(the.rexp.thing, aes(rates,the.means)) + geom_point(col = "blue", size = 4) +
  geom_point(aes(rates,the.sds), col = "green", size = 4) + geom_point(alpha = .5)

a = .5
b = 50
x = rexp(b,a); x
mean(x)
sd(x)
var(x)
dfr = data.frame(x = x,nums = 1:100, mean.line = mean(x))
sqrt(mean((dfr$mean.line - dfr$x)^2))
ggplot(dfr, aes(nums,x)) + geom_point() + labs(title = sprintf("rexp(%s,%s)",b,a)) + 
  geom_line(data = dfr, mapping = aes(y = mean.line)) + 
  coord_cartesian(ylim = c(-.2,10))



##########################################################################################

### GREAT COURSE LECTURE 11: HYPOTHESIS TESTING

## Example: Cholesterol, where mean cholesterol level is 200 mg/dL, and the sample mean
# of the treatment group is 182.

ch = function(x) (1/(15*sqrt(2*pi)))*exp((-(x-200)^2)/(2*15^2))
integrate(ch, lower = -100, upper = 182)
integrate(ch, lower = 185, upper = 215)
integrate(ch, lower = -100, upper = 245)

z = (182-200)/(15/sqrt(40))
z
zfun = function(x) (1/sqrt(2*pi))*exp((-x^2)/2)
integrate(zfun, lower = -100, upper = -7.589) # 1.64e-14 with some error
pnorm(-7.589, lower.tail = TRUE) # 1.6123-14; THESE ARE SLIGHTLY DIFFERENT VALUES! INTERESTING ...

f = rnorm(100000)
m = rnorm(100000,mean = .001)
hist(f)
hist(m)
plot(density(f))
plot(density(m))  
which(f > 3)
which(m > 3)
which(f > 4)
which(m > 4)
mean(f)
mean(m)
m = rnorm(100000,mean = .1)
which(m > 4)
mean(m)
m = rnorm(100000, mean = .2)
plot(density(m))
mean(m)
which(m > 4)

# sample 1: f has 2 over 160, and m has 6
f = rnorm(100000,mean = 100,sd = 15)
m = rnorm(100000, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 2: f has 2; m has 6
f = rnorm(100001,mean = 100,sd = 15)
m = rnorm(100001, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 3: f has 2; m has 1
f = rnorm(100002,mean = 100,sd = 15)
m = rnorm(100002, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 4: f has 0; m has 6
f = rnorm(100003,mean = 100,sd = 15)
m = rnorm(100003, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 5: f has 0; m has 6
f = rnorm(100003,mean = 100,sd = 15)
m = rnorm(100003, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 6: f has 2; m has 5
f = rnorm(100004,mean = 100,sd = 15)
m = rnorm(100004, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 7: f has 0; m has 3
f = rnorm(100005,mean = 100,sd = 15)
m = rnorm(100005, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 8: f has 1; m has 3
f = rnorm(100000,mean = 100,sd = 15)
m = rnorm(100000, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 9: f has 2; m has 3
f = rnorm(100000,mean = 100,sd = 15)
m = rnorm(100000, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
# sample 10: f has 3; m has 3
f = rnorm(100000,mean = 100,sd = 15)
m = rnorm(100000, mean = 101, sd = 15)
which(f > 160)
which(m > 160)
males = c(6,6,1,6,6,5,3,3,3,3)
females = c(2,2,2,0,0,2,0,1,2,3)
plot(females, ylim = c(-.5,10))
points(males, col = "red", cex = 1.5)
sum(males)
sum(females)
14/42
42*320
13*320


##########################################################################################

### LECTURE 12: PAIRED T
## Things I learned (see below): 
# 1. parallel plot for horsebean and meatmeal chicken feeds - any better than a ggplot or a plot? 
# 2. qqplot and abline stuff for two dependent samples
# 3. McNeil plot for paired data
# 4. qqplot and line for differences between extension and flexion in IceSkating, by subraction
# 5. Paired t test by hand and by R

data("chickwts")
summary(chickwts)

tapply(chickwts$weight, chickwts$feed, mean)

qqnorm(chickwts$weight)
qqline(chickwts$weight, col = "red")

t.test(chickwts[37:48,1], chickwts[60:71,1])
t.casein = chickwts[60:71,1]
t.sunf = chickwts[37:48,1]
sd(t.casein)
sd(t.sunf)
t.test(t.casein,t.sunf)

feed.fac = as.factor(chickwts$feed)
plot(chickwts$weight ~ feed.fac, xlab = "feeds", ylab = "weight", las = 2)
mod1 = aov(chickwts$weight ~ feed.fac)
anova(mod1)
summary(mod1)
TukeyHSD(mod1,conf.level = .95)

# Parallel plot - any better than a simple plot?
hb.and.mm = chickwts[c(which(chickwts$feed == "horsebean"),which(chickwts$feed == "meatmeal")),]
hbmm.fac = as.factor(hb.and.mm$feed)
ggplot(hb.and.mm, aes(hbmm.fac,hb.and.mm$weight)) + geom_point()

library(PairedData)
data("IceSkating")
qqplot(IceSkating$Extension, IceSkating$Flexion, xlim = c(1.5,2.5), ylim = c(1.5,2.5)) # THIS IS ACTUALLY SOMETHING I'VE NEVER DONE BEFORE THIS WAY
abline(a=0,b=1,lwd = 2, col = "red") # HOW THE HECK DOES THIS MAKE SENSE?
attach(IceSkating) # I ONLY DID THIS BECAUSE WITHOUT ATTACHING IT, I WROTE THE LINE BELOW WITH DOLLAR SIGNS BEFORE EXTENSION AND FLEXION, AND IT SAID "UNDEFINED COLUMNS SELECTED"
with(IceSkating, plot(paired(Extension,Flexion), type = "McNeil")) # ONLY WORKS VIA THE ATTACHMENT ROUTE! WTF!?
hist(Extension - Flexion)
with(IceSkating,qqnorm(Extension-Flexion))
with(IceSkating,qqline(Extension-Flexion))
qt(.05/2,6) # or you could do abs(qt(.05/2,6))
qt(.01/2,6) # or you could do abs()
?t.test
t.test(paired(Extension,Flexion))
IceSkating$d = Extension-Flexion
d = IceSkating$d
ice.SE = sd(d)/sqrt(nrow(IceSkating)); ice.SE
mean(d)/ice.SE
2*(1-pt(2.934676,6))


##########################################################################################

### Lecture 13: Linear Regression Models and Assumptions
## Things I learned:
# 1. The 3 deadly statistical sins:
#  A Failure to randomize
#  B "Accept"-ing the alternative hypothesis
#  C Extrapolating - You can interpolate within your data range, but DON'T extrapolate outside of that range
# 2. The term for the fanning out of the residuals is "heteroscedasticity". If the residual plot looks
# good, then it is characterized by "homoscedasticity." 
# 3. How do we know when to use the qqplot function vs the qqnorm function, both followed by qqline, or qqplot followed by abline?
#  examples from below and the lecture above:
#  qqplot(IceSkating$Extension, IceSkating$Flexion, xlim = c(1.5,2.5), ylim = c(1.5,2.5)); abline(a=0,b=1,lwd = 2, col = "red")
#  vs. qqnorm(residuals(reg.wheat)); qqline(residuals(reg.wheat), lwd = 2, col = "red")
# 4. Our linear regression assumptions:
#  A. Normality - verified with residual plots and if need be, qqplots
#  B. Constant variability - verified with residual plots
#  C. Independence - to verify this, it's a little harder. We have to examine how our data values were
# collected to make sure our study doesn't involve any type of dependency between our data. It's not always
# possible to pull that out of graph.
#  D. Linearity - a simple scatter plot of our data can tell us whether we can fit a linear model to it? Wouldn't
# a residual plot also help with this?
# 5. Our linear model pitfalls:
#  A. Fitting a linear model to non-linear data. 
#  B. Fitting a linear model to data with non-constant variance.
#  C. Fitting a linear model to data with non-normal error. HOW CAN I CREATE SCATTER PLOTS IN R THAT LOOK 
# LIKE THE ONES SHE SHOWS IN THIS LECTURE? THE QQPLOT MAKES THE NON-NORMALITY OF THE ERROR MOST APPARENT.

# Rainfall and wheat growth

rfall = c(3.07,3.55,3.9,4.38,4.79,5.3,5.42,5.99,6.45,6.77)
wheat = c(78,82,85,91,92,96,97,104,111,119)
summary(cbind(rfall,wheat))
crops = as.data.frame(cbind(rfall,wheat))
plot(crops$rfall,crops$wheat)
attach(crops)
reg.wheat = lm(wheat ~ rfall)
abline(reg.wheat)
cor(rfall,wheat) # .98
Rsqrd = (cor(rfall,wheat))^2; Rsqrd # .96
summary(reg.wheat)

# WHAT IS GOING ON THAT IS DIFFERENT IN THESE TWO PLOTS???
plot(fitted(reg.wheat),resid(reg.wheat))
plot(resid(reg.wheat))
# IN ADDITION TO DOING A RESIDUAL PLOT, DO A QQ PLOT, ESPCIALLY WHEN YOU HAVE SO FEW DATA POINTS AND A RESIDUAL PLOT ALONE IS HARD TO TELL:
qqnorm(residuals(reg.wheat)); qqline(residuals(reg.wheat), lwd = 2, col = "red")


##########################################################################################

### Lecture 14: Regression predictions, confidence intervals

# mtcars example! - predicting gas mileage from vehicle weight
data("mtcars")
summary(mtcars)

plot(mtcars$wt,mtcars$mpg)
reg.wt = lm(mpg ~ wt, data = mtcars)
summary(reg.wt)
qt(.975,df = 30)
my.t = qt(.975,df = 28) # WHY DIDN'T THEY USE DF = 30???? n - 2
u.bound = -5.3445 + my.t*.5591
l.bound = -5.3445 - my.t*.5591
u.bound; l.bound # the true slope is between -6.5 and -4.2
t.sttstc = -5.3445/.5591; t.sttstc # -9.559
pt(-9.559,df = 28)
qqplot(fitted(reg.wt),resid(reg.wt))
qqnorm(residuals(reg.wt))
qqline(residuals(reg.wt))
shapiro.test(residuals(reg.wt)) # ALWAYS DO A SHAPIRO-WILKS NORMALITY TEST IF YOU WANT TO QUANTIFY HOW NORMAL YOUR RESIDUALS ARE!


##########################################################################################

### Lecture 15 - MLR







