# STATS 4000 1st case study (Cholesterol)

getwd()
setwd("/Users/jamescutler/Desktop/R_Data_Course")
setwd("/Users/jamescutler/Desktop/Stats_4000")
getwd()
M = read.csv("CholData.csv")
M
ones = rep(1,155)
ones
onesMtrx = matrix(ones,nrow=155)
NChol = M[,3:8]
XChol = cbind(onesMtrx,NChol)
YChol = M[,2]
X1 = M[,3]
X2 = M[,4]
X3 = M[,5]
X4 = M[,6]
X5 = M[,7]
X6 = M[,8]
betaChol = lm(formula = YChol ~ X1+X2+X3+X4+X5+X6)
betaChol
bCholHDI = lm(formula = YChol ~ X2)
bCholHDI
HDI = X1
meat = X2
milk = X3
egg = X4
fish = X5
animalFat = X6
cholesterol = YChol
# Various scatter plots for HDI and chol and the other factors:
plot(HDI,meat)
plot(HDI,milk)
plot(HDI,egg)
plot(HDI,fish)
plot(HDI,animalFat)
plot(HDI,cholesterol)
plot(meat,cholesterol)
plot(milk,cholesterol)
plot(egg,cholesterol)
plot(fish,cholesterol)
plot(animalFat,cholesterol)


# Program for feeding values into the cholesterol prediction model:
country = function(){
  nation = readline(prompt="Enter country name: ")
  HDIc = readline(prompt="Enter HDI: ")
  HDIc = as.numeric(HDIc)
  meatc = readline(prompt="Enter meat: ")
  meatc = as.numeric(meatc)
  milkc = readline(prompt="Enter milk: ")
  milkc = as.numeric(milkc)
  eggc = readline(prompt="Enter egg: ")
  eggc = as.numeric(eggc)
  seafoodc = readline(prompt="Enter seafood: ")
  seafoodc = as.numeric(seafoodc)
  animalc = readline(prompt="Enter animal fat: ")
  animalc = as.numeric(animalc)
  YCholc = 1.371004*HDIc+.00143*meatc+.000357*milkc+.010355*eggc+.001648*seafoodc+
    .007664*animalc+3.55969
  print(YCholc)
}

# Add a regression line to the graph of any two of the variables.
# For example, HDI explaining/predicting cholesterol:
ggplot(M, mapping = aes(x = M[,3], y = M[,2], col = "red")) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# I don't need to find countries with identical cholesterol values; I need identical HDI, and identical meat:
plot(HDI,cholesterol)
abline(v=c(.88,.92), col="blue")
plot(countries.88_.92_meat,countries.88_.92_chol)
plot(M2[(M2$HDI > .88)&(M2$HDI < .92),3],M2[(M2$HDI > .88)&(M2$HDI < .92),2])
length(countries.88_.92_chol)
M2[(M2$HDI > .88)&(M2$HDI < .92),1:3]
M2_0.88_0.92 = data.frame(M2[(M2$HDI > .88)&(M2$HDI < .92),c(1:3)])
sort(M2_0.88_0.92$m.t.chol)

# x[(x > 0) & (x < 1)]    gives you all x values between 0 and 1
M2 = M
names(M2)
colnames(M2) <- c("Country","m.t.chol","HDI","meat","milk","eggs","fish","an.fats")
names(M2)

M2[(M2$meat > 120)&(M2$meat < 140),1] # This code works! Don't make it more complicated than this!
M2[(M2$meat > 120)&(M2$meat < 140),1:2]
M2[(M2$meat > 55)&(M2$meat < 65),1:2]
data.frame(M2[(M2$meat > 55)&(M2$meat < 65),1:2])
M2.56.64 = data.frame(M2[(M2$meat >= 56)&(M2$meat <= 64),c(1,2,4)])
sort(M2.56.64$m.t.chol)
countries56_64_HDI = M2[(M2$meat >= 56)&(M2$meat <= 64),3]
countries56_64_HDI
countries56_64_chol = M2[(M2$meat >= 56)&(M2$meat <= 64),2]
plot(countries56_64_HDI,countries56_64_chol)
plot(M2[(M2$meat >= 56)&(M2$meat <= 64),4],countries56_64_chol)
plot(meat,cholesterol)
abline(v=c(56,64), col="blue")

### Now I need to fit a line to the data
## For the HDI (of similar meat countries) explaining cholesterol:
reg.HDI.o.s.meat = lm(countries56_64_chol ~ countries56_64_HDI)
plot(countries56_64_HDI,countries56_64_chol)
abline(reg.HDI.o.s.meat, col="blue")
cor(countries56_64_HDI, countries56_64_chol) # correlation coefficient = 0.85168

# For the meat "explaining" cholesterol in the same (similar meat) subset:
reg.meat.o.s.meat = lm(countries56_64_chol ~ M2[(M2$meat >= 56)&(M2$meat <= 64),4])
plot(M2[(M2$meat >= 56)&(M2$meat <= 64),4],countries56_64_chol)
abline(reg.meat.o.s.meat, col="blue")
cor(M2[(M2$meat >= 56)&(M2$meat <= 64),4],countries56_64_chol) # correlation coeff = -0.0673

# For the meat (of similar HDI countries) explaining cholesterol:
reg.meat.o.s.HDI = lm(countries.88_.92_chol ~ countries.88_.92_meat)
plot(countries.88_.92_meat,countries.88_.92_chol)
abline(reg.meat.o.s.HDI, col="blue")
cor(countries.88_.92_meat, countries.88_.92_chol) # correlation coeff = .3538

# For the HDI "explaining" cholesterol in the same (similar HDI) subset:
reg.HDI.o.s.HDI = lm(countries.88_.92_chol ~ M2[(M2$HDI > .88)&(M2$HDI < .92),3])
plot(M2[(M2$HDI > .88)&(M2$HDI < .92),3],M2[(M2$HDI > .88)&(M2$HDI < .92),2])
abline(reg.HDI.o.s.HDI, col="blue")
cor(M2[(M2$HDI > .88)&(M2$HDI < .92),3], countries.88_.92_chol) # correlation coeff = 0.0439

### Two more slice subsets for each (for meat and HDI)
## HDI slice between .75 and .79:
which(M2$HDI == .79)
which((M2$HDI > .75)&(M2$HDI < .79))
M2[(M2$HDI > .75)&(M2$HDI < .79),1:3]

M2_0.75_0.79 = data.frame(M2[(M2$HDI > .75)&(M2$HDI < .79),1:3])
sort(M2_0.75_0.79$m.t.chol)
sort(M2_0.75_0.79$HDI)
length(M2_0.75_0.79$Country) # 13 countries between .75 and .79 HDI

# For the HDI in the HDI slice "explaining" cholesterol:
reg.HDI.osHDI_0.75_0.79 = lm(M2[(M2$HDI > .75)&(M2$HDI < .79),2] ~ 
                               M2[(M2$HDI > .75)&(M2$HDI < .79),3])
plot(M2[(M2$HDI > .75)&(M2$HDI < .79),3],M2[(M2$HDI > .75)&(M2$HDI < .79),2])
abline(reg.HDI.osHDI_0.75_0.79, col="blue")
cor(M2[(M2$HDI > .75)&(M2$HDI < .79),3], M2[(M2$HDI > .75)&(M2$HDI < .79),2]) # correlation coeff = 0.1138

# For the meat in the HDI slice explaining cholesterol:
reg.meat.osHDI_0.75_0.79 = lm(M2[(M2$HDI > .75)&(M2$HDI < .79),2] ~
                                M2[(M2$HDI > .75)&(M2$HDI < .79),4])
plot(M2[(M2$HDI > .75)&(M2$HDI < .79),4], M2[(M2$HDI > .75)&(M2$HDI < .79),2])
abline(reg.meat.osHDI_0.75_0.79, col="blue")
cor(M2[(M2$HDI > .75)&(M2$HDI < .79),4], M2[(M2$HDI > .75)&(M2$HDI < .79),2]) # correlation coeff = 0.6863

plot(HDI,cholesterol)
abline(v=c(.75,.79), col="blue")


## Meat slice between 81 and 90:
plot(meat,cholesterol)
abline(v=c(81,90), col="blue")

M2[(M2$meat > 81)&(M2$meat < 90),c(1,2,4)]
M2.81.90 = data.frame(M2[(M2$meat > 81)&(M2$meat < 90),c(1,2,4)])
sort(M2.81.90$m.t.chol)
length(M2.81.90$Country) # 12 countries between 81 and 90 meat


# For the meat in the meat slice "explaining" cholesterol:
reg.meat.osmeat_81_90 = lm(M2[(M2$meat > 81)&(M2$meat < 90),2] ~ 
                             M2[(M2$meat > 81)&(M2$meat < 90),4])
plot(M2[(M2$meat > 81)&(M2$meat < 90),4], M2[(M2$meat > 81)&(M2$meat < 90),2])
abline(reg.meat.osmeat_81_90, col="blue")
cor(M2[(M2$meat > 81)&(M2$meat < 90),4], M2[(M2$meat > 81)&(M2$meat < 90),2]) # correlation coeff = -0.4757

# For the HDI in the meat slice EXPLAINING cholesterol:
reg.HDI.osmeat_81_90 = lm(M2[(M2$meat > 81)&(M2$meat < 90),2] ~ 
                            M2[(M2$meat > 81)&(M2$meat < 90),3])
plot(M2[(M2$meat > 81)&(M2$meat < 90),3], M2[(M2$meat > 81)&(M2$meat < 90),2])
abline(reg.HDI.osmeat_81_90, col="blue")
cor(M2[(M2$meat > 81)&(M2$meat < 90),3], M2[(M2$meat > 81)&(M2$meat < 90),2]) # correlation coeff = 0.9178


# Country lists for the first 4 (2 for HDI-adjusted, 2 for meat-adjusted) subsets:
M2_0.75_0.79$Country
M2_0.88_0.92$Country
M2.56.64$Country
M2.81.90$Country

# I want rows 104, 75, 106
M2[(M2$HDI > .6)&(M2$HDI < .63),1:3]
which(M2$HDI == .289)
which(M2$HDI == .95)
plot(M2[c(104,75,106),2:3])
M2[c(104,75,106),1:4]

### How to generate random slices of data and run analyses on them:
mylist1 = c(100,21,3,4,5,20,75)
mylist1[which.min(mylist1)]
mylist1[which.max(mylist1)] # Practice: to get max and min values from cholesterol interval

pi 
sprintf("%.2f", pi) # This is how you limit the number of decimal places

sort(M2$HDI)
HDI.bottom = as.numeric(sprintf("%.2f", runif(1, min=.289, max=.91))) # This code works!
HDI.top = HDI.bottom+.04; HDI.bottom; HDI.top

sort(M2$m.t.chol)
sort(M2$meat)

new.vector = c(which((M2$HDI > .75)&(M2$HDI < .79))); new.vector
M2[new.vector,2]

mytrial = function(){
  foobar = as.numeric(sprintf("%.2f",runif(1,min=5,max=1000)))
  return(foobar)
}

myfib = function(mytrial,foobar){
  mytrial
  fibseq = c(1,1)
  counter = 2
  repeat{
    fibseq = c(fibseq,fibseq[counter-1]+fibseq[counter])
    counter = counter+1
    if(fibseq[counter] > foobar){break}
  }
  print(foobar,fibseq)
}


myHDI.sampler.plotter = function(){
  HDI.low = as.numeric(sprintf("%.2f",runif(1,min=.289,max=.91)))
  HDI.high = HDI.low+.04
  rndm.HDI = M2[(M2$HDI > HDI.low)&(M2$HDI < HDI.high),3]
  rows.ofrndmHDI = c(which((M2$HDI > HDI.low)&(M2$HDI < HDI.high)))
  chol.of.rndmHDI = M2[rows.ofrndmHDI,2]
  if((chol.of.rndmHDI[which.max(chol.of.rndmHDI)]-chol.of.rndmHDI[which.min(chol.of.rndmHDI)] >=.5)&
     (length(rows.ofrndmHDI) >= 9)){
    plot(HDI,cholesterol)
    abline(v=c(HDI.low, HDI.high), col="red")
    reg.HDIof.rndmHDI = lm(chol.of.rndmHDI ~ rndm.HDI)
    plot(rndm.HDI,chol.of.rndmHDI)
    abline(reg.HDIof.rndmHDI, col="blue")
    r.ofHDI.predct.chol = cor(rndm.HDI, chol.of.rndmHDI)
    reg.meatof.rndmHDI = lm(chol.of.rndmHDI ~ M2[rows.ofrndmHDI,4])
    plot(M2[rows.ofrndmHDI,4], chol.of.rndmHDI)
    abline(reg.meatof.rndmHDI, col="blue")
    r.ofmeat.predct.chol = cor(M2[rows.ofrndmHDI,4], chol.of.rndmHDI)
    print.noquote(c("HDI interval:", HDI.low, "to", HDI.high))
    print.noquote(c("HDI corr. coeff.:", r.ofHDI.predct.chol))
    print.noquote(c("Meat corr. coeff.:", r.ofmeat.predct.chol))
    print.noquote(c("Cholesterol range size:",chol.of.rndmHDI[which.max(chol.of.rndmHDI)]-chol.of.rndmHDI[which.min(chol.of.rndmHDI)]))
    print.noquote(c("Cholesterol interval:",chol.of.rndmHDI[which.min(chol.of.rndmHDI)],"to",chol.of.rndmHDI[which.max(chol.of.rndmHDI)]))
    print.noquote(c("Country sample size:",length(rows.ofrndmHDI)))
    print(M2[rows.ofrndmHDI,1])
  } else {
    print("Cholesterol variation too narrow or sample size too small")
  }
}
myHDI.sampler.plotter()


### Other models (just using HDI to predict cholesterol, or just using meat, or using both)
ones = rep(1,155)
ones
onesMtrx = matrix(ones,nrow=155)
NChol = M[,3:8]
XChol = cbind(onesMtrx,NChol)
YChol = M[,2]
X1 = M[,3]
X2 = M[,4]
X3 = M[,5]
X4 = M[,6]
X5 = M[,7]
X6 = M[,8]
betaChol = lm(formula = YChol ~ X1+X2+X3+X4+X5+X6)
betaChol
bChol.meat = lm(formula = YChol ~ X2)
rm(bCholHDI)

bChol.meat
bChol.HDI = lm(YChol ~ X1)
bChol.HDI

bChol.HDI.meat = lm(YChol ~ X1+X2); bChol.HDI.meat

country.HDI = function(){
  nation = readline(prompt="Enter country name: ")
  HDI.fun = readline(prompt="Enter HDI: ")
  HDI.fun = as.numeric(HDI.fun)
  YChol.HDI = 2.148*HDI.fun+3.272
  print(YChol.HDI)
}

country.meat = function(){
  nation.m = readline(prompt="Enter country name: ")
  meat.fun = readline(prompt="Enter meat: ")
  meat.fun = as.numeric(meat.fun)
  YChol.m = .01069*meat.fun + 4.22408
  print(YChol.m)
}

country.HDI.meat = function(){
  nation.H.m = readline(prompt="Enter country name: ")
  HDI.both = readline(prompt="Enter HDI: ")
  HDI.both = as.numeric(HDI.both)
  meat.both = readline(prompt="Enter meat: ")
  meat.both = as.numeric(meat.both)
  YChol.both = 1.825315*HDI.both + .002212*meat.both + 3.38795
  print(YChol.both)
}

country.all.stuff = function(){
  nation.three = readline(prompt="Enter country name: ")
  HDI.three = readline(prompt="Enter HDI: ")
  HDI.three = as.numeric(HDI.three)
  meat.three = readline(prompt="Enter meat: ")
  meat.three = as.numeric(meat.three)
  milk.3 = readline(prompt="Enter milk: ")
  milk.3 = as.numeric(milk.3)
  egg.3 = readline(prompt="Enter egg: ")
  egg.3 = as.numeric(egg.3)
  seafood.3 = readline(prompt="Enter seafood: ")
  seafood.3 = as.numeric(seafood.3)
  animal.3 = readline(prompt="Enter animal fat: ")
  animal.3 = as.numeric(animal.3)
  YChol.HDI = 2.148*HDI.three+3.272
  YChol.m = .01069*meat.three + 4.22408
  YChol.both = 1.825315*HDI.three + .002212*meat.three + 3.38795
  YChol.all = 1.371004*HDI.three+.00143*meat.three+.000357*milk.3+.010355*egg.3+.001648*seafood.3+
    .007664*animal.3+3.55969
  print.noquote(c("HDI-predicted:",YChol.HDI))
  print.noquote(c("meat-predicted:",YChol.m))
  print.noquote(c("both-predicted:",YChol.both))
  print.noquote(c("all-predicted:",YChol.all))
}

### Random country selector:
nation.selector = function(){
  country.numbers = sample(1:155,10)
  country.names = M2[country.numbers,1]
  print(country.names)
  print(country.numbers)
}


### This program probably won't end up panning out. But I want to quantify accuracy of the
### models without having to use excel.
proximity.tallier = function(){
  trueNum = readline(prompt = "Enter the true cholesterol value: ")
  trueNum = as.numeric(trueNum)
  estimarHDI = readline(prompt = "Enter HDI-predicted chol: ")
  estimarHDI = as.numeric(estimarHDI)
  estimarMeat = readline(prompt = "Enter meat-predicted chol: ")
  estimarMeat = as.numeric(estimarMeat)
  estimarBoth = readline(prompt = "Enter both-predicted chol: ")
  estimarBoth = as.numeric(estimarBoth)
  estimarAll = readline(prompt = "Enter all-predicted chol: ")
  estimarAll = as.numeric(estimarAll)
  HDI.proximity = abs(trueNum-estimarHDI)
  meat.proximity = abs(trueNum-estimarMeat)
  both.proximity = abs(trueNum-estimarBoth)
  all.proximity = abs(trueNum-estimarAll)
  
}




estimator = c("HDI","Meat","Both","All")
accuracy = c(.37,.29,.34,.32)
pnts = c(1,4,2,3)
table.it = data.frame(estimator,accuracy,pnts); table.it
rm(table.it)

