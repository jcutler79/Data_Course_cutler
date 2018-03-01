### MASTER LIST OF ALL THE COOLEST R CODE I LEARN AND QUESTIONS I HAVE ###
# (Usually in alphabetical order)

# QUESTIONS:
## 1. What can you do about 2 or more data points in a scatter plot of x vs y 
# that are exactly the same? They look like a single data point--you can't 
# tell there's more than one data point there. ANSWER: USE ALPHA IN GGPLOT FOR TRANSPARENCY
## 2. Is there a way to color a single data point in a scatter plot? Say I wanted
# to highlight the US in the chol vs HDI scatter plot by coloring it red or something.
## 3. Is there a way to pick a dot from a scatter plot and figure out which item it 
# represents? For example, say I see an outlier on the scatter plot. How can I find 
# out which item in a data.frame it represents?
## 4. How can I add a balloon color column with the four colors matching the numbers?



## Boxplots - How to create a boxplot by converting column values (e.g. years) to as.factor
# Example from Biol-490R:
DNA_Conc_blah2$year.fac = as.factor(DNA_Conc_blah2$year)
plot(DNA_Conc_blah2$year.fac, DNA_Conc_blah2$Katy, xlab = "YEAR", 
     ylab = "DNA concentration", main = "Katy's Extractions")
# Boxplot with colors in boxes (example from Stats 4100):
battery$TYPEBAT.fac = as.factor(battery$TYPEBAT)
color.list = c("pink","yellow","orange","blue")
plot(battery$TYPEBAT.fac, battery$LPUC, xaxt="n", xlab = "battery type", 
     ylab = "life per unit cost", col = color.list)
axis(1, at = 1:4, labels = color.list)
# Another way to do a boxplot (CO2 data set example):
boxplot(df$uptake ~ df$Treatment*df$Type, col = c("Lightgrey","White"))


## cbind vs rbind - binding columns vs binding rows
sport = c("Hockey", "Baseball", "Football")
league = c("NHL", "MLB", "NFL")
trophy = c("Stanley", "Comissioner's", "Lombardi")
bind.columns1 = cbind(sport, league, trophy); bind.columns1 # It makes sense!
bind.rows1 = rbind(sport, league, trophy); bind.rows1 # This makes sense too!


## CO2 data set - money code I learned from the CO2_instructor R script:
# A money boxplot idea (df = CO2):
boxplot(df$uptake ~ df$Treatment*df$Type, col = c("Lightgrey","White")) # THIS IS MONEY!
# THE ULTIMATE GGPLOTS:
p1 = ggplot(df, aes(x=conc, y=log10(uptake), col=Treatment)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake")
p1 # THIS ONE IS GOOD

p2 = ggplot(df, aes(x=conc, y=log10(uptake), col=Type)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake")
p2 # AND THIS ON IS GOOD, BUT ...

p3 = ggplot(df, aes(x=conc, y=log10(uptake), col=Treatment)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake") +
  facet_grid(facets = ~ Type)
p3 # ... THIS ONE IS THE MONEY PLOT OF ALL PLOTS! YOU CAN SEE THE DIFFERENCE BETWEEN CHILLED
# AND NON-CHILLED, ***AND*** THE DIFFERENCE BETWEEN QUEBEC AND MISSISSIPPI!
# TO BE TOTALLY CLEAR, THIS IS NOT A JUXTOPOSITION OF THE TWO PREVIOUS PLOTS--THE PREVIOUS
# TX-COLOR-CODED PLOT SHOWED *ALL* PLANTS, IN MISS AND QUEBEC, MIXED TOGETHER, AND THEIR
# RESPONSE TO TX. THE PLACE-COLOR CODED PLOT DID THE SAME--IT SHOWED *ALL* PLANTS MIXED
# TOGETHER, TREATED AND NON-TREATED (CHILLED AND NON-CHILLED), AND THEIR RESPONSE BASED
# ON PLACE. BUT THIS SHOWS HOW QUEBEC PLANTS RESPONDED TO TX VS NO TX, RIGHT NEXT TO
# MISS PLANTS' RESPONSE TO TX VS NO TX.


## Columns - How to add a column (example)
df$Total_Points = rowSums(df[,3:17])


## Column names - How to change them
# An example from the cholesterol data:
colnames(M2) <- c("Country","m.t.chol","HDI","meat","milk","eggs","fish","an.fats")
names(M2)


## Cool code (miscellaneous):
# %in% # This does something cool


## Draw function plots:
curve((1/sqrt(2*pi))*exp((-x^2)/2), from = -4, to = 4,main = "the normal curve",xlab = "x",ylab = "y")
func.stand = function(x) ((1/sqrt(2*pi))*exp((-x^2)/2))
integrate(func.stand, lower = -5, upper = 2)
curve((3*x^3 - 4*x^2 + x -1)/((x^2 + 1)*(x^2 + 2)), from = -30, to = 10, n = 1000,
      xlab = "x", ylab = "y")
curve(x-x, from = -30, to = 10, add = TRUE)
# I can add the x axis but how do you add the y axis??? x=0 doesn't work
# or you could use plot.function() which works the same exact way


### expand.grid (example):
# Automatically create a data frame in a cool way (cyclic repeats of first vector):
dudes = expand.grid(height = seq(60,80,5), peso = seq(100,300,50), sex = c("male","female"))
# For every time the first sequence prints (60,65,70,75,80), the second sequence lines up a
# column of 100s. Then the first sequence repeats, this time lined up with five 150s. Then
# another cycle from 60 to 80 with 200s, etc. till it goes through the 300s. All of those
# are matched with a column of 25 males. Then 25 females (same thing in the first two columns).
## Another example:
de = expand.grid(x = 1:3, a = c("male","female","intersex")); de


### FOR LOOPS!!!!!!!!!! - the mysteries and wonders of for loops:
## GET THE FREAKING OUTPUT OF A FOR LOOP FOR CRYING OUT LOUD AND FREAKING PUT IT IN A DATA FRAME!
# rbind/cbind and for loops are a weird mix:
dube = NULL
for (i in 1:10){
  what = c(1,2,3)
  butt = c(3,2,1)
  dube = rbind(dube, data.frame(what,butt)) # RBIND! IT WORKS!
}
dube # FOR SOME ODD REASON IT WORKS!
######################################
rollin = NULL
for (i in 1:10){
  tut = c(4,5,6)
  king = c(1,2,3)
  rollin = cbind(rollin, data.frame(tut,king)) # CBIND DOESN'T WORK!
}
rollin # DOESN'T FREAKING WORK!!?!!?!?!? WHY!!?????? IT'S BACKWARDS! RBIND DOES WORK, AND 
# BINDS WHAT AND BUTT INTO COLUMNS!!! CBIND DOESN'T WORK, AND APPARENTLY DOESN'T BIND
# THEM INTO COLUMNS!
## a for loop for creating cyclicly repeating values:
myfunc = function(){
  for (i in 1:10){
    print(1)
    print(2)
    print(3)
  } 
}
## compound interest for loops:
beg = 0
for (i in 1:12){
  beg = (beg + 1000)*1.06
  print(beg)
} # This gets the right result for compounded interest on 1000 added each month
for (i in 1:12){
  beg = 1000
  ultimo = beg*1.06^i
  print(ultimo)
} # Gives you the monthly breakdown of how much it grows with no added money after the initial 1000
1000*1.06^12
## an example of a for loop that works and one that doesn't work from Stats 4100 classwork notepad:
# for loop that "works" in that it will give me a variable that I can use as an entire vector
# after (i.e. outside) the loop:
yep = 0
for (i in 1:5){
  yep[i] = (i-.5)/5
  print(yep)
}
yep
# But if I do it this way ...
pep = 0
for (i in 1:5){
  pep = (i - .5)/5
  print(pep)
}
pep # ... Then it just gives me the final value of .9 as pep. I have to index pep! That makes it 
# think of pep as a vector I guess. 
## for loops for sequences (plus plotting the results!):
prblm = 0
for (i in 1:10){
  prblm[i] = 3*i/(1 + 6*i)
  print(prblm)
}
prblm

  
## ggplot - How to do a ggplot graph that's pretty okay
# An example from the thatch ant colonies data (dat, copied as dat2):
ggplot(dat2, mapping = aes(x = Headwidth, y = Mass, col = Colony)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)


## Integrals - How to integrate definite integrals
func = function(x) (x^2 + 2*x + 3)
integrate(func, lower = 0, upper = 2)
# e.g.
func5 = function(y) (2*pi*y*sqrt(1 + (1+3*y^2)^2))
integrate(func5, lower = 0, upper = 1)
# e.g. that's not possible to integrate by hand (actually, it is! Ha I'm an idiot):
func1 = function(x) ((3*x^3 - 4*x^2 + x -1)/((x^2 + 1)*(x^2 + 2)))
integrate(func1, lower = -30, upper = 1)
# Probability example (IQ score):
func.IQ = function(x) ((1/(15*sqrt(2*pi)))*exp((-(x-100)^2)/(2*15^2)))
integrate(func.IQ, lower = 85, upper = 115)
## Probability standard normal:
func.stand = function(x) ((1/sqrt(2*pi))*exp((-x^2)/2))
integrate(func.stand, lower = -3, upper = 3)
# Integrate a simple exp(-x^2):
func.e = function(x) (exp(-x^2))
integrate(func.e, lower = -10, upper = 10) # The area under this curve is NOT going to be 1
# Neither will THIS curve have area 1:
func.e.norm = function(x) (1/sqrt(2*pi))*exp(-x^2)
integrate(func.e.norm, lower = -10, upper = 10)
# But the curve above ("Probability standard normal") does have area under the curve of 1


## Matrices - creating matrices
# Simple example:
n = 2
input = matrix(c(1,2,3,4,5,6), ncol = n, byrow = TRUE); input # fills up by row  
input2 = matrix(c(1,2,3,4,5,6), ncol = n, byrow = FALSE); input2 # fills up by column


## Messy_Data_Practice coolest things I learned:
# 1. Don't forget to load these:
library(ggplot2)
library(dplyr)
library(tidyr) # Needed for the gather() function
library(MASS)
####### BIGGEST LESSON OF ALL: HOW TO TURN SEPERATE COLUMNS INTO ONE COLUMN "KEY" WITH 
# VALUES IN THE COLUMN BESIDE THEM MATCHED TO THEIR "KEY":
df_long = gather(df, key = "Time", value = "Abs", c("Hr_24","Hr_48","Hr_144"))
# Then convert the characters to numerics with mapvalues (from the plyr library):
df_long$Time = as.numeric(plyr::mapvalues(df_long$Time, from =c("Hr_24","Hr_48","Hr_144"), 
                                          to = c(24,48,144)))
# How to plot shiz by substrate in that data set with 32 substrates:
# YAY THIS FOR LOOP FREAKING WORKS!!! ALL BECAUSE I ACTUALLY TOLD IT TO FREAKING PRINT!: 
for (i in levels(df_long$Substrate)){
  sub1 = subset(df_long, Substrate == i)
  stuff = ggplot(sub1, aes(x = Time, y = Abs, col = Sample.ID)) +
    geom_point() + stat_smooth() + ggtitle(i)
  print(stuff)
}
# How to do the same thing as the for loop with a function and lapply:
# This is money: 
substrates = levels(df_long$Substrate)
substrate.plot = function(x){
  ggplot(df_long[df_long$Substrate == x,], aes(x = Time, y = Abs, col = Sample.ID)) + geom_point() +
    stat_smooth() + ggtitle(x)
}
lapply(substrates,substrate.plot) # The vector or list you want to apply the function to
# goes first, then the function (already stored in R, or created and called previously by 
# you), goes next. Just like in tapply! 
###### How to create a smaller data frame with just a few substrates that I choose:
df_short = df_long[which(df_long$Substrate == c("L-Serine","L-Arginine","D-Xylose")),] # HOLY FREAK this did not work
df_short2 = subset(df_long, Substrate %in% c("L-Serine","L-Arginine","D-Xylose")) # This did work!
#################
### Fun practice cleaning messy data
# practice on messy data round 2
string = "[1] 27.171198 23.731919 29.864818 28.918605 29.203685 24.569740 21.135274 17.779827 18.640766
[10] 20.842806 22.688750 21.279813 21.602885 20.329239 18.975821 20.485232 21.794798 18.093943
[19] 20.639335 19.295169 15.833791 17.830053 17.198175 16.184027 17.150483 16.339271 17.237826
[28] 16.404370 18.845733 19.756243 15.610480 17.988624 13.863125 16.518413 14.984714 21.662258
[37] 21.435391 22.117069 22.295217 30.216472 31.349093 24.982721 23.807852 23.776066 22.460616
[46] 22.051260 21.761623 20.297256 16.842247 20.361394 21.330119 21.876627 24.855784 22.744847
[55] 21.203989 29.855509 24.512312 29.924539 23.298075 21.950098 19.719313 19.185531 22.727848
[64] 23.975597 25.362793 26.405900 23.685981 22.294277 20.989119 21.898868 23.695884 22.771961
[73] 23.495193 23.524038 23.460332 22.450714 21.276259 22.187105 21.788835 22.252185 25.419067
[82] 24.520290 23.911910 23.238712 23.526570 25.467206 22.277192 23.424130 29.739755 30.016261
[91] 25.095485 25.197988 23.511869 23.997796 22.208930 26.148983 23.871551 39.983126 39.600897
[100] 33.418247 21.268001 22.307637 13.952314 19.216281 19.425228 18.314375 17.963129 19.265477
[109] 19.986273 18.928054 20.794346 22.355789 19.427601 19.454625 20.982684 19.626901 20.682572
[118] 20.562882 19.788924 20.322903 20.235989 20.513943 19.671937 17.740723 20.125630 20.028902
[127] 17.944666 16.876181 18.163832 16.922282 18.643083 18.543961 18.192566 17.344555 16.989907
[136] 17.724298 17.186670 18.499010 16.694840 17.056001 16.403625 12.801367 16.400540 15.283628
[145] 15.823504 16.204264 16.258014 14.644187 14.190319 17.541871 19.839132 19.060228 16.689737
[154] 17.724689 15.152198 16.969975 15.766704 36.059641 23.662331 21.356402 25.723272 45.598335
[163] 49.783783 49.995430 21.384580 22.240985 48.174928 22.166654 22.613670 23.440089 22.662040
[172] 22.360407 22.168693 25.184956 23.590568 26.803648 23.663714 25.079286 27.497418 29.510602
[181] 36.291971 23.971733 30.404551 27.034628 22.674770 23.766406 37.246165 30.407309 30.168497
[190] 31.307578 29.873187 29.702214 31.441395 28.815254 28.018513 44.319129 33.940103 32.930025
[199] 32.510241 30.476902 32.424066 24.624014 39.293922 47.152630 47.218149 22.770310 22.464071
[208] 19.441218 22.507795 20.069271 21.782155 20.657277 23.161931 23.109826 22.039596 22.668694
[217] 23.022431 23.280389 22.297526 24.105336 26.833251 21.818741 26.599677 25.000162 42.112621
[226] 42.931964 41.274340 29.599418 37.446984 26.154816 22.225471 32.800356 43.296173 41.072378
[235] 26.195629 22.721049 27.209002 32.271660 25.677316 25.222915 24.647532 21.622084 22.394345
[244] 25.247255 19.266342 18.569053 22.421009 20.562312 22.628029 24.857199 24.621690 24.772196
[253] 25.485112 28.926178 23.469113 22.669894 39.270779 48.224949 35.504193 35.488313 35.398634
[262] 35.463805 46.558644 35.818465 34.951772 26.811669 34.272019 45.553964 40.173624 23.605338
[271] 22.103854 24.157951 24.794854 33.334437 33.194204 28.291188 33.851355 32.777617 26.428249
[280] 30.670991 36.735933 30.437271 33.383755 38.225409 32.434193 26.141422 23.328502 24.254630
[289] 24.150192 25.246769 32.403877 30.456501 31.641342 23.909530 23.137479 24.794177 24.321690
[298] 21.311077 26.271468 30.204380 28.488350 25.905564 26.074671 28.630756 29.569282 27.244850
[307] 31.421688 28.944644 24.149734 20.955563 21.288839 22.632504 20.427674 21.893576 22.453818
[316] 20.464484 19.345253 19.980383 22.107887 21.554237 22.858179 22.753132 22.015351 19.949928
[325] 23.053169 23.617929 23.031579 21.547465 21.924151 22.879031 21.998755 21.805618 23.476678
[334] 22.409096 22.408346 21.756851 21.188268 20.739252 21.867528 21.506507 21.123584 29.398086
[343] 23.504352 27.021837 28.206977 20.825314 20.076766 26.120688 26.562274 24.781754 24.998411
[352] 22.686700 22.973942 26.006356 20.570922 18.346767 21.778729 21.868208 22.408707 18.143108
[361] 19.358227 17.293553 19.677287 24.533987 49.899297 34.597348 18.542450 18.637538 24.295833
[370] 48.493676 49.468168 17.932071 26.881328  9.756635 10.499088 15.292546 12.863623 14.241937
[379] 10.221381 12.554189  5.000010 13.297125 12.739254 12.457239 10.520863 10.315969  9.150623
[388]  9.640189 10.232434 13.527139 15.613975 15.848970 11.377569 16.756673 15.496701 15.712822
[397] 15.558766 14.817707  8.318361 12.792395 10.858992 14.053735 14.663470 10.781293  5.216423
[406]  9.481403  9.982321 18.759874 13.113005 13.792371 10.888638 11.956713  9.611199 10.723529
[415] 11.088818  9.907133 11.498571 13.519317 14.993592 11.762825 16.131856 16.721084 18.911585
[424] 12.354687 12.934414 10.599380 13.100816 14.627721 12.771575 11.896061 13.836509 14.365093
[433] 13.991193 13.229979 12.913358 11.458402 12.257845  9.889063  9.136370 13.124695 10.682829
[442] 14.741395 15.959991 14.876534 13.823605 11.000173 15.876340 15.944599 15.316354 15.519333
[451] 12.995608 16.006247 16.455985 15.670785 12.125267 13.045086 13.706818 13.372452 16.506824
[460] 17.336254 15.913456 17.649423 17.766703 19.151431 19.563363 20.414633 12.941929 15.725379
[469] 17.605814 19.777039 18.789931 20.598720 20.083847 20.903150 16.634411 15.809089 16.644881
[478] 12.131099 16.081996 18.366976 21.392310 23.370500 23.855087 20.981945 21.033048 21.692045
[487] 19.616773 21.091389 13.578443 12.320564  7.799157 15.886784 14.632199 20.501554 21.005808
[496] 21.606437 18.547243 19.772189 20.202211 19.513632 19.761832 20.884394 19.354953 21.979156
[505] 21.828636 20.410901"
string = unlist(strsplit(string, " "))
length(string)
string # CREATING A VECTOR OUT OF IT ACTUALLY RUINED IT IN A VERY BIZARRE WAY. SO DON'T CONVERT IT TO VECTOR
strng.frm = data.frame(column1 = seq(1,520,1), column2 = string) # THIS WORKS. NO VECTOR NECESSARY.
strng.frm$column2 = as.character(strng.frm$column2)
class(strng.frm$column2)
strng.frm$column2 = gsub("\n", "", strng.frm$column2)
strng.frm = strng.frm[2:520,]
strng.frm$column2 = strtrim(strng.frm$column2,8)
strng.frm$column2 = as.numeric(strng.frm$column2)
length(strng.frm$column2)
strng.frm = na.omit(strng.frm) # DON'T FORGET TO GET RID OF THE EMPTY CELLS! I FORGOT AND CREATED NAs THIS TIME WITHOUT R EVEN TELLING ME.
length(strng.frm$column2)
strng.frm$column1 = seq(1,506,1)
sqrt(mean((mybos$medv - strng.frm$column2)^2))
# I DIDN'T NECESSARILY GO IN ORDER HERE. IN THE FUTURE, I WOULD GO:
# 1. Surround in quotes to make a string
# 2. unlist and stringsplit by spaces
# 3. Find length and create a data frame with another column being seq(1,nrows,1)
# 4. a) Remove first row with the [1]; 
# 4. b) remove the empty cells by first converting them to NAs
# 5. Convert to character
# 6. gsub out the '\n'
# 7. stringtrim down to managable length (might as well get rid of the unriddable '[' crap)
# 8. Convert to numeric
# 9. Renumber the first column
# 10. Done. 


## na.omit
# example from assignment_3:
which(dat$Headwidth == "")
bad = which(dat$Headwidth == 1)
dat$Headwidth[bad] = NA
# NOW, REMOVE ALL THE ROWS OF "dat" THAT HAVE AN "NA" VALUE
dat2 = na.omit(dat)


## Random number generator - How to generate random numbers within a range
# How to do decimals:
sprintf("%.2f",runif(1,min=.289,max=.91)) # fixes the number of decimal spaces to 2 only
# How to do integers:
?sprintf
?runif
runif(10,min=1,max=155) # This will NOT give you integers
?sample
sample(1:155, each=10)
sample(1:155,10) # THIS IS HOW YOU DO IT! SIMPLE!!! 10 RANDOM NUMBERS SELECTED BETWEEN 1 AND 155
# An example: 
trial1 = rnorm(100,mean = 23,sd = 10); trial1
trial1b = rnorm(50, mean = 27, sd = 10); trial1b
trial1c = c(trial1,trial1b); mean(trial1c)
trial1c
mean(c(mean(trial1),mean(trial1b)))
mean(trial1b)
mean(22.467+28.59)


## tapply - example from rpm data (Stats 4100):
# Two methods for doing the same thing--calculating the means of the txs (true response):
for (i in 1:5){
  stuff = mean(with(rpm, subset(liters.minute, level.fac == i)))
  print(stuff)
}
rpm.mean.vector = tapply(rpm$liters.minute, rpm$level, mean); rpm.mean.vector

#########







