# NOTE: ALL THOSE RIDICULOUS PUBLICATION-WORTHY TABLES PACKAGES ARE AT THE BOTTOM


# Coolest stuff I've been able to see/learn with R:
## (chronological order)

# Table of Contents:
## 19 Nov 2018 - Visualize the distribution of max values from a 100,000 normal distributions
## 07 Feb 2019 - confint() - not that exciting, but it ...
## 14 May 2019 - How to create a vector of objects/variables with paste and a loop




# 19 Nov 2018 - The distribution of max values from a 100,000 normal distributions:
reps = replicate(1e5,max(rnorm(100)))
hist(reps, breaks = 100) # INSANELY SIMPLE! BUT SOOOOO FREAKING FASCINATING! 
# WHY IS IT SLIGHTLY SKEWED RIGHT??




# 07 Feb 2019 - confint() 
## It's the function that gives you confidence intervals for coefficients of the 
## predictor variables in linear models. Not super exciting, but I realized I needed
## a function like this when I saw that SAS automatically provides this output in
## summaries of a linear model.




# 14 May 2019 - How to create a vector of objects/variables with paste and a loop
DC = readLines("/Users/jamescutler/Downloads/dc.txt")
DC2 = Corpus(VectorSource(DC))
dcp = paste(DC2,sep = " ") # DOES THIS STEP DO ANYTHING?
dcp2 = dcp[1]
s10 = gregexpr("because you delivered up those writings",dcp2)[[1]][1]
s20 = gregexpr("The rise of the Church of Christ",dcp2)[[1]][1]
s30 = gregexpr("you have feared man and have not relied on me",dcp2)[[1]][1]
s40 = gregexpr("the heart of my servant James",dcp2)[[1]][1]
s50 = gregexpr("according as ye have asked and are agreed as touching the church",dcp2)[[1]][1]
s60 = gregexpr("return speedily to the land from whence they came",dcp2)[[1]][1]
s70 = gregexpr("and also unto my servant Sidney Rigdon",dcp2)[[1]][1]
s80 = gregexpr("that cometh under the sound of your voice",dcp2)[[1]][1]
s90 = gregexpr("according to thy petition",dcp2)[[1]][1]
s100= gregexpr("my friends Sidney and Joseph",dcp2)[[1]][1]
s110= gregexpr("The veil was taken from our minds",dcp2)[[1]][1]
s120= gregexpr("it shall be disposed of by a council",dcp2)[[1]][1]
s130= gregexpr("shall appear we shall see him as he is",dcp2)[[1]][1]

mytext = paste0("s",seq(10,130,10))
mytext
sections = vector()
for (i in 1:length(mytext)){
  sections[i] = eval(parse(text = mytext[i]))
}
sections

#####################################################################################
#####################################################################################
#####################################################################################

## R datasets website:
# https://vincentarelbundock.github.io/Rdatasets/datasets.html


# libraries:

## INSTEAD OF INSTALLING FORCATS, GGPLOT2, TIDYR, DPLYR, STRINGR, ETC. SEPARATELY,
# JUST INSTALL "tidyverse":
# install.packages("tidyverse")
library(tidyverse)

## Packages needing introduction:
# install.packages("ggplot2") - To get ggplot2 version 3.0.0
# install.packages("scatterplot3d")

library(ggplot2)
library(tidyr) # wide to long and vice versa
library(dplyr) # for the %>% and filter and select ...
library(scales) # for the percent function in geom_text and scale_y_continuous
library(ggpubr) # before and after plots with lines connecting
library(scatterplot3d)

## SOMETHING I TRIED TO START WORKING ON:
# data("Titanic")
# summary(Titanic)
# as.data.frame.table(Titanic)
# # THIS IS NOT THE DATASET I'M LOOKING FOR. I DON'T EVEN KNOW IF IT HAS 2200 OBSERVATIONS
# 
# # THIS LOOKS LIKE A BETTER ONE:
# ti = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Titanic.csv")
# head(ti,5)
# ggplot(ti, aes(passengerClass,age, fill = survived)) +
#   geom_split_violin() +
#   facet_grid(~sex)






                          ##########################################
                          ########## DATA VISUALIZATION ############
                          ##########################################


# Alphabetical Table of Contents:

## 3D scatterplot - easy
## Before and After plots
## Combine two barplots of related topics - top 10 mortality example from Overview of PH
## Compound (stacked) barplots - SEE STACKED BARPLOTS BELOW
## Factor and levels & labels to organize bars in a bar chart however you want!! SUPER EXCITING!
### Cancer epi example
### Don't forget the BRFSS example from SAS_class
## Grouped barplots
## Interaction plot in ggplot using stat_summary (without qplot) - poison/tx DV example
## Labels - add x-axis labels to tick marks in base R plot 
### William example
### Austin TX creative jobs earnings example
## Lsmeans plots (easy) - poison/tx DV example
## Overlapping barplot - tobacco states example from Applied Bayesian
## Percentages or count labels above bars in barplot
## Stacked (compound) barplots in barplot() and in ggplot2 (includes cool table margins!)
## Translucent histograms in base R hist()
## Unemployment Vox graphic - example of easy publishable custom graphics 





### 3D scatterplot - easy
# website: https://statmethods.wordpress.com/
# library(scatterplot3d)
with(mtcars, {
  scatterplot3d(disp, wt, mpg,             # x,y,z
                color = "blue", pch = 19,
                type = "h",                # Very helpful lines from dots to the horizontal plane
                main = "3D scatterplot of mileage, weight, and displacement of cars",
                xlab = "Displacement (cubic inches)",
                ylab = "Weight (lb/1000)",
                zlab = "MPG")
})
# Add colors to show a fourth variable:
mtCols = c("green","darkgreen","blue")
mtcars$cyColor[mtcars$cyl==4] = mtCols[1]
mtcars$cyColor[mtcars$cyl==6] = mtCols[2]
mtcars$cyColor[mtcars$cyl==8] = mtCols[3]
with(mtcars, {
  scatterplot3d(disp,wt,mpg,
                color = cyColor, pch = 19,
                type = "h", lty.hplot = 2,
                main = "3D scatter plot with a 4th variable (cylinder) shown by color",
                xlab = "Displacement (cubic inches)",
                ylab = "Weight (lb/1000)",
                zlab = "MPG")
  legend("topleft", inset = .05,
         bty = "n", cex=.5,
         title = "Number of cylinders",
         c("4","6","8"), fill = c(mtCols[1],mtCols[2],mtCols[3]))
})




### Before and After plots
W = read.csv("/Users/jamescutler/Desktop/Stats_4000/various_data/WestminsterTreated.csv")
WC = read.csv("/Users/jamescutler/Desktop/Stats_4000/various_data/WestminsterControls.csv")
W$post.minus.pre = W$Post.treatment.asymmetry - W$Pre.treatment.asymmetry
WC$post.minus.pre = WC$Post.asymmetry - WC$Pre.asymmetry

colnames(WC) = c("pre","post","diffs")
# library(ggpubr)
ggpaired(WC, cond1 = "pre", cond2 = "post",
         fill = "condition", palette = "jco")
# wide to long:
# library(tidyr)
WC.long = gather(WC, key = "pre.or.post", value = "symmetry", c("pre","post"))
ggpaired(WC.long, x = "pre.or.post", y = "symmetry",
         color = "pre.or.post", line.color = "gray", line.size = .4, palette = "npg")



### Combined barplots of related topics - Top 10 mortality example from Overview of PH:
top10 = data.frame(source = rep(c("CDC","Turnock"),each = 10),
                   cause = c("Heart disease","Cancer","Accidents","Chronic lower resp",
                             "Stroke","Alzheimer's","Diabetes","Influenza and pneumonia",
                             "Nephritis/-otic/-osis","Suicide",
                             "Tobacco","Diet/activity patterns","Alcohol",
                             "Microbial agents","Toxic agents","Motor vehicles",
                             "Firearms","Sexual behavior","Illicit drug use","Nothing"),
                   toll = c(635260,598038,161374,154596,142142,116103,80058,51537,50046,44965,
                            435000,400000,85000,75000,55000,43000,29000,20000,20000,0),
                   orden = rep(1:10,2))

ggplot(top10, aes(factor(orden),toll, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = cause,y=toll),
            stat = "identity",
            position = position_dodge(width = .9),
            size = 4, angle = 60, hjust = .3, vjust = .3) +
  ggtitle("Comparing Turnock top 9 (preventable?) causes of death to the CDC's") +
  ylab("Number of deaths (in the year 2016?)") +
  xlab("")




## Factor and levels & labels to organize bars in a bar chart however you want!! SUPER EXCITING!
### Cancer epi example:
malesType = c("Prostate","Lung & Bronchus","Colon & Rectum","Urinary Bladder",
              "Melanoma","Kidney & R Pelvis","Non-Hodgkin L","Oral & Pharynx",
              "Leukemia","Liver & IHBD","All other sites")
malesRates = c(19,14,9,7,6,5,5,4,4,4,22); sum(malesRates)
femalesType = c("Breast","Lung & Bronchus","Colon & Rectum","Uterine Corpus",
                "Thyroid","Melanoma","Non-Hodgkin L","Pancreas","Leukemia",
                "Kidney & R Pelvis","All other sites")
femalesRates = c(30,13,7,7,5,4,4,3,3,3,21); sum(femalesRates)
length(malesType)
length(femalesType)
mf = data.frame(males_type = malesType,
                males_rate = malesRates,
                females_type = femalesType,
                females_rate = femalesRates)
mf2 = mf[,c(2,4)]
mfLong = gather(data = mf2, 
                key = "gender",
                value = "rate",
                c(males_rate,females_rate))
mfLong$type = c(malesType,femalesType)
mfLong$gender = as.character(mfLong$gender)
mfLong$gender = gsub("_rate","",mfLong$gender)
# THIS PLOT SUCKED! SO, PROBLEM SHOOTING STEPS FOLLOW:
# ggplot(mfLong, aes(type,rate, fill = gender)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9, vjust = .9))
sort(mfLong$rate)
mfLong$type[sort(mfLong$rate)] # nope
mfLong[order(mfLong$rate),"type"] # this could be it
mfLong$rate[order(mfLong$rate)]
# THIS IS THE SINGLE MOST IMPORTANT, MAGICAL STEP:
mfLong$type = factor(mfLong$type,
                     levels = mfLong[order(mfLong$rate),"type"],
                     labels = mfLong[order(mfLong$rate),"type"])
mfLong # Looks the same. Good.
# THE AMAZING SUPER EXCITING PLOT:
ggplot(mfLong, aes(type,rate, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9, vjust = .9),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "black", linetype = "dashed"),
        panel.grid.minor.y = element_line(colour = "gray")) +
  labs(title = "Estimated New Cancer Cases in the US in 2018",
       x="",y="Percentage of all cancers in that gender", fill = "Gender")
# Notice that ggplot is able to automatically remove the repeat cancer types. 
## Compare the bar chart above to the 7 repeat types in the matrix below:
cbind(as.matrix(mfLong[order(mfLong$rate),"type"]),
      mfLong$rate[order(mfLong$rate)])

### Don't forget the BRFSS example from SAS_class




### Grouped barplots 
## (for things like before and after, or two factors with one of each in the other, see example below)

# Whether you do grouped barplots obviously depends on what your data is like:
animals = data.frame(category = rep(c("declined","improved"),4),
                     reason = rep(c("genuine","misclassified","taxonomic","unclear"),each = 2),
                     species_n = c(24,16,41,85,2,7,41,117))

ggplot(animals, aes(factor(reason),species_n, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  
  ggtitle("number of species that declined or improved for various reasons") +
  xlab("reason for improvement or decline") + 
  ylab("number of species")

shayak = data.frame(part = rep(c(1,2),6),
                    difficulty = rep(c(0,1,2,3,4,6),each=2),
                    count = c(6,8,24,6,29,19,14,11,1,7,1,4))

ggplot(shayak, aes(factor(difficulty),count, fill = factor(part))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  
  ggtitle("number of problems in parts 1 & 2 at each difficulty level") +
  xlab("difficulty level") + 
  ylab("number of problems")
## Pretty straightforward. You just need to ...
# 1) set the "separate" categories as your x axis factor ['separate' is my terminology]
# 2) make your grouped category your fill and put it in the aesthetics
# 3) add a position = 'dodge'
# 4) and use a scale fill brewer with a palette of your choice




### Interaction plot in ggplot using stat_summary (without qplot) - poison/tx DV example:
headers = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/survival_data_Dean_Voss.csv",
                   skip = 0, header = FALSE, nrows = 1, as.is = TRUE)

pois = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/survival_data_Dean_Voss.csv",
                header = FALSE)
pois = pois[-1,-c(1,5)]
colnames(pois) = headers

pois$group = rep(1:12,each=4)

# With alpha instead of jitter:
ggplot(pois, aes(x=POISON,y=TIME,col=TRTMT)) +
  geom_point(alpha=.4) +
  stat_summary(fun.y = "mean", geom = "line", aes(group = factor(TRTMT)))

# With jitter:
ggplot(pois, aes(x=POISON,y=TIME,col=TRTMT)) +
  geom_jitter(width = .1) +
  stat_summary(fun.y = "mean", geom = "line", aes(group = factor(TRTMT))) +
  labs(title = "Survival times by treatment and poison",
       x="Poison",y="Survival time in units of 10 hours")




### Labels - add x-axis labels to tick marks in base R plot (William example)
subscales = c("hyperactivity",
              "aggression",
              "attention probs",
              "atypicality",
              "anxiety",
              "withdrawal",
              "depression",
              "somatization")
f = c(63,58,74,53,39,43,52,40)
m = c(73,70,67,51,37,41,48,40)
t = c(69,56,58,49,70,41,51,40)

plot(f, col = "red", type = "b", pch = 16, ylim = c(30,90),
     xlab = "Subscales",ylab = "T-scores", main = "William",
     xaxt = "n")
points(m, col = "blue", type = "b", pch = 18)
points(t, col = "green", type = "b", pch = 21)
axis(side = 1, at = 1:8, labels = FALSE)
text(seq(1,8,1), par("usr")[3] - .5, labels = subscales, srt = 50, pos = 1, xpd = TRUE)
legend(6,90, 
       legend = c("Father","Mother","Teacher"),
       col = c("red","blue","green"), lty = 1, cex = .9)

### Austin TX creative jobs earnings example
jobs = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Median_Earnings_of_Creative_Sector_Occupations__CLL.B.1.csv")

colnames(jobs)[c(3,4)] = c("Y2016","Y2017")
jobs$diff = jobs$Y2017 - jobs$Y2016

########## shorten the occupations ###########
class(jobs$Occupation)
jobs$Occupation = as.character(jobs$Occupation)

# cvec = rep("abcdefghi",3); cvec
# length(cvec)
# strtrim(cvec,c(2,4,6))

jobs$Occupation = strtrim(jobs$Occupation,width = 13)
##############################################

xlose = which(jobs$diff < 0)
xwin = which(jobs$diff > 0)

par(mar=c(7,4,2,1))

plot(1:nrow(jobs),jobs$Y2016, col = "blue", pch = 18, ylim = c(0,40),
     xaxt = "n", xlab  = "", ylab = "Bucks per hour", 
     main = "Changes in earnings for creative occupations: Austin, 2016-2017")

points(xlose,jobs[xlose,"Y2017"], col = "red", pch = 6)
points(xwin,jobs[xwin,"Y2017"], col = "green", pch = 2)
axis(side = 1, at = 1:nrow(jobs), labels = FALSE)
text(seq(1,nrow(jobs),1), par("usr")[3] - 5.5, 
     labels = jobs$Occupation, srt = 90, pos = 1, xpd = TRUE)
abline(v = 1:nrow(jobs), lty = 2, col = "gray")
legend(44,42,
       legend = c("2016","2017 increase","2017 decrease"),
       col = c("blue","green","red"), pch = c(18,2,6), cex = .9)

par(mar=c(5.1,4.1,4.1,2.1))

60*40*(52-9)
max(jobs$diff)
min(jobs$diff)
mean(jobs$diff)
mean(jobs$Y2016)
mean(jobs$Y2017)
median(jobs$Y2016)
median(jobs$Y2017)




### Lsmeans plot (easy) - poison/tx DV example:
headers = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/survival_data_Dean_Voss.csv",
                   skip = 0, header = FALSE, nrows = 1, as.is = TRUE)

pois = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/survival_data_Dean_Voss.csv",
                header = FALSE)
pois = pois[-1,-c(1,5)]
colnames(pois) = headers

pois$group = rep(1:12,each=4)

odAB = aov(TIME ~ factor(POISON)+factor(TRTMT), data = pois)
ls.trt = lsmeans(modAB, specs = ~TRTMT)
ls.trt

plot(ls.trt, main = "Survival time means by treatment",
     xlab = "Mean survival time in units of 10 hours",
     ylab = "Treatment factor")




### Overlapping barplots - tobacco state example from Applied Bayesian:
tob2 = read.csv("/Users/jamescutler/Desktop/Bayesian/data/6_tobstate.csv", 
                header = FALSE)
ggplot() +
  geom_bar(data = tob2, mapping = aes(1:nrow(tob2),tob2$V1*100), stat = "identity", width = .9, fill = "green") +
  geom_bar(data = tob2, mapping = aes(1:nrow(tob2),tob2$V2), stat = "identity", width = .4, fill = "blue") +
  scale_x_continuous(breaks = 1:nrow(tob2),
                     labels = tob2$V3) +
  ggtitle("Green = smoking prevalence; Blue = strength of laws") +
  xlab("Ones produce tobacco") + ylab("prev and law strength (green and blue)")




### Percentages or count labels above bars in barplot 
## (ALSO, HOW TO GET THE LEGEND FACTORS TO DISPLAY THE FACTOR LABELS RATHER THAN THE NUMBERS THEY CORRESPOND TO)
# YOU NEED THE SCALES LIBRARY JUST TO GET THE PERCENT OBJECT for LABEL(S)
RIOK = readRDS("/Users/jamescutler/Desktop/SAS/RI_OK_data.rds")
as.matrix(colnames(RIOK))
ggplot(na.omit(RIOK[,c("diabetes","exercise")]), aes(x=diabetes, group = exercise)) +
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..),y=..prop..), 
            stat = "count", size = 2, vjust = -.5) +
  facet_grid(~exercise) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(breaks = c(1,2), labels = levels(RIOK$diabetes)) + 
  # I FINALLY DID IT! IT TOOK TRYING SCALE_COLOR_MANUAL AND SOMETHING ELSE FIRST JUST TO FINALLY RALIZE THAT ITS SCALE_FILL_DISCRETE THAT DOES IT!!!!!!!!!!!
  labs(y="percent", fill = "diabetes status") +
  theme(axis.text.x = element_text(angle = 45))
# source for learning the successful way to change legend factor labels: https://ggplot2.tidyverse.org/reference/guide_legend.html

### BRFSS barplots in R:
## Figure 1: Health outcome by state
# THIS IS STUPID. IT HOLLABAUGH WANTED WHAT PERCENTAGE OF DIABETICS IN THIS 
# DATASET THAT CAME FROM OK AND WHAT PERCENTAGE CAME FROM RI. WHAT IF THERE ARE
# WAY MORE OK RESPONDENDTS IN THIS DATASET TO BEGIN WITH!?!?!? DUH!!!
ggplot(na.omit(RIOK), aes(x=state, group = diabetes)) +
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count") + 
  # LEARNED THAT YOU **DO** NEED TO PUT FACTOR() AROUND ..X.. IN ORDER TO GET IT TO WORK, IN SPITE OF THE FACT THAT IT'S ALREADY A FACTOR
  geom_text(aes(label = percent(..prop..),y=..prop..),
            stat = "count", size = 2, vjust = -.5) +
  facet_grid(~diabetes) +
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(breaks = c(1,2), labels = levels(RIOK$state)) +
  labs(title = "Health outcome by state",x="State",y="Percent", fill = "State")
# WHAT YOU SHOULD ACTUALLY BE LOOKING FOR--THE PERCENTAGE OF OK RESPONDENTS WHO
# WERE DIABETIC, COMPARED TO THE PERCENTAGE OF RI RESPONDENTS WHO WERE DIABETIC:
ggplot(na.omit(RIOK), aes(x=diabetes, group = state)) +
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count") +
  geom_text(aes(label = percent(..prop..),y=..prop..),
            stat = "count", size = 2, vjust = -.5) +
  facet_grid(~state) +
  scale_y_continuous(labels = percent) +
  scale_fill_discrete(breaks = c(1,2), labels = levels(RIOK$diabetes)) +
  labs(title = "Health outcome by state - the real way",x="Diabetes status",y="Percent", fill = "Diabetes status")
## THE TAKE HOME WITH THIS AND WITH THE SMOKING/LUNG CANCER QUESTION FROM BIOSTATS
# IS THAT IN BOTH CASES YOU DON'T WANT PROPORTION OF HEALTH OUTCOME BY RISK FACTOR;
# YOU WANT PROPORTION OF RISK FACTOR BY HEALTH OUTCOME!

## Figure 2: Health outcome by risk factor
# (It's the same as the one above at the beginning of the percentage topic)
ggplot(na.omit(RIOK[,c("diabetes","exercise")]), aes(x=diabetes, group = exercise)) +
  geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..),y=..prop..), 
            stat = "count", size = 2, vjust = -.5) +
  facet_grid(~exercise) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(breaks = c(1,2), labels = levels(RIOK$diabetes)) + 
  # I FINALLY DID IT! IT TOOK TRYING SCALE_COLOR_MANUAL AND SOMETHING ELSE FIRST JUST TO FINALLY RALIZE THAT ITS SCALE_FILL_DISCRETE THAT DOES IT!!!!!!!!!!!
  labs(y="percent", fill = "diabetes status") +
  theme(axis.text.x = element_text(angle = 45))

### Figure 3: Health outcome by risk factor, stratified by income
## Attempt #1 (looks weird):
# ggplot(na.omit(RIOK[,c("diabetes","exercise","income")]), 
#        aes(x=diabetes, group = exercise)) +
#   geom_bar(aes(y=..prop.., fill = factor(..x..)), stat = "count") +
#   geom_text(aes(label = scales::percent(..prop..),y=..prop..), 
#             stat = "count", size = 2, vjust = -.5) +
#   facet_grid(~income) +
#   scale_y_continuous(labels = scales::percent) +
#   scale_fill_discrete(breaks = c(1,2), labels = levels(RIOK$diabetes)) + 
#   
#   labs(y="percent", fill = "diabetes") +
#   theme(axis.text.x = element_text(angle = 45))
## Attempt #2 (getting closer!):
# ggplot(na.omit(RIOK[,c("diabetes","exercise","income")]), 
#        aes(x=diabetes, group = income)) +
#   geom_bar(aes(y=..prop.., fill = factor(..x..)), 
#            stat = "count", position = "dodge") +
#   geom_text(aes(label = percent(..prop..),y=..prop..), 
#             stat = "count", position = position_dodge(width = .9), # TOOK A WHILE TO FIGURE OUT THAT YOU NEED TO TYPE IT SPECIFICALLY AS POSITION = POSITION_DODGE()
#             size = 2, vjust = -.5) +
#   facet_grid(~exercise) +
#   scale_y_continuous(labels = percent) +
#   scale_fill_discrete(breaks = c(1,2,3), labels = levels(RIOK$income)) + 
#   
#   labs(title = "health outcome by risk factor, stratified by income",
#        y="percent", fill = "income") +
#   theme(axis.text.x = element_text(angle = 45))
## Attempt #3 (good enough, though it still includes the non-diabetic!):
ggplot(na.omit(RIOK[,c("diabetes","exercise","income")]), 
       aes(x=diabetes, group = income)) +
  geom_bar(mapping = aes(y=..prop.., fill = income), 
           stat = "count", position = "dodge") +
  geom_text(aes(label = percent(..prop..),y=..prop..), 
            stat = "count", position = position_dodge(width = .9), # TOOK A WHILE TO FIGURE OUT THAT YOU NEED TO TYPE IT SPECIFICALLY AS POSITION = POSITION_DODGE()
            size = 2, vjust = -.5) +
  facet_grid(~exercise) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(breaks = c(1,2,3), labels = levels(RIOK$income)) + 
  # WTF!!!!!!??????? NOW IT'S BACK TO SCALE_COLOR_MANUAL!!!!!!!
  labs(title = "Health outcome by risk factor, stratified by income",
       x="Diabetes status",y="Percent", fill = "Income") +
  theme(axis.text.x = element_text(angle = 45))

##################################################################################
## ALL OF THIS WAS BECAUSE DPLYR'S FILTER HAD THE STUPIDEST ERROR
# (WHICH WASN'T SOLVED THIS WAY--IT WAS SOLVED BY INSTALLING DPLYR 0.7.7 VERSION):
# install.packages("rlang")
# install.packages("remotes")
# remotes::install_github("tidyverse/dplyr#3894")
RIOK %>% select(diabetes,race,sex,income) %>% 
  filter(diabetes == "diabetic" & income == "<$25k") %>% nrow()
RIOK %>% select(diabetes,race,sex,income) %>% 
  filter(diabetes == "diabetic" & income == "$25-$50k") %>% nrow()
RIOK %>% select(diabetes,race,sex,income) %>% 
  filter(diabetes == "diabetic" & income == "≥$50k") %>% nrow()
##################################################################################

# And text with counts above bars (Brooke's income example) - MUCH EASIER THAN PERCENTS:
## Brooke's income 2017-2018

b1 = sum(143)
b2 = sum(34,218,15)
b3 = 0
b4 = sum(218,240)
b5 = sum(267)
b6 = sum(218,267,267,200,227)
b7 = sum(196,266,340)
b8 = sum(266,218,266,34,266,266)
b9 = sum(170,194,218,266,266)
b10 = sum(266,48,266,400,240,240,286)
b11 = sum(237,72,43,237)
b12 = sum(43,49,43,43,237,286,388)
b13 = sum(48)
b14 = sum(286,286,286,237,43,43)
b15 = sum(48,286)
b16 = sum(286,48,286,44,286)
b17 = sum(48,48,44,286,286)
b18 = sum(237,73,286,286)
b19 = sum(286,286)

brooke.income = vector()
for (i in 1:19){
  brooke.income[i] = eval(parse(text = paste0("b",i)))
}
brooke.income
Bmonths = seq.Date(from = as.Date("2017-06-01"), 
                   to = as.Date("2018-12-01"), by = "month")
brooke.df = data.frame(months = Bmonths,
                       income = brooke.income)

ggplot(brooke.df, aes(Bmonths,income)) +
  geom_bar(stat = "identity", fill = "#99CCFF") + 
  geom_text(aes(label = income), vjust = -.3) +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = .9, vjust = .9)) +
  labs(title = "Brooke's income: 2017-2018", x="", y = "USD") +
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%Y-%b")) +
  geom_hline(yintercept = mean(brooke.df$income), col = "red", linetype = "dashed")




### Stacked (compound) barplots in barplot() and ggplot2 (includes cool table margins!)
data("mtcars")
tableCars = table(mtcars$cyl,mtcars$gear, dnn = c("Cylinders","Gears"))
addmargins(tableCars)
barplot(tableCars, main = "Stacked Bar Chart", xlab = "Gears",
        beside = FALSE, col = c("turquoise4","turquoise2","turquoise"),
        xlim = c(0,1), width = .2)
legend("right", title = "Cylinders", legend = sort(unique(mtcars$cyl)),
       fill = c("turquoise4","turquoise2","turquoise"), box.lty = 0)
# Now in ggplot (it's actually less code!!):
head(mtcars)
ggplot(mtcars, aes(x=factor(gear),fill=factor(cyl))) +
  geom_bar() +
  labs(title = "Cylinders by gears", x="Gears", fill = "Cylinders")

# Stacked barplot with time series data:
red.what = c("One jalapeño sausage: 5 oz.",
             "One jalapeño sausage: 5 oz.",
             "One jalapeño sausage: 5 oz.",
             "One jalapeño sausage: 5 oz.",
             "Two burger patties from The Garage: 8 oz.",
             "One jalapeño sausage: 5 oz.")
red.amount.oz = c(5,5,5,5,8,5)
red.date = as.Date(c("2019-05-28","2019-05-29","2019-05-30","2019-05-31","2019-06-04",
                     "2019-06-06"), 
                   format = "%Y-%m-%d")
processed.yes.no = c("Yes","Yes","Yes","Yes","No","Yes")
# Primary dataframe:
redmeat = data.frame(amount = red.amount.oz,
                     meat.type = red.what,
                     dates = red.date,
                     processed = processed.yes.no)
# Add an index for which numeric order week it is:
week.nums = as.numeric(redmeat$dates-redmeat$dates[1]) %/% 7; week.nums
redmeat$weekNums = week.nums
# Add a sequence of weeks so that in the following step you can index the sequence:
x1 = floor_date(redmeat$dates, unit = "weeks") + 1; x1
#x2 = ceiling_date(redmeat$dates, unit = "weeks") + 1; x2
x1 = unique(x1)
#x2 = unique(x2)
# Index the sequence with the weekNums, and add a column for positioning the bars:
redmeat$weekIndex = x1[redmeat$weekNums+1]
redmeat$weekIndexBars = redmeat$weekIndex + 3
# Plot:
ggplot(redmeat, aes(x=weekIndexBars,y=amount,fill=factor(processed))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red","brown")) +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = as.Date(c("2019-05-01","2019-07-01"),format = "%Y-%m-%d")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9, vjust = .9)) +
  coord_cartesian(ylim = c(0,40)) +
  geom_hline(yintercept = 11, linetype = "dashed", colour = "red") +
  labs(title = "Weekly red and processed meat consumption",fill="Processed",
       x="",y="Ounces")




### Translucent histograms in base R hist()
## (from Bayesian notepad)
dcc = read.csv("/Users/jamescutler/Desktop/Bayesian/dcc.csv")
head(dcc,5)
plot(dcc$fc_kid ~ factor(dcc$bleach))
# dcc %>%
#   filter(fc_kid, bleach == 1) # THIS JUST PRODUCES A NEW DATA FRAME
dcc1 = filter(dcc, bleach == 1) # THIS DOES EXACTLY THE SAME THING
dcc2 = filter(dcc, bleach == 2)
hist(dcc1$fc_kid, col = rgb(1,0,0,alpha=.5), breaks = 20, 
     main = "Coliform bacteria with and without bleach")
hist(dcc2$fc_kid, col = rgb(0,0,1,alpha=.5), breaks = 10, add = TRUE)




### Unemployment Vox graphic - example of easy publishable custom graphics: 
# Monthly unemployment from 1990 to 2019

# STEPS I HAD TO GO THROUGH TO FINALLY GET MY GRAPH WITH THE YEARS ON THE X AXIS:
## 1. Define the headers using skip, nrows, and as.is (plus header=F).
## 2. Read in data and skip to the right (non-sensical) row!
## 3. Define colnames and rownames, then trim the df down
## 4. Convert the df to matrix, transpose it, and convert that to a vector (so I can create a long df out of this wide one I was given)
## 5. [Plot the vector in base R plot to make sure I got it right]
## 6. Create a new df with percent unemployment as one column, months and years as other columns
## 7. Use function from stackoverflow to convert month abbreviations to month numbers, and add a new column to the df with the month numbers
## 8. Add a dates column to the df that pastes the year, month numbers, and an arbitrary day number, in order to create a full date that can be converted to an as.Date
## 9. After converting the pasted-together %Y-%m-%d with as.Date, ...
## 10. ... plug it into ggplot2

# unem = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Unempl_1990_2019.csv")
# unem = unem[11:nrow(unem),]
# unem[1,]
# colnames(unem) = as.character(unem[1,]) # NOPE. THIS DID NOT WORK AT ALL. AND I DON'T KNOW WHY.

## 1. Header:
headers = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Unempl_1990_2019.csv",
                   skip = 11, header = FALSE, nrows = 1, as.is = TRUE) 
# THE SKIP = 11 MAKES NO SENSE. IF YOU WERE TO DO SKIP = 1, IT WOULD SKIP THE 
## FIRST ROW!!! SO WHY DOES SKIP = 1 MEAN SKIP ROW 1, AND SKIP = 11 MEANS SKIP 
## ROWS 1-10 INSTEAD OF UP TO AND INCLUDING THE 11TH ROW????

## 2. Read in data:
unem = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Unempl_1990_2019.csv",
                skip = 12, header = FALSE)

## 3. Define colnames, rownames, trim:
colnames(unem) = headers
unem = unem[,-14]
rownames(unem) = unem[,1]
unem = unem[,-1]

## 4. Convert to vector:
newunem = as.vector(t(as.matrix(unem)))
newunem

## 5. Plot it just to see:
plot(newunem, type = "l", lwd = 2, col = "blue")

## 6. Create new df:
dfunem = data.frame(per_unemplmt = newunem,
                    months = rep(colnames(unem),30),
                    year = rep(rownames(unem), each = 12))

# dfunem$dates = as.Date(dfunem$dates, format = "%b-%Y") # DOESN'T WORK!!

## 7. Nifty function:
# This function will convert the month abbreviations to month numbers:
monthConvert = function(month_b){
  strftime(strptime(paste(month_b,strftime(Sys.time(),"%d")), 
                    format = "%b %d"), 
           "%m")
}
dfunem$monthnums = monthConvert(dfunem$months)

## 8. Paste arbitrary day number to %Y-%m:
dfunem$dates = strptime(paste0(paste(dfunem$year,dfunem$monthnums, sep = "-"),
                               "-01"),
                        format = "%Y-%m-%d")

## 9. Convert to "Date" class using as.Date:
class(dfunem$dates)
dfunem$dates = as.Date(dfunem$dates, format = "%Y-%m-%d")
class(dfunem$dates)

## 10. ggplot2:
ggplot(dfunem, aes(dates,per_unemplmt)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 years"),
               labels = date_format("%Y"),
               limits = as.Date(c("1990-01-01","2019-04,01"), format = "%Y-%m-%d")) +
  theme(axis.text.x = element_text(size = 5)) +
  coord_cartesian(ylim = c(0,10)) +
  scale_y_continuous(breaks = seq(0,10,.5))

## A better ggplot:
Vox = dfunem %>% filter(year > 2014)
ggplot(Vox, aes(dates,per_unemplmt)) +
  geom_line(colour = "cadetblue", size = .8) +
  scale_x_date(breaks = date_breaks("1 years"),
               labels = date_format("%Y"),
               limits = as.Date(c("2015-01-01","2019-03-01"), format = "%Y-%m-%d")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  coord_cartesian(ylim = c(3.5,6)) +
  scale_y_continuous(breaks = seq(3.5,6,.5),
                     position = "right") +
  labs(x = "", y = "")
# Now I have a plot that is exactly like the one in Vox:
## https://www.vox.com/policy-and-politics/2019/3/22/18276155/donald-trump-2020-presidential-election-odds







#####################################################################################
#####################################################################################
#####################################################################################

# PACKAGES FOR PUBLICATION-WORTHY TABLES

# It seems like none are good for word docs

# qwraps     - PDF
# stargazer  - PDF - didn't work for aov table, but does work for lm models
# xtable     - LaTeX or HTML
# sjPlot     - HTML
# tangram    - HTML???
# kable      - HTML or PDF - VERY GOOD (worked with car's Anova output, and with summary(lsmeans.output))











