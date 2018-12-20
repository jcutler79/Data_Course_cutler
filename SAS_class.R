###############################
########## SAS CLASS ##########
###############################

# grades
hw = c(15,14.5,14.5,15,14.9,15,29.75,13,26,19); sum(hw)
15*11 + 20
Phw = sum(hw)/185; Phw
Pmid = .8075
Pproj = .93
sum(20*Phw,25*Pmid,25*Pproj,30*.90) # 92 on the final would be a sure A, but even a 90-final would get an A

# Important Libraries:
library(sas7bdat)
library(tables)
library(dplyr) # for the pipe
library(tidyr) # for the gather and spread

### text data
getwd()
lec2text = read.csv("/Users/jamescutler/Desktop/lec2text.csv", header = FALSE) # header needs to be false here cuz there is no header
colnames(lec2text) = c("id","year3","year4","board_exam","pass_board")


ggplot() +
  geom_point(data = lec2text, mapping = aes(x = id, y = year3)) +
  geom_point(data = lec2text, mapping = aes(x = id, y = year4), col = "red", alpha = .3) + 
  geom_point(data = lec2text, mapping = aes(x = id, y = board_exam), col = "blue", alpha = .3) +
  ylab("years 3, 4 and board exam (black, red, and blue)")

length(which(lec2text$year3 > 50)) # black
length(which(lec2text$year4 > 50)) # red
length(which(lec2text$board_exam > 50)) # blue
length(which(lec2text$year4 < 50)) # red
length(which(lec2text$board_exam < 50)) # blue

mean(lec2text$year3)
mean(lec2text$year4)
mean(lec2text$board_exam)
nrow(lec2text)
t.test(lec2text$year3,lec2text$year4)


#####################################################################################

# Week 3

# dem = read.csv("/Users/jamescutler/Desktop/Lecture 3 demographic data.csv") # I GOT RID OF THIS DATASET CUZ IT'S BORING
colnames(dem)
length(which(dem$gender == "F"))
length(which(dem$gender == "M"))
Fdem = dem[which(dem$gender == "F"),]
unique(dem$ethnic_group)

length(which(dem$ethnic_group == 2)) + length(which(dem$ethnic_group == 1))
nrow(dem) - (length(which(dem$ethnic_group == 2)) + length(which(dem$ethnic_group == 1)))
length(which(is.na(dem$ethnic_group)))


theNAs = data.frame(NAs = which(is.na(dem$ethnic_group)), 
                    Y = rep(1,length(which(is.na(dem$ethnic_group)))))
ggplot(theNAs, aes(NAs,Y)) +
  geom_point(col = "red", alpha = .3)

df = data.frame(weight = sample(100:300,10), height = sample(60:72,10))
sort(df$weight)
df$names = c("Aaron","Alice","Bobby","Cassandra","Denice","Fanny","Godwin","Harry","James","Paul")

df[order(df$weight),]
df[order(df$height),]
hist(df$weight)
hist(df$height)
ggplot(df, aes(weight,height)) + 
  geom_point() +
  geom_text(data = df, mapping = aes(weight,height, label = names), 
            size = 3, angle = 45, vjust = -.1, hjust = -.1) + 
  coord_cartesian(xlim = c(100,310),ylim = c(60,74)) +
  scale_y_continuous(breaks = seq(60,74,2))
  


###########################################################################################

# Week 4

mcat = read.csv("/Users/jamescutler/Desktop/SAS/mcat.csv")

tapply(mcat$average_mcat_score, mcat$gender, mean)
tapply(mcat$ms_gpa, mcat$gender, mean)
mcat.gpa = mcat[-which(is.na(mcat$ms_gpa)),]
tapply(mcat.gpa$ms_gpa, mcat.gpa$gender, mean)

tapply(mcat$age, mcat$gender, median)
hist(mcat[which(mcat$gender == "Male"),8], 
     main = "Ages of males and females \n(females in red)", 
     xlab = "age")
hist(mcat[which(mcat$gender == "Female"),8], col = "red", add = TRUE)
length(which(mcat$gender == "Male"))
length(which(mcat$gender == "Female"))/length(which(mcat$gender == "Male"))





###########################################################################################


### Lecture 6

x = round(rnorm(200, mean = 78, sd = 6))
x
hist(x, breaks = 15)
x[which(x <= 65)]
x[which(x >= 90)]
which(x == 70)
library(dplyr) # between function does x >= left and x <= right (not > and < )
grades = numeric()
for (i in 6:9){
  grades[i] = length(which(between(x,i*10,((i+1)*10) - 1))) # The -1 is perfect, except for the fact that it cuts off 100 at the very end; convenient that there is no 100 in this vector
}
grades = grades[6:9]; grades
sum(grades)
df = data.frame(Letter_grade = c("D","C","B","A"),
                Freqs = grades)
dim(df)[1]:1 # I LOVE HOW SIMPLE THIS CODE ENDED UP BEING!!!!
df = df[dim(df)[1]:1,]


##############################################################################

tlc = read.csv("/Users/jamescutler/Desktop/SAS/tlc.csv", header = FALSE)
tlc = tlc[,-1] # reads in with first column of NAs, not needed
colnames(tlc) = c("ID","group","W1","W2","W3","W4")
# length(which(tlc$group == "P")) # 50% of them are P, 50% are A
# class(tlc$group) # yep it's a factor already

# library(tidyr)

tlc.long = gather(tlc, key = "Week", value = "LeadLevels", c("W1","W2","W3","W4"))
ggplot(tlc.long, aes(x = Week, y = LeadLevels, col = group)) + 
  geom_boxplot() +
  xlab("Weeks 1 - 4") + ylab("Blood lead levels") + 
  ggtitle(label = "Comparing the tx group (A) to the placebo group (P)") +
  scale_y_continuous(breaks = seq(0,65,5))
mean(tlc.long$LeadLevels)

# For the separate A and P dataframes:
tlcP = tlc[which(tlc$group == "P"),]
tlcA = tlc[which(tlc$group == "A"),]

A.long = gather(tlcA, key = "Week", value = "LeadLevels", c("W1","W2","W3","W4"))
A.long$Week = as.factor(A.long$Week)
# plot(A.long$LeadLevels ~ A.long$Week)

P.long = gather(tlcP, key = "Week", value = "LeadLevels", c("W1","W2","W3","W4"))
P.long$Week = as.factor(P.long$Week)
# plot(P.long$LeadLevels ~ P.long$Week)

# Mean and sd for each group:
mean(A.long$LeadLevels) # tx group is lower
mean(P.long$LeadLevels)
sd(A.long$LeadLevels) # but it has a much higher sd
sd(P.long$LeadLevels)


#############################################################################################
#############################################################################################

nrt = read.sas7bdat("/Users/jamescutler/Desktop/SAS/homework_9_tobacco2.sas7bdat", debug = TRUE)

colnames(nrt)[c(7,9,11)] = c("cigs","calls","cigs4mo")
nrt[which(nrt$gender == 1),2] = "F"
nrt[which(nrt$gender == 2),2] = "M"

plot(nrt$age,nrt$cigs4mo)
plot(nrt$age,nrt$cigs)
plot(nrt$cigs,nrt$cigs4mo)
plot(factor(nrt$calls),nrt$cigs4mo)
plot(factor(nrt$weeks_nrt),nrt$cigs4mo)
plot(factor(nrt$gender),nrt$cigs)
length(which(nrt[,2] == "F"))
plot(factor(nrt$education),nrt$cigs4mo)

z = unique(nrt$weeks_nrt)
for (i in z){
  print(c(i,length(which(nrt$weeks_nrt == i))))
}
which(nrt$weeks_nrt == 0)
nrt[which(nrt$weeks_nrt == 0),7:11]
length(which(nrt$weeks_nrt == 8))
unique(nrt$calls)
for (i in 1:5){
  print(c(i,length(which(nrt$calls == i))))
}

which(nrt$calls < 3)
callgrps = nrt
callgrps[which(callgrps$calls < 3),9] = 1
callgrps[which(callgrps$calls >= 3),9] = 2
callgrps[which(callgrps$calls == 1),9] = "Low"
class(callgrps$calls)
callgrps[which(callgrps$calls == "2"),9] = "High"
plot(factor(callgrps$calls),callgrps$cigs4mo)

callgrps[which(callgrps$weeks_nrt < 4),8] = 1
callgrps[which(callgrps$weeks_nrt >= 4),8] = 2
callgrps[which(callgrps$weeks_nrt == 1),8] = "Fewer weeks"
callgrps[which(callgrps$weeks_nrt == "2"),8] = "More weeks"

ggplot(callgrps, aes(x=cigs,y=cigs4mo, col = factor(calls))) +
  geom_point() +
  facet_grid(facets = ~factor(weeks_nrt)) +
  ggtitle("does # cigs before correlate with # cigs after?") +
  xlab("cigarettes smoked per day before") +
  ylab("cigarettes smoked per day after")


####################################################################################
####################################################################################

### PROC REPORT - tables

library(sas7bdat)
c2 = read.sas7bdat("/Users/jamescutler/Desktop/SAS/class2.sas7bdat", debug = TRUE)





####################################################################################

# HW 10 - Vitamin E and Placebo data on 491 trial participants

# library(tables)
vite = read.sas7bdat("/Users/jamescutler/Desktop/SAS/vite.sas7bdat", debug = TRUE)

vit2 = vite
colnames(vit2)[4] = "alcgrp"
colnames(vit2)[5] = "smkgrp"

vit2[which(vit2$alcgrp == 0),4] = 0
vit2[which(vit2$alcgrp %in% c(1,2)),4] = 1
vit2[which(vit2$alcgrp > 2),4] = 2

vit2[which(vit2$smkgrp == 0),5] = 0
vit2[which(vit2$smkgrp %in% 1:19),5] = 1
vit2[which(vit2$smkgrp > 19),5] = 2

vit2[which(vit2$gender == 0),2] = "M"
vit2[which(vit2$gender == "1"),2] = "F"

vit2.tab = tabular(
  (
    Factor(gender,"Gender") + 
      Factor(alcgrp, "Alcohol use") +
      Factor(smkgrp, "Smoking status") + 
      Factor(bpgrp, "Mean arterial bp grp") +
      Factor(plaqgrp, "Plaque msrmnt grp")
  ) ~ 
    Factor(trt, "Treatment")*( (n=1) + (Percentage=Percent("row")) ),
  data=vit2
)



# Now to get p-values for the categorical interactions:
chisq.test(vit2$gender,vit2$trt) # This gives a p-value of .5411
chisq.test(vit2$gender,vit2$trt, correct = FALSE) # This gives the .4816 of SAS's chi-square test - so they don't use the contintuity correction! what the?
g_trt = table(vit2$gender,vit2$trt); g_trt
chisq.test(g_trt) # same: .5411
# So here's all five:
chisq.test(vit2$gender,vit2$trt, correct = FALSE) # gender
chisq.test(vit2$alcgrp,vit2$trt, correct = FALSE) # alcohol
chisq.test(vit2$smkgrp,vit2$trt, correct = FALSE) # smoking
chisq.test(vit2$bpgrp,vit2$trt, correct = FALSE)  # bp
chisq.test(vit2$plaqgrp,vit2$trt, correct = FALSE)# plaque
# and here's all five produced via a for loop:
mykey = c(2,4,5,14,15)
pvalues = vector()
for (i in 1:5){
  pvalues[i] = chisq.test(vit2[,mykey[i]],vit2$trt, correct = FALSE)[[3]]
}
pvalues
pdf = data.frame(p_values = pvalues)
rownames(pdf) = c("Gender","Alcohol use","Smoking status","Mean arterial bp grp","Plaque msrmnt grp")



##### BRFSS

p = read.sas7bdat("/Users/jamescutler/Desktop/SAS/p2.sas7bdat", debug = TRUE)
p$X_BMI5 = p$X_BMI5/100
length(which(p$INCOME2 == 99))
length(which(is.na(p$X_BMI5)))
unique(p$SEX)
length(which(p$SEX == 9))
length(which(p$X_RFBING5 == 9))
length(which(p$X_RACEGR3 == 9))

plot(p$X_BMI5 ~ factor(p$X_RFBING5))
length(which(p$X_RFBING5 == 2))/nrow(p)

binge = p[,c(1,8)]
length(which(binge$X_RFBING5 == 9))
binge = binge[-which(binge$X_RFBING5 == 9),]
binge[which(binge$X_STATE == 40),1] = "Oklahoma"
binge[which(binge$X_STATE == 44),1] = "Rhode Island"
binge[which(binge$X_RFBING5 == 1),2] = "No"
binge[which(binge$X_RFBING5 == 2),2] = "Yes"
length(which(binge$X_STATE == "Oklahoma"))

tbinge = table(binge)
ptbinge = round(prop.table(tbinge,1),3)
ptstate = round(prop.table(tbinge,2),3)
chisq.test(tbinge, correct = FALSE)

length(which(p$X_STATE == 40))/length(which(p$X_STATE == 44))
plot(p$X_BMI5 ~ factor(p$X_STATE))
plot(p$X_BMI5 ~ factor(p$SEX))
hist(p[which(p$SEX == 2),6])
hist(p[which(p$SEX == 1),6], col = "red", add = TRUE)

Y = c(ptbinge[[3]],ptbinge[[4]])
states = c("Oklahoma","Rhode Island")
barplot(Y, ylim = c(0,.2), 
        names.arg = states, col = c("red","blue"), border = NA)
barplot(c(tbinge[[3]],tbinge[[4]]), ylim = c(0,1000),
        names.arg = states, col = c("red","blue"), border = NA)


chess = vector()
for (i in 1:64){
  chess[i] = 2^(i-1)
}
chess
d = sum(chess)/100
prettyNum(d, big.mark = ",", scientific = FALSE)

head(p)
## possible interactions:
# state * binge
# state * income
# state * income category
# state * race
# sex   * income
# sex   * BMI 
# sex   * BMI category
# sex   * binge
# income* race
# income* age
# income* BMI
# income* BMI category
# income* binge
# race  * BMI
# race  * BMI category
# race  * binge
# age   * BMI
# age   * BMI category
# age   * binge
# BMI   * binge
# BMIcat* binge


##################################################################################
##################################################################################
## Full survey dataset (34 MB)
# diabete3      is on page  29
# _INCOME2 #68  is on page  37
# _RACEGR3 #283 is on page 161
# _AGEG5YR #285 is on page 163
# _BMI5CAT #293 is on page 167
# _RFBING5      is on page 173
# _paindx1      is on page 190

f = read.sas7bdat("/Users/jamescutler/Desktop/SAS/sasdata.sas7bdat", debug = TRUE) # about 1 minute and 10 seconds to load
# colnames(f)


# SEE ri.ok = readRDS("/Users/jamescutler/Desktop/SAS/RI_OK_data.rds") BELOW


### MY DIABETES DATAFRAME (and grouped barplot to compare states):
# diab = as.data.frame(f[,c(1,52)])
# t.dia = table(diab); t.dia
# diab.grpd = data.frame(state = rep(c("Oklahoma","Rhode Island"),each=6),
#                        code = colnames(t.dia),
#                        vals = c(t.dia[1,1:6],t.dia[2,1:6]))
# ggplot(diab.grpd, aes(factor(code),vals, fill = factor(state))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_brewer(palette = "Set1") +
#   ggtitle("Number of diabetic (1), non-diabetic (3), and others") +
#   xlab("coded responses") + ylab("# of people answering each question")
# 
# p.dia = round(prop.table(t.dia,1),4); p.dia
# p.dia.gr = data.frame(state = rep(c("Oklahoma","Rhode Island"),each=6),
#                       answ = colnames(p.dia),
#                       vals = c(p.dia[1,1:6],p.dia[2,1:6]))
# ggplot(p.dia.gr, aes(factor(answ),vals, fill = factor(state))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_brewer(palette = "Set1") +
#   ggtitle("Proportion of diabetic (1), non-diabetic (3), and others") +
#   xlab("coded responses") + ylab("proportions by state")
# 
# ### MY PHYSICAL ACTIVITY DATAFRAME (and grouped barplot to compare states):
# act = data.frame(state = f$X_STATE, activity = f$X_PAINDX1)
# t.act = table(act); t.act
# p.act = round(prop.table(t.act,1),4); p.act
# p.act.gr = data.frame(state = rep(c("Oklahoma","Rhode Island"),each=3),
#                       answ = colnames(p.act),
#                       vals = c(p.act[1,1:3],p.act[2,1:3]))
# ggplot(p.act.gr, aes(factor(answ),vals, fill = factor(state))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_brewer(palette = "Set1") +
#   ggtitle("Proportion of people who met the \n aerobic recommendation (1) and not (2)") +
#   xlab("coded responses") + ylab("proportions by state")
# 
# ### MY AGE DATAFRAME (AND GROUPED BARPLOT):
# age = data.frame(state = f$X_STATE,
#                  age = f$X_AGEG5YR)
# t.age = table(age); t.age
# p.age = round(prop.table(t.age,1),4); p.age
# p.age.gr = data.frame(state = rep(c("Oklahoma","Rhode Island"),each=14),
#                       answ = colnames(p.age),
#                       vals = c(p.age[1,1:14],p.age[2,1:14]))
# library(forcats)
# answ = colnames(p.age)
# ord.nums = as.character(1:14); ord.nums
# p.age.gr = fct_relevel(answ, ord.nums) %>%
#   data.frame(state = rep(c("Oklahoma","Rhode Island"),each=14),
#              answ = .,
#              vals = c(p.age[1,1:14],p.age[2,1:14]))
# ages = c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69",
#          "70-74","75-79","80 & older","DK/refused")
# ggplot(p.age.gr, aes(factor(answ),vals, fill = factor(state))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_brewer(palette = "Set1") +
#   ggtitle("Proportion of people in age groups 1-13") +
#   xlab("coded responses") + ylab("proportions by state") +
#   scale_x_discrete(labels = ages) +
#   theme(axis.text.x = element_text(angle = 45, hjust = .9))
# 
# 
# 
# 
# ### MY FULL DATAFRAME WITH THE VARIABLES DIABETES AND EXERCISE:
mine = data.frame(state = f$X_STATE,
                  sex = f$SEX,
                  income = f$INCOME2,
                  race = f$X_RACEGR3,
                  age.grp = f$X_AGEG5YR,
                  BMI = f$X_BMI5,
                  BMI.grp = f$X_BMI5CAT,
                  binge = f$X_RFBING5,
                  diabetes = f$DIABETE3,
                  exercise = f$X_PAINDX1)

mine[which(mine$state == 40),1] = "OK"
mine[which(mine$state == "44"),1] = "RI"

mine[which(mine$sex == 1),2] = "Male"
mine[which(mine$sex == "2"),2] = "Female"

mine[which(mine$income %in% c(1,2,3,4)),3] = 1 # "1" # "<$25k"
mine[which(mine$income %in% c(5,6)),3] = 2 # "2" # "$25-$50k"
mine[which(mine$income %in% c(7,8)),3] = 3 # "3" # "≥$50k"
mine[which(mine$income %in% c(77)),3] = 4 # "4" # "DK"
mine[which(mine$income %in% c(99)),3] = 5 # "5" # "refused"
# length(which(mine$income %in% c("DK","refused")))/nrow(mine) # 19.9%
# HAS SOME NaNs IN ADDITION TO THE ABOVE
mine$income = factor(mine$income,
                     levels = c(1,2,3,4,5),
                     labels = c("<$25k","$25-$50k","≥$50k","DK","refused"))

mine[which(mine$race == 1),4] = 1 # "1" # "White/NH"
mine[which(mine$race == 2),4] = 2 # "2" # "Black/NH"
mine[which(mine$race == 5),4] = 3 # "3" # "Hispanic"
mine[which(mine$race %in% c(3,4)),4] = 4 # "4" # "Other/NH"
mine[which(mine$race == 9),4] = 5 # "DK/refused"
# length(which(mine$race == "DK/refused"))/nrow(mine)
mine$race = factor(mine$race,
                   levels = c(1,2,3,4,5),
                   labels = c("white","black","hispanic","other","DK/refused"))

mine[which(mine$age.grp %in% c(1,2)),5] = 1 # "1" # "18-29"
mine[which(mine$age.grp %in% c(3,4,5,6)),5] = 2 # "2" # "30-49"
mine[which(mine$age.grp %in% c(7,8,9,10)),5] = 3 # "3" # "50-69"
mine[which(mine$age.grp %in% c(11,12,13)),5] = 4 # "4" # "70 & older"
mine[which(mine$age.grp == 14),5] = 5 # "DK/refused"
# length(which(mine$age.grp == "DK/refused"))/nrow(mine)
mine$age.grp = factor(mine$age.grp,
                      levels = c(1,2,3,4,5),
                      labels = c("18-29","30-49","50-69","70 & older","DK/refused"))

mine$BMI = mine$BMI/100
# length(which(is.na(mine$BMI))) # 1016 are NA, same as the number of NaN in the BMI.grp!

# mine[which(mine$BMI.grp == 1),7] = 1 # "under.w"
# mine[which(mine$BMI.grp == "2"),7] = 2 # "normal.w"
# mine[which(mine$BMI.grp == "3"),7] = 3 # "over.w"
# mine[which(mine$BMI.grp == "4"),7] = 4 # "obese"
# length(which(mine$BMI.grp == "NaN"))/nrow(mine) # 8.3% ... 1016 (same as BMI continuous!)
mine$BMI.grp = factor(mine$BMI.grp,
                      levels = c(1,2,3,4),
                      labels = c("under.w","normal.w","over.w","obese"))

mine[which(mine$binge == 1),8] = 1 # "No"
mine[which(mine$binge == 2),8] = 2 # "Yes"
mine[which(mine$binge == 9),8] = 3 # "DK/refused"
# length(which(mine$binge == "DK/refused"))/nrow(mine) # 7%
mine$binge = factor(mine$binge,
                    levels = c(1,2,3),
                    labels = c("No","Yes","DK/refused"))

mine[which(mine$diabetes %in% c(1,2)),9] = 1 # "1" # "diabetic"
mine[which(mine$diabetes %in% c(3,4)),9] = 2 # "2" # "non-diabetic"
mine[which(mine$diabetes == 9),9] = 3 # "3" # "refused"
mine[which(mine$diabetes == 7),9] = 4 # "4" # "DK"
# length(which(mine$diabetes %in% c("DK","refused")))/nrow(mine)
mine$diabetes = factor(mine$diabetes,
                       levels = c(1,2,3,4),
                       labels = c("diabetic","non-diab.","refused","DK"))

mine[which(mine$exercise == 1),10] = 1 # "Met recomm."
mine[which(mine$exercise == 2),10] = 2 # "Did not"
mine[which(mine$exercise == 9),10] = 3 # "DK/refused"
# length(which(mine$exercise == "DK/refused"))/nrow(mine) # 12.2%
mine$exercise = factor(mine$exercise,
                       levels = c(1,2,3),
                       labels = c("met recomm.","didn't","DK/refused"))





# permanent Rhode Island / Oklahoma dataset (for fast loading in the future):
saveRDS(mine, file = "/Users/jamescutler/Desktop/SAS/RI_OK_data.rds")
ri.ok = readRDS("/Users/jamescutler/Desktop/SAS/RI_OK_data.rds")

ggplot(mine, aes(BMI.grp, fill = diabetes)) +
  geom_bar(stat = "count") +
  ggtitle("Income distribution in RI and OK")
  # guides(fill = FALSE)

myvals(f$X_STATE,f$X_PAINDX1)
p.act.gr = data.frame(state = rep(c("Oklahoma","Rhode Island"),each=3),
                      answ = colnames(p.act),
                      vals = capture.output(myvals(f$X_STATE,f$X_PAINDX1)))

categorical.grouped.barplots = function(cat.data.fill,cat.data.factors,
                                        yourtitle,yourxlab,yourylab){
  l.u.fct = length(unique(cat.data.factors))
  df = data.frame(LLENAR = cat.data.fill,
                  FACTORES = cat.data.factors)
  t.df = table(df)
  p.df = prop.table(t.df,1)
  myvals = function(cat.data.fill,cat.data.factors){
    for (i in 1:length(unique(cat.data.fill))){
      cat(p.df[i,1:length(unique(cat.data.factors))], sep = "\n")
    }
  }
  p.df.grpd = data.frame(FILL = rep(unique(cat.data.fill), each=l.u.fct),
                         FACTORS = colnames(p.df),
                         vals = capture.output(myvals(cat.data.fill,cat.data.factors)))
  p.df.grpd$vals = as.character(p.df.grpd$vals)
  p.df.grpd$vals = as.numeric(p.df.grpd$vals)
  print(p.df.grpd$vals)
  ggplot(p.df.grpd, aes(factor(FACTORS),vals, fill = factor(FILL))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_brewer(palette = "Set1") +
    ggtitle(yourtitle) + xlab(yourxlab) + ylab(yourylab)
}

categorical.grouped.barplots(mine$state,mine$diabetes,
                             "Proportion of diabetics by state",
                             "survey responses","proportions")
categorical.grouped.barplots(mine$state,mine$exercise,
                             "Proportions of people meeting the aerobic recommendation",
                             "survey responses","proportions by state")
categorical.grouped.barplots(mine$state,mine$BMI.grp,
                             "Proportion of BMI categories by state",
                             "survey responses","proportions")
categorical.grouped.barplots(mine$race,mine$BMI.grp,
                             "Proportion of BMI categories by race",
                             "survey responses","proportions")
categorical.grouped.barplots(mine$income,mine$BMI.grp,
                             "Proportion of BMI groups by income",
                             "survey responses","proportions")
categorical.grouped.barplots(mine$income,mine$race,
                             "Proportion of races by income",
                             "survey responses","proportions")


# state = f$X_STATE,
# sex = f$SEX,
# income = f$INCOME2,
# race = f$X_RACEGR3,
# age.grp = f$X_AGEG5YR,
# BMI = f$X_BMI5,
# BMI.grp = f$X_BMI5CAT,
# binge = f$X_RFBING5,
# diabetes = f$DIABETE3,
# exercise = f$X_PAINDX1


##################################################################################
##################################################################################

library(dplyr)       # for the %>% operator
library(data.table)  # THIS IS MY NEW FAVORITE PACKAGE. SQL IN R!!!!!!!!

train = read.csv("/Users/jamescutler/Desktop/SAS/training2017.csv")
colnames(train)
colnames(train)[6] = "Training"
colnames(train)[10] = "Smoke"
colnames(train)[12] = "Adverse"
attach(train)
Asum = as.matrix(summary(Age))
as.matrix(Asum[nrow(Asum):1,]) # THERE'S A GUY IN HERE WHO'S 17 - THAT'S AN ERROR
# train[,5:14] %>% table() # VERY BAD IDEA!!
train[which(train$Training == 1),6] = "Y"
train[which(train$Training == "0"),6] = "N"

# MONEY!!!!!!!: (look at contingency tables of all of the variables you think might have some typo values in addition to the 2 or 3 appropriate values)
stuff = c(5:10,12:14); stuff
for (i in stuff){
  print(colnames(train)[i])
  print(as.matrix(table(train[,i])))
  print(cat("...\n...\n"))
}
# NOW THE BUSY WORK OF FIXING ALL THE PROBLEMS LAID BARE WITH THIS FOR LOOP CAN BEGIN.

## BUT WHAT ABOUT POTENTIAL DATE PROBLEMS (TYPOS) IN THE ADMIT AND DISCHARGE COLUMNS???
# If someone was erroneously entered in as leaving before being admitted, we could detect that.
# If someone stayed in the hospital for a ridiculously long time, we could detect that too.
Admit.date = as.Date(Admit.date, format = "%m/%d/%y")
Discharge.date = as.Date(Discharge.date, format = "%m/%d/%y")
stays = difftime(Discharge.date,Admit.date, units = "weeks")
which(stays < 0) # Two are negative timelapses - those are typos
which(stays > 10)
max(stays) # One guy stayed in the hospital for a year. Could be realisitic.
staysvec = as.vector(stays)
barplot(staysvec) # SO THAT'S HOW MUCH BIGGER 519 IS THAN 52
staysvec[114] # This is the -519
barplot(staysvec[-114]) # LOVE IT.

# data.table is freaking SQL in R!!!!!!!!!!!!!!
myDT = as.data.table(train)
ans = myDT[Gender == "F" & Age > 75 & Training == "Y"] # WOW THIS IS FREAKING AWESOME! SQL!!!!!!!!!
head(ans)
length(ans)
nrow(ans)
ans


################################################################################
# diabetes/demo final

D = read.csv("/Users/jamescutler/Desktop/SAS/mydiabetesdata.csv")
unique(D$bmi) # DOESN'T TELL YOU IF THERE ARE EMPTY CELLS! WTF!?!?!?
which(D$bmi == "")
which(is.na(D$chol))
length(which(D$glyhb >= 6.5))
which(is.na(D$glyhb))
class(D$glyhb) # it's numeric, even though some are NAs!
sort(D$glyhb)

qqnorm(D$whratio)
qqline(D$whratio, col = "red")
hist(D$whratio)
shapiro.test(D$whratio)
foo = rnorm(300)
hist(foo)
shapiro.test(foo)


D$bmi = as.character(D$bmi)
D[which(D$bmi %in% c("U","N","OV")),11] = "Not obese"
D[which(D$bmi == "OB"),11] = "Obese"
D$bmi = as.factor(D$bmi)

D[which(D$glyhb < 6.5),12] = "Not diabetic"
D[which(D$glyhb >= 6.5),12] = "Diabetic"
colnames(D)[12] = "Diabetes"

table(D$Diabetes,D$bmi)
chisq.test(D$Diabetes,D$bmi, correct = FALSE)
fisher.test(D$Diabetes,D$bmi)
unique(D$Diabetes)
unique(D$bmi)

###
which(is.na(D$glyhb))
D[which(D$glyhb >= 6.5),5] = "Diabetic"
D[which(D$glyhb < 6.5),5] = "Non-diabetic"
class(D$glyhb)

D$bmi = as.character(D$bmi)
D[which(D$bmi %in% c("N","U","OV")),11] = "Not obese"
D[which(D$bmi == "OB"),11] = "Obese"
D[which(D$bmi == ""),11] = NA

t1 = table(D$glyhb,D$bmi)
t2 = table(D$bmi,D$glyhb)
barplot(t1, beside = TRUE)
barplot(t2, beside = TRUE, col = c("red","blue"))
legend("topleft",
       legend = c("Not obese","obese"),
       fill = c("red","blue"))
# prop.table argument has to be a table! Can't be raw data!
t2.prop = prop.table(t2); t2.prop 
barplot(t2.prop, beside = TRUE, col = c("red","blue"))
legend("topleft",
       legend = c("Not obese","obese"),
       fill = c("red","blue"))


######################

# lead

lead = read.csv("/Users/jamescutler/Desktop/SAS/lead.csv", header = FALSE)
colnames(lead) = c("ID","gender","age_group","year","levels")
class(lead)
means = tapply(lead$levels, factor(lead$year), mean); means
plot(levels ~ factor(year), data = lead)
points(means, col = "red", pch = 18)

# tidyr
ls = spread(lead,year,levels)
# dplyr
colnames(ls)[4] = "blood1"
colnames(ls)[5] = "blood2"
ls$leaddiff = ls$blood2 - ls$blood1
summary(ls$blood1)[[2]]
ls$blood1 %>% summary() %>% as.matrix()
lead %>% group_by(year) %>% summarise(N = n(),
                                      Std_Dev = sd(levels))
                                             
summary(ls$blood1)[5]
tabular(
  (
    Factor(blood1,"Baseline") + 
           Factor(blood2,"Follow up") + 
           Factor(leaddiff,"Difference")
    ) ~
          ((n=1) + min + max + median + summary()[[2]] + summary()[[5]] + mean + sd),
        data = ls)
tabular(
  1
   ~ 
    ( (n=1) + blood1*(min + max + median + mean + sd) ),
  data = ls
)


vit2.tab = tabular(
  (
    Factor(gender,"Gender") + 
      Factor(alcgrp, "Alcohol use") +
      Factor(smkgrp, "Smoking status") + 
      Factor(bpgrp, "Mean arterial bp grp") +
      Factor(plaqgrp, "Plaque msrmnt grp")
  ) ~ 
    Factor(trt, "Treatment")*( (n=1) + (Percentage=Percent("row")) ),
  data=vit2
)

tabular(
  Species 
  ~
    (n=1) + Format(digits=2)*(Sepal.Length + Sepal.Width)*(mean + sd),
  data = iris
)

t1 = tabular(
  Factor(year,"Year")
  ~
    (n=1) + Format(digits=2)*(levels)*(min + max + median + mean + sd),
  data = lead
)

# ls[1,6] = 0 # THIS TABLES PACKAGE IS A PIECE OF CRAP. YOU NEED TO CHANGE THE NA IN ORDER TO MAKE IT WORK. 
ls2 = ls[-which(is.na(ls$leaddiff)),]
# THIS TABLES THING IS SUCH A PIECE OF CRAP (DOESN'T WORK):
# ls2$year = rep("diff",nrow(ls2))
# t2 = tabular(
#   Factor(year,"Year")
#   ~
#     (n=1) + Format(digits=2)*leaddiff*(min + max + median + mean + sd), 
#   data = ls2
# )
# rbind(t1,t2)






