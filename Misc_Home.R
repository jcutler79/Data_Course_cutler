# Miscellaneous Home Stuff



# Libraries:
library(readxl)
library(ggplot2)
library(scales)    # for date_breaks("1 weeks"); for adding text with percents above bars in barplots
library(lubridate) # for floor_date and ceiling_date (to get the intervals for the geom_rect months)
library(gridExtra)



# TABLE OF CONTENTS:

## timeDiffandFreq function

## Budget

## Brooke 

## William



## Red meat / processed meat consumption

## Eating out

## Gas

## Headaches



## Dow Jones Industrial Average: https://finance.yahoo.com/quote/%5EDJI/history/

## sleep data

## crime in the US

## US ammendments

## Denmark's average annual GDP growth

####################

# This function's output:
## 1) How long the time interval is (in units supplied) between the start and end dates supplied
## 2) How many events there were per supplied unit of time
## 3) Returns: (1) time lapse; (2) number of events per unit time; (3) and (4) time unit
timeDiffandFreq = function(df,columnNum,secsminshoursdaysweeks,numEvents = nrow(df),
                           months = FALSE,years = FALSE, totCostCol = numeric()){
  begYmd = df[,columnNum][1]
  endYmd = df[,columnNum][nrow(df)]
  thebeg = strptime(begYmd, format = "%Y-%m-%d")
  theend = strptime(endYmd, format = "%Y-%m-%d")
  unitTime = substr(secsminshoursdaysweeks,start=1,stop=nchar(secsminshoursdaysweeks)-1)
  dec.round = 1
  if (months == TRUE){
    thediff = cat("Time difference of ",as.numeric(difftime(theend,thebeg,units = "days"))/30.5, " months\n")
    numdiff = round(as.numeric(difftime(theend,thebeg,units = "days"))/30.5, dec.round) # Gives the number of months
    Freq = round(numEvents/numdiff,dec.round)
    if (totCostCol != 0){
      CostPerTime = round(sum(df[,totCostCol])/numdiff,dec.round)
    }
  } else if (years == TRUE){
    thdiff = cat("Time difference of ",as.numeric(difftime(theend,thebeg,units = "days"))/365.25, " years\n")
    numdiff = round(as.numeric(difftime(theend,thebeg,units = "days"))/365.25, dec.round)
    Freq = round(numEvents/numdiff,dec.round)
    if (totCostCol != 0){
      CostPerTime = round(sum(df[,totCostCol])/numdiff,dec.round)
    }
  } else{
    thediff = difftime(theend,thebeg,units = secsminshoursdaysweeks)
    numdiff = round(as.numeric(thediff),dec.round)
    Freq = round(numEvents/numdiff,dec.round)
    if (totCostCol != 0){
      CostPerTime = round(sum(df[,totCostCol])/numdiff,dec.round)
    }
  }
  print(thediff)
  if (totCostCol != 0){
    cat("There were ",Freq," events per ",unitTime,
        "\n And ",CostPerTime," dollars spent per ",unitTime,"\n")
    return(c(numdiff,Freq,secsminshoursdaysweeks,unitTime,CostPerTime))
  } else{
    cat("There were ",Freq," events per ",unitTime,"\n")
    return(c(numdiff,Freq,secsminshoursdaysweeks,unitTime))
  }
}


####################


### Budget:

utilities = 225
cars = 315
phones.internet = 130
car.gas = 130
student.loans = 221
mortgage = 2000
clothes.misc = 500
12*sum(utilities,cars,phones.internet,car.gas,student.loans,mortgage,clothes.misc)
550*2*12
24000+30000



########################################################################################
########################################################################################

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
n = 19

brooke.income = vector()
for (i in 1:n){
  brooke.income[i] = eval(parse(text = paste0("b",i)))
}
brooke.income

Bmonths = seq.Date(from = as.Date("2017-06-01"), 
                   to = as.Date("2018-12-01"), by = "month")

brooke.df = data.frame(months = Bmonths,
                       income = brooke.income)

ggplot(brooke.df, aes(Bmonths,income)) +
  geom_bar(stat = "identity", fill = "#99CCFF") + 
  geom_text(aes(label = income), vjust = -.3) + # THIS IS WAY EASIER THAN ADDING PERCENT LABELS
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = .9, vjust = .9)) +
  labs(title = "Brooke's income: 2017-2018", subtitle = "(mean in red)", x="", y = "USD") +
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%Y-%b")) +
  geom_hline(yintercept = mean(brooke.df$income), col = "red", linetype = "dashed")




########################################################################################
########################################################################################

## William

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





########################################################################################
########################################################################################

# Red meat / processed meat consumption

# On the basis of available data, the World Cancer Research Foundation 2007
## (WCRF 2007 ) on animal food intake recommends as public health goal that the
## population average consumption of red meat to be no more than 300 g (11 oz) a
## week, very little if any of which to be processed.

# Processed meat:
## Processed meat is defined as any meat preserved by smoking, curing or salting, 
## or with the addition of chemical preservatives; examples include bacon, salami, 
## sausages, hot dogs or processed deli or lunch- eon meats.

## Processed meat refers to choices such as bacon, sausage, ham, hot dogs and bologna.
## ... However, fresh red meat – which refers to beef, lamb and pork – whether it is 
## solid (like a steak or roast) or ground (like meatloaf or hamburger) is linked to 
## colorectal cancer risk when consumed in amounts beyond 18 ounces per week.

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
week.nums = as.numeric(redmeat$dates-redmeat$dates[1]) %/% 7; week.nums
redmeat$weekNums = week.nums

x1 = floor_date(redmeat$dates, unit = "weeks") + 1; x1
x2 = ceiling_date(redmeat$dates, unit = "weeks") + 1; x2
x1 = unique(x1)
x2 = unique(x2)

redmeat$weekIndex = x1[redmeat$weekNums+1]
redmeat$weekIndexBars = redmeat$weekIndex + 3

ggplot(redmeat, aes(x=weekIndexBars,y=amount,fill=factor(processed))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red","brown")) +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = as.Date(c("2019-05-20",today()+31),format = "%Y-%m-%d")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9, vjust = .9)) +
  coord_cartesian(ylim = c(0,40)) +
  geom_hline(yintercept = 11, linetype = "dashed", colour = "red") +
  labs(title = "Weekly red and processed meat consumption", 
       subtitle = "(11 oz. is the recommended weekly limit)",
       fill="Processed",x="",y="Ounces")


# For meat.rects dataframe:
meat.week.sums = tapply(redmeat$amount,redmeat$weekNums,sum)
meat.week.sums

# num.meat = as.numeric(redmeat$dates); num.meat
# meat.bins = seq(as.numeric(floor_date(redmeat$dates[1], unit = "weeks")),
#                 as.numeric(ceiling_date(redmeat$dates[nrow(redmeat)], unit = "weeks")),7)
# meat.bins

meat.cols = c("red","orange","brown")

meat.rects = data.frame(x1 = x1,
                        x2 = x2,
                        y1 = 0,
                        y2 = meat.week.sums)
meat.rects$weeks = rep("quien.sabe",length(unique(redmeat$weekNums))) # WOW. THE ALPHABETICAL ORDER MATTERS

ggplot() +
  geom_bar(data = redmeat, mapping = aes(dates,amount,fill = processed),
           stat = "identity", width = .9) +
  geom_rect(data = meat.rects, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=weeks)) +
  scale_fill_manual(values = alpha(meat.cols, c(1,.4,1))) +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = as.Date(c("2019-05-01","2019-07-01"),format = "%Y-%m-%d")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9, vjust = .9),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        panel.grid.minor.y = element_line(color = "gray", linetype = "dotted")) +
  coord_cartesian(ylim = c(0,30)) +
  geom_hline(yintercept = 11, linetype = "dashed", colour = "black") +
  labs(title = "Red and processed meat consumption",
       subtitle = "dashed line is maximum recommended limit of 11 oz. a week",
       x="",y="Ounces", fill = "Processed")




########################################################################################
########################################################################################

# Eating out:


item = c("SnB","Jimmy Johns","Jimmy Johns","SnB","Dominos","Basil",
         "Five Guys","Dominos","Chipotle","Jimmy Johns","Go Go Sushi",
         "Go Go Sushi","Dominos for SAS final","SMASH PASS","Five Guys",
         "Chipotle","Jimmy Johns","Taqueria Sanchez","SnB","Chipotle",
         "Five Guys","Five Guys","SnB","Go Go Sushi","The Garage")
cost = c(15,14,14,15,9,9,20,9,11,9,14,14,8,50,22,10,14,8,20,10,22,22,15,17,15)
newdates = as.Date(c("2018-08-13","2018-09-07","2018-09-18","2018-09-21",
                     "2018-09-25","2018-09-26","2018-09-29","2018-10-08",
                     "2018-10-14","2018-10-23","2018-10-24","2018-11-15",
                     "2018-12-06","2018-12-17","2019-02-07","2019-02-09",
                     "2019-02-23","2019-02-25","2019-03-09","2019-03-20",
                     "2019-04-08","2019-04-22","2019-05-06","2019-05-09",
                     "2019-06-04"),
                   format = "%Y-%m-%d")
# length(item)
# length(cost)
# length(newdates)
budget = data.frame(item = item,
                    cost = cost,
                    dates = newdates)

# For the geom_rect() months:
f = seq(as.Date("2018-08-06"),today()+31, by = 31) # ONLY THE LAST DATE HERE WILL NEED TO BE ADJUSTED
f

begD = vector()
for (i in 1:length(f)){
  begD[i] = floor_date(f[i], "month")
}
x1 = as.Date(begD, origin = "1970-01-01", format = "%Y-%m-%d")
x1

endD = vector()
for (i in 1:length(f)){
  endD[i] = ceiling_date(f[i], "month")
}
x2 = as.Date(endD, origin = "1970-01-01", format = "%Y-%m-%d")
x2

# I'M GOING TO USE THE STANDARD DEVIATION:
mySD = function(cost){
  mymean = mean(cost)
  mySD = sd(cost)
  ub = mymean+mySD
  lb = mymean-mySD
  return(c(lb,ub))
}
food.lb.ub = mySD(cost = budget$cost[-14])
food.lb.ub

food.rects = data.frame(x1 = x1,
                        x2 = x2,
                        y1 = rep(food.lb.ub[1],length(f)),
                        y2 = rep(food.lb.ub[2],length(f))
                        ) # I FIGURED IT OUT (SEE BELOW: FOOD.RECTS$MONTHS = FOOD.COLS)! - How to update this so I don't have to keep updating it manually?

10%%2
myColors = function(df){
  numReps = nrow(df)
  if (numReps %% 2 != 0){
    food.cols = c(rep(c("red","goldenrod"),nrow(df)/2),"red")
  } else{food.cols = rep(c("red","goldenrod"),nrow(food.rects)/2)}
  return(food.cols)
}

food.cols = myColors(food.rects)

nrow(food.rects)
length(food.cols)
food.rects$months = food.cols # THIS IS HOW YOU AUTOMATICALLY UPDATE THE FILL COLUMN OF THE FOOD.RECTS DF!

food.freq.cost = timeDiffandFreq(budget,
                                 3,
                                 "months",
                                 months = TRUE,
                                 totCostCol = 2)
food.freq.cost
cat("I've spent ",sum(budget$cost)," dollars so far on eating out.")

ggplot() +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = as.Date(c("2018-07-30",today()+31), format = "%Y-%m-%d")) + # How to fix this so I don't have to keep updating it manually?
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = .9, hjust = .9)) +
  geom_rect(data = food.rects, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2, fill = months)) +
  scale_fill_manual(values = alpha(food.cols, .3)) + 
  coord_cartesian(ylim = c(0,60)) + 
  scale_y_continuous(breaks = seq(0,60,10)) +
  geom_point(data = budget, mapping = aes(dates,cost), col = "blue", alpha = .6) +
  geom_hline(yintercept = mean(budget$cost[-14]), linetype = "dashed", colour = "lightblue") +
  geom_text(data = budget, mapping = aes(dates,cost, label = item),
            size = 2, angle = 45, vjust = -.3, hjust = -.1) +
  geom_bar(data = budget, mapping = aes(x=dates,y=rep(4,nrow(budget))), 
           stat = "identity", fill = "red", width = .6) + 
  theme(panel.background = element_blank(), legend.position = "none",
        panel.grid.major = element_line(colour = "lightgray")) +
  labs(title = sprintf("Money spent eating out solo: %s outings per %s with $%s spent per %s over the last %s %s",
                       food.freq.cost[2],food.freq.cost[4],food.freq.cost[5],
                       food.freq.cost[4],food.freq.cost[1],food.freq.cost[3]), 
       subtitle = "with SD month colors around the mean", 
       x = "", y = "Cost in USD")



# sum(budget[1:13,2])/4 # As of December 12, it was exactly 4 months, at $40.25 a month!
# sum(budget$cost)/6    # As of February 12, I only spend ~$40 a month eating out!
sum(budget$cost)

mean(budget$cost[-14])
hist(budget$cost[-14])
mean(budget$cost[-14])


# Questions:
## How often do I go out to eat? How many times a week? A month? - see below
## CHECK: How much do I typically spend? 

## Which places do I frequent the most?
foodplaces = as.data.frame(as.matrix(sort(as.matrix(summary(budget$item))[,1], 
                                          decreasing = TRUE)))
col.ints = sort(unique(foodplaces$V1))
cols = ifelse(foodplaces$V1 <= col.ints[1], "blue", 
              ifelse(foodplaces$V1 <= col.ints[2], "green", 
                     ifelse(foodplaces$V1 <= col.ints[3], "yellow", "red")))
par(mar=c(9,3,1,2))
barplot(foodplaces$V1, names.arg = rownames(foodplaces), las = 2, 
        col = cols)
par(mar=c(5.1,4.1,4.1,2.1))



# How much I spend a month on eating out - THE EXACT NUMBER OF MONTHS IN THE DENOMINATOR:
monthly.mean = timeDiffandFreq()
sum(budget$cost)/monthly.mean[1] # As of April 22, 2019, I only spend $41.03 bucks a month



############### The over-time differences version of eating out:

n = length(item) # or could be any value less
n = 14
foodindex = 1:n
budget2 = data.frame(item = item[foodindex],
                     cost = cost[foodindex],
                     dates = newdates[foodindex])

# f = seq(as.Date("2018-08-06"),as.Date("2019-04-15"), by = 31) # ONLY THE LAST DATE HERE WILL NEED TO BE ADJUSTED
# f
# 
# begD = vector()
# for (i in 1:length(f)){
#   begD[i] = floor_date(f[i], "month")
# }
# x1 = as.Date(begD, origin = "1970-01-01", format = "%Y-%m-%d")
# x1
# 
# endD = vector()
# for (i in 1:length(f)){
#   endD[i] = ceiling_date(f[i], "month")
# }
# x2 = as.Date(endD, origin = "1970-01-01", format = "%Y-%m-%d")
# x2

food.lb.ub2 = mySD(budget2$cost[-14])

food.rects = data.frame(x1 = x1,
                        x2 = x2,
                        y1 = rep(food.lb.ub2[1],length(f)),
                        y2 = rep(food.lb.ub2[2],length(f)),
                        months = c("D Aug","E Sep","F Oct",
                                   "G Nov","H Dec","I Jan",
                                   "J Feb","K Mar","L April",
                                   "M May","N June")) # How to update this so I don't have to keep updating it manually?

food.cols2 = c("red","goldenrod","red","goldenrod",
               "red","goldenrod","red","goldenrod",
               "red") # And how to update this automatically as well?

# budget2 ggplot (any point in time version): 
ggplot() +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = as.Date(c("2018-07-30","2019-05-06"), format = "%Y-%m-%d")) + # How to fix this so I don't have to keep updating it manually?
  
  theme(axis.text.x = element_text(size = 5, angle = 45, vjust = .9, hjust = .9)) +
  
  geom_rect(data = food.rects, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill = months)) +
  
  scale_fill_manual(values = alpha(food.cols2, .3)) + 
  
  coord_cartesian(ylim = c(0,60)) + 
  scale_y_continuous(breaks = seq(0,60,10)) +
  
  geom_point(data = budget2, mapping = aes(dates,cost), col = "blue", alpha = .6) +
  geom_hline(yintercept = mean(budget2$cost[-14]), linetype = "dashed", colour = "lightblue") +
  geom_text(data = budget2, mapping = aes(dates,cost, label = item),
            size = 2, angle = 45, vjust = -.3, hjust = -.1) +
  geom_bar(data = budget2, mapping = aes(x=dates,y=rep(4,nrow(budget2))), 
           stat = "identity", fill = "red", width = .6) + 
  theme(panel.background = element_blank(), legend.position = "none",
        panel.grid.major = element_line(colour = "lightgray")) +
  labs(title = "Money spent eating out solo", subtitle = "(with 95% CI around mean)", 
       x = "", y = "Cost in USD")






########################################################################################
########################################################################################

# Gas - amounts, price per gallon, and dates, for just the Hyundai

gas      = c(26.00,28.12,27.25,26.74,31.01,32.49,31.04,29.30,28.61,28.35) 
pergall  = c(2.199,2.399,2.309,2.399,2.519,2.519,2.519,2.399,2.409,2.339)
gasdates = as.Date(c("2019-03-09","2019-03-20","2019-04-01","2019-04-08",
                     "2019-04-17","2019-04-26","2019-05-07","2019-05-14",
                     "2019-05-22","2019-06-04"), format = "%Y-%m-%d")

# as.numeric(difftime("2019-04-17","2019-03-09",units = "days"))

dfgas = data.frame(cost = gas,
                   costgal = pergall,
                   dates = gasdates)

gas.freq.cost = timeDiffandFreq(dfgas,
                                3,
                                "weeks",
                                totCostCol = 1)
gas.freq.cost
cat("I've spent ",sum(dfgas$cost)," dollars so far on gas since I started keeping track.")

ggplot(dfgas, aes(x=dates,y=cost)) +
  geom_point(aes(y=cost, colour = "total cost")) +
  geom_point(aes(y=costgal*10, colour = "cost per gallon")) +
  geom_line(aes(y=cost, colour = "total cost")) +
  geom_line(aes(y=costgal*10, colour = "cost per gallon")) +
  scale_y_continuous(breaks = seq(10,40,2),
                     sec.axis = sec_axis(~./10,
                                         breaks = seq(0,4,.2),
                                         name = "cost ($) per gallon")) +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = as.Date(c("2019-03-01",today()), 
                                format = "%Y-%m-%d")) +
  coord_cartesian(ylim = c(10,40)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9, vjust = .9)) +
  labs(title = sprintf("Spending on gas: %s refuels per %s with $%s spent per %s over the last %s %s",
                       gas.freq.cost[2],gas.freq.cost[4],gas.freq.cost[5],gas.freq.cost[4],
                       gas.freq.cost[1],gas.freq.cost[3])
       ,x="",y="Total cost ($) at the pump")

# Time in between each visit to the pump:
gas.t.diffs = vector()
for(i in 1:length(gasdates)-1){
  gas.t.diffs[i] = gasdates[i+1]-gasdates[i]
}
gas.t.diffs

barplot(gas.t.diffs, names.arg = as.character(1:length(gas.t.diffs)),
        main = "Number of days between each visit to the pump",
        xlab = "", ylab = "Days")
range(gas.t.diffs)
mean(gas.t.diffs)
sd(gas.t.diffs)
volatility = (sd(gas.t.diffs)/mean(gas.t.diffs))*100
cat("There is ",round(volatility,1)," percent volatility in the number of days I go in between gas refuels for my car.")





########################################################################################
########################################################################################

### Headaches


# For the month color rectangles:
x = seq(as.Date("2018-06-01"), today(), by = 31); x

firstDs = vector()
for (i in 1:length(x)){
  firstDs[i] = floor_date(x[i],"month")
}
x1 = as.Date(firstDs, origin = "1970-01-01", format = "%Y-%m-%d")

# ceiling function really just gives first day of the month of the following month
lastDs = vector()
for (i in 1:length(x)){
  lastDs[i] = ceiling_date(x[i],"month")
}
x2 = as.Date(lastDs, origin = "1970-01-01", format = "%Y-%m-%d")
l = length(x2)
l

# IT TURNS OUT THIS PART IS UNNECESSARY:
# months = c("A May","B Jun","C Jul","D Aug","E Sep","F Oct",
#            "G Nov","H Dec","I Jan","J Feb","K Mar","L Apr",
#            "M May","N June","O July","P August",
#            "Q September","R October","S November")
# actual.months = months[1:l]

if (l %% 2 != 0){
  actual.months = c(rep(c("blue","green"),floor(l/2)),"blue")
} else {actual.months = rep(c("blue","green"),l/2)}
actual.months
length(actual.months)

mycols2 = actual.months

year.rects = data.frame(x1 = x1,
                        x2 = x2,
                        y1 = rep(0,length(actual.months)),
                        y2 = rep(1,length(actual.months)),
                        months = actual.months)

headache.dates = as.Date(c("2018-06-24","2018-07-07","2018-07-09","2018-07-19",
                           "2018-07-23","2018-07-27","2018-07-29","2018-08-01",
                           "2018-08-10","2018-08-22","2018-08-28","2018-09-14",
                           "2018-09-24","2018-10-06","2018-10-21","2018-11-01",
                           "2018-11-03","2018-12-23","2019-01-12","2019-01-18",
                           "2019-02-06","2019-02-19","2019-02-23","2019-03-04",
                           "2019-03-26","2019-04-01","2019-04-03","2019-04-13",
                           "2019-04-22","2019-04-28","2019-04-30","2019-05-13",
                           "2019-05-18","2019-05-23","2019-05-28","2019-06-06"),
                         format = "%Y-%m-%d")

head.df = data.frame(X = headache.dates,
                     Y = rep(1,length(headache.dates)))

# How many headaches per month (using timeDiffandFreq):
head.freq = timeDiffandFreq(head.df,1,secsminshoursdaysweeks = "months",
                            months = TRUE, totCostCol = 0)
head.freq
cat("I've had ",nrow(head.df)," headaches so far since I started keeping track.")

headache.plot = 
  ggplot() +
  geom_rect(data = year.rects, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2, fill = months)) +
  scale_fill_manual(values = alpha(mycols2, .3)) +
  geom_bar(data = head.df, mapping = aes(X,Y), 
           stat = "identity", width = .8, fill = "red") +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = as.Date(c("2018-06-01",today()+31), format = "%Y-%m-%d")) + 
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9),
        legend.position = "none",
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle(sprintf("Headaches in the last %s %s: %s headaches per %s on average",
                  head.freq[1],head.freq[3],head.freq[2],head.freq[4])) +
  ylab("") + xlab("Time")
headache.plot


# Number of days between each headache:
head.ints = vector()
for (i in 1:length(headache.dates)-1){
  head.ints[i] = headache.dates[i+1]-headache.dates[i]
}
head.ints
headache.dates # Yep, it worked (to calculate the intervals manually/mentally)
cat("On average I have one headache every ",round(mean(head.ints),1)," days.")
range(head.ints)
mean(head.ints)
sd(head.ints)
head.volatility = round((sd(head.ints/mean(head.ints))*100),1)
cat("There is ",head.volatility," percent volatility in the number of days I go between headaches.")

barplot(head.ints,names.arg = as.character(1:length(head.ints)),
        main = "Number of days between each headache (one bar per interval)",
        ylab = "Time in days",
        xlab = "Numbers start with the interval between the first and second headaches")


# Next, a time series histogram of headaches with 1-week-long bins:
num.head = as.numeric(headache.dates) # Note that these values number the days
head.bins = seq(as.numeric(floor_date(headache.dates[1],
                                      unit = "weeks")),
                as.numeric(ceiling_date(headache.dates[length(headache.dates)],
                                        unit = "weeks")),
                7)
head.bins
hist(num.head, breaks = head.bins,
     col = "red",
     main = "Number of headaches within a 7-day period",
     ylab = "Number of headaches per 7-day period",
     xlab = "")

head.counts = hist(num.head, breaks = head.bins, plot = FALSE)$counts
as.Date("2018-06-24")-17706 # (Just checking that it's the Unix origin)
# as.POSIXct(head.bins, origin = "1970-01-01") # Weird. Not what I'm looking for
ggints = as.Date(head.bins, origin = "1970-01-01")
df.head.counts = as.data.frame(head.counts)
df.head.counts$intervals = ggints[1:length(ggints)-1] # Interesting needed step.

# Time series histogram by week:
head.hist = 
  ggplot(df.head.counts, aes(intervals,head.counts)) +
  geom_bar(stat = "identity", fill = "#99CCFF") +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = c(ggints[1]-21,
                          ggints[length(ggints)]) ) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = .9, vjust = .9)) +
  labs(title = "Number of headaches per 7-day period",
       y = "Number of headaches per 7 days", x = "")
head.hist

grid.arrange(head.hist,headache.plot)

head.ratio = length(which(head.counts >= 1))/length(which(head.counts == 0))
cat("I have ",round(head.ratio,1)," weeks with at least one headache for every 1 week without.")

# Time series histogram by month:
head.month.bins0 = seq(as.numeric(floor_date(headache.dates[1],
                                             unit = "months")),
                       as.numeric(ceiling_date(headache.dates[length(headache.dates)]+31,
                                               unit = "months")),
                       30)
head.month.bins1 = seq(as.numeric(floor_date(headache.dates[1],
                                             unit = "months")),
                       as.numeric(ceiling_date(headache.dates[length(headache.dates)]+31,
                                               unit = "months")),
                       31)
hist(num.head, breaks = head.month.bins0, col = rgb(1,0,0,alpha=.5), 
     main="Red: Number of headaches per 30-day period \nBlue: Number of headaches per 31-day period")
hist(num.head, breaks = head.month.bins1,rgb(0,0,1,alpha=.5), add = TRUE)

head.month.counts = hist(num.head,breaks = head.month.bins,plot=FALSE)$counts
ggMonthInts = as.Date(head.month.bins, origin = "1970-01-01")



#################################################################################
#################################################################################

## Dow Jones Industrial Average: https://finance.yahoo.com/quote/%5EDJI/history/

dj = read.csv("/Users/jamescutler/Downloads/^DJI.csv")
head(dj)
plot(dj$Volume, type = "l")
dj$Date = as.Date(dj$Date, format = "%Y-%m-%d")
class(dj$Date)

Y = dj$Close
ylab = which(dj[1,] == Y[1])[1]

yrs = data.frame(x1 = as.Date(c("2016-06-01","2016-12-31","2017-12-31","2018-12-31"),
                              format = "%Y-%m-%d"),
                 x2 = as.Date(c("2016-12-31","2017-12-31","2018-12-31","2019-05-01"),
                              format = "%Y-%m-%d"),
                 y1 = rep(min(Y)-sd(Y),4),
                 y2 = rep(max(Y),4),
                 years = c("2016","2017","2018","2019")) # NOTE! YOU DON'T NEED TO ADD A, B, C, D HERE BECAUSE THE YEARS ALREADY HAVE AN "ALPHABETICAL" ORDER
mycolors = c("goldenrod","red","goldenrod","red")

ggplot() +
  geom_line(data=dj, mapping=aes(x=Date,y=Y)) +
  scale_x_date(breaks = date_breaks("1 months"),
               labels = date_format("%b-%Y"),
               limits = as.Date(c( dj[1,"Date"] , dj[nrow(dj),"Date"] ), 
                                format = "%Y-%m-%d")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = .9, hjust = .9),
        legend.position = "none") +
  geom_rect(data = yrs, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2, fill = years)) +
  scale_fill_manual(values = alpha(mycolors,.3)) +
  labs(title = "Dow Jones Industrial Average",x="",
       y=sprintf("%s",colnames(dj)[ylab]))



### Dow Jones Monthly data from 1989 to 2019:

dm = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Dow1989.2019.csv")
head(dm)
dm$Date = as.Date(dm$Date, format = "%Y-%m-%d")
class(dm$Date)
plot(dm$Volume, type = "l")

Y = dm$Close
ylab = which(dm[1,] == Y[1])[1]

x1 = seq(as.Date("1990-01-01"),as.Date("2010-01-01"), by = 3650); x1
x2 = c(x1[2:3],as.Date("2019-05-01")); x2
decades = data.frame(x1=x1,
                     x2=x2,
                     y1=rep(min(Y)-sd(Y),length(x1)),
                     y2=rep(max(Y),length(x2)), # Don't know what Y is? See 8 lines above! (line 527)
                     decs = c("A 90s","B 2000s","C 2010s")) # NOTE! YOU ***DO*** NEED A, B, C, HERE BECAUSE THESE STRINGS ARE NOT IN ALPHABETICAL ORDER
dec.colors = c("red","goldenrod","red")

ggplot() +
  geom_line(data=dm, mapping = aes(x=Date,y=Y)) + # Don't know what Y is? See 13 lines above! (line 527)
  scale_x_date(breaks = date_breaks("1 years"),
               labels = date_format("%Y"),
               limits = as.Date(c( dm[1,"Date"] , dm[nrow(dm),"Date"] ),
                                format = "%Y-%m-%d")) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9, vjust = .9),
        legend.position = "none") +
  geom_rect(data=decades, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2, fill = decs)) +
  scale_fill_manual(values = alpha(dec.colors,.3)) +
  labs(title = "Dow Jones by month (volume) 1989 to 2019",x="",
       y=sprintf("%s",colnames(dm)[ylab]))





#################################################################################
#################################################################################

# sleep data

sleep = read_xlsx("/Users/jamescutler/Desktop/Data_Course_cutler/sleep_data.xlsx")

sleep = sleep[nrow(sleep):1,]

x1 = sleep$Start
x2 = sleep$End

y1 = rep(0,nrow(sleep))
y2 = rep(1,nrow(sleep))

sleep.rects = data.frame(x1 = x1,
                         x2 = x2,
                         y1 = y1,
                         y2 = y2)
ggplot() +
  geom_rect(data = sleep.rects, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2)) +
  scale_x_datetime(breaks = date_breaks("24 hours"),
                   limits = c(as.POSIXct("2019-02-06 00:00:00"),
                              as.POSIXct("2019-03-02 00:00:00")),
                   labels = date_format("%d-%b %I-%p")) +
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust = 1))





#################################################################################
#################################################################################


A = c(74.78,50.4,44.2,46.55,43.23,39.27,42.83,52.04,39.15,42.42)
B = c(72.4,58.77,67.83,66.67,55.5,38.43,37.46,46.85,37.47,52.74)
mean(A)
mean(B)
plot(A, col = "green", pch = 18, type = "b", ylim = c(30,80))
points(B, col = "red", pch = 18, type = "b")
abline(h = mean(A), col = "green")
abline(h = mean(B), col = "red")

abline(h = 40, lty = 2, col = "gray")

Abar = mean(A[6:length(A)])
Bbar = mean(B[6:length(B)])
points(6:10,rep(Abar,5), col = "green", type = "c")
points(6:10,rep(Bbar,5), col = "red", type = "c")
fA = mean(A[1:5]); fA
fB = mean(B[1:5]); fB
points(1:5,rep(fA,5), col = "green", type = "c")
points(1:5,rep(fB,5), col = "red", type = "c")
abline(v = 5.5, lty = 2)




# Procompsognathid heights from Jurassic Park
?rnorm
compies = round(rnorm(n=29,mean = 33.5, sd = 3))
plot(compies)
hist(compies)
hist(rnorm(50,33.5,3))
plot(density(rnorm(29,33.5,3)))




# Pew liberals and religion:
# https://www.pewforum.org/religious-landscape-study/political-ideology/liberal/
libs = data.frame(percents = c(45,24,8,1,19,3),
                  categories = c("strong belief in God",
                                 "fairly strong in God",
                                 "not too strong in God",
                                 "don't know",
                                 "don't believe",
                                 "don't know if they believe"))
barplot(libs$percents, names.arg = libs$categories, las = 2)
sum(libs$percents[1:3])/sum(libs$percents) # 77% of liberals believe in God
libs$percents[1]/sum(libs$percents[1:3]) # 58% of that 77% are absolutely certain about God



#################################################################################
#################################################################################


# Crime in the US

other = 386.3-sum(5.3,102.8,248.5)
sum(468.9,1745,236.9)
crime = data.frame(rates = c(5.3,102.8,248.5,other,386.3, # violent crimes 
                             468.9,1745,236.9,2450.8),    # theft crimes
                   types = c("Homicide","Robbery","Aggravated assault","Other violent crime",
                             "Total violent crime",
                             "Burglary","Larceny-theft","Motor vehicle theft",
                             "Total property crime"),
                   categories = c(rep(1,5),rep(2,4)))
# crime$rates = factor(crime$rates,
#                      levels = 1:nrow(crime),
#                      labels = crime$types)

ggplot(crime, aes(1:nrow(crime),rates,fill = categories)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 1:nrow(crime),
                     labels = crime$types) +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = .9, vjust = .9),
        legend.position = "none") +
  labs(title = "Crime rates in the US (2016)", x = "", y = "Number of reported crimes per 100,000 population")




########################################

# US amendments:

Ayrs = c(1795,1804,1865,1868,1870,1912,1913,1919,1920,1932,1933,1951,1961,1964,1967,1971,1992)
Anums = paste0(11:27,"th")
Awhich = c("sovereign immunity","vice elected with","slavery abolished",
           "citizenship; due process; equal protection; etc.",
           "all race suffrage","income tax","senators by popular vote",
           "prohibitiion","women's suffrage","date of beg/end terms",
           "repeals prohibition","presidential term limits","DC in electoral college",
           "voting rights and non-tax-payment","unfit for presidency rule (USE IT!!!)",
           "18-yo can vote","congress salary after election")
which(Awhich == "senators by popular vote")
Acolors = c(1,1,2,2,2,2,2,1,2,1,1,2,1,1,2,2,1)
Acolors[which(Acolors == 1)] = 3 # This is the dumb way I found to make the colors distribute how I wanted (otherwise it would have been the opposite of the way I wanted)
Acolors[7] = 4

A = data.frame(years = Ayrs,
               nums = Anums,
               amends = Awhich,
               colors = Acolors)
A$colors = factor(A$colors,
                  levels = c(1,2),
                  labels = c("important, right?","meh"))

ggplot(A, aes(x=Ayrs,y=11:27, col = factor(Acolors))) +
  scale_color_hue(direction = -1) +
  geom_point() +
  geom_text(data = A, mapping = aes(Ayrs,11:27), label = Awhich,
            size = 4, angle = 0, hjust = -.2, vjust = -.1) +
  coord_cartesian(xlim = c(1789,2040)) + 
  scale_y_continuous(breaks = seq(11,27,1)) +
  scale_x_continuous(breaks = seq(1789,2020,10)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = .9, vjust = .9),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray")) +
  labs(title = "US amendments following the original 10 (Amendments 11-27)",
       x="",y="Number of amendment")





##############################################################################
##############################################################################

# Denmark's average annual GDP growth over history
## source: https://eh.net/encyclopedia/an-economic-history-of-denmark/

den = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Denmark_GDP.csv")

class(den$Years)
den$Years = as.character(den$Years)
years = unlist(strsplit(den$Years,"-"))
den$yrsEnd = years[seq(2,length(years),2)]
den$Years = years[seq(1,length(years),2)]
colnames(den)[1] = "yrsBeg"
den$yrsBeg = as.numeric(den$yrsBeg)
den$yrsEnd = as.numeric(den$yrsEnd)

class(den$Total)
den$Total = as.character(den$Total)
class(den$Total)
den$Total = gsub("%","",den$Total)
den$Per.capita = as.character(den$Per.capita)
den$Per.capita = gsub("%","",den$Per.capita)
den$Total = as.numeric(den$Total)
den$Per.capita = as.numeric(den$Per.capita)

den.x = c(den$yrsBeg,2004)
den.x

# Plot: Denmark's average annual GDP growth over history
ggplot(data = den) +
  geom_rect(mapping = aes(xmin=den$yrsBeg,xmax=den$yrsEnd,
                          ymin=rep(0,nrow(den)),ymax=den$Total, fill = "total")) +
  geom_rect(mapping = aes(xmin=den$yrsBeg,xmax=den$yrsEnd,
                          ymin=rep(0,nrow(den)),ymax=den$Per.capita, fill = "per capita")) +
  scale_fill_manual(values = alpha(colour = c("red","orange"),.4)) +
  scale_x_continuous(breaks = den.x) +
  labs(title = "Denmark's annual average GDP growth", subtitle = "1870 to 2004",
       x="", y="GDP growth (percent)", fill = "Type of GDP") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "gray"))








##################################################################################

# Making robots out of carbon coming from the CO2 in the atmosphere:

## We emit 40 billion tons of CO2 a year.

## CO2 is 27.27% carbon by weight. That's 11 billion tons of carbon a year.

## Divide that by 200 lbs: 11 billion tons x 2000 lbs per ton, divided by 200:

((11e9)*2000)/200
# You could make 110 billion robots each with 200 lbs of carbon in them from just
## one year's worth of man-made CO2, if you could suck it back out of the atmosphere.



prettyNum(240000/.09, big.mark = ",", scientific = FALSE)

# Proportion of global population contributed by China, India, US, and Brazil:
sum(1.386e9,1.339e9,327e6,209e6)/7.6e9 









