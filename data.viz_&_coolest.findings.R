#### Coolest stuff I've been able to see/learn with R:
# (chronological order)

### Table of Contents:
## 19 Nov 2018 - Visualize the distribution of max values from a 100,000 normal distributions
## 07 Feb 2019 - confint() - not that exciting, but it ...




### The distribution of max values from a 100,000 normal distributions:
reps = replicate(1e5,max(rnorm(100)))
hist(reps, breaks = 100) # INSANELY SIMPLE! BUT SOOOOO FREAKING FASCINATING! 
# WHY IS IT SLIGHTLY SKEWED RIGHT??



### confint() 
## It's the function that gives you confidence intervals for coefficients of the 
## predictor variables in linear models. Not super exciting, but I realized I needed
## a function like this when I saw that SAS automatically provides this output in
## summaries of a linear model.



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

## To get ggplot2 version 3.0.0
# install.packages("ggplot2")

library(ggplot2)
library(tidyr) # wide to long and vice versa
library(dplyr) # for the %>% and filter and select ...
library(scales) # for the percent function in geom_text and scale_y_continuous
library(ggpubr) # before and after plots with lines connecting



#### Different kinds of data visualizations:
# (alphabetical order)

### Table of Contents:
## Before and After plots
## Grouped barplots
## Percentages or count labels above bars in barplot




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




### Percentages or count labels above bars in barplot 
## (ALSO, HOW TO GET THE LEGEND FACTORS TO DISPLAY THE FACTOR LABELS RATHER THAN THE NUMBERS THEY CORRESPOND TO)
# YOU NEED THE SCALES LIBRARY JUST TO GET THE PERCENT OBJECT for LABEL(S)
RIOK = readRDS("/Users/jamescutler/Desktop/SAS/RI_OK_data.rds")
as.matrix(colnames(ri.ok))
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


  

data("Titanic")
summary(Titanic)
as.data.frame.table(Titanic)
# THIS IS NOT THE DATASET I'M LOOKING FOR. I DON'T EVEN KNOW IF IT HAS 2200 OBSERVATIONS

# THIS LOOKS LIKE A BETTER ONE:
ti = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Titanic.csv")
head(ti,5)
ggplot(ti, aes(passengerClass,age, fill = survived)) +
  geom_split_violin() +
  facet_grid(~sex)






