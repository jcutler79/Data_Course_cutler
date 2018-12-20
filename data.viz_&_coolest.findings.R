#### Coolest stuff I've been able to see/learn with R:
# (chronological order)

### Table of Contents:
## 19 Nov 2018 - Visualize the distribution of max values from a 100,000 normal distributions




### The distribution of max values from a 100,000 normal distributions:
reps = replicate(1e5,max(rnorm(100)))
hist(reps, breaks = 100) # INSANELY SIMPLE! BUT SOOOOO FREAKING FASCINATING! 
# WHY IS IT SLIGHTLY SKEWED RIGHT??





#####################################################################################
#####################################################################################
#####################################################################################

# libraries:
library(ggplot2)
library(ggpubr) # before and after plots with lines connecting
library(tidyr) # wide to long and vice versa
library(scales)


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
library(scales)
ri.ok = readRDS("/Users/jamescutler/Desktop/SAS/RI_OK_data.rds")
as.matrix(colnames(ri.ok))
ggplot(ri.ok, aes(income, fill = income)) +
  geom_bar(stat = "count") + 
  geom_text() + 
  ggtitle("Counts of people in each income category") + 
  labs(y = "percent", x = "income") +
  guides(fill = FALSE)
  







