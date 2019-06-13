# CANCER EPI



# Libraries
install.packages("epitools")
install.packages("epiDisplay") # superseded epicalc, which is now removed from CRAN
library(epitools)
library(epiDisplay)

library(tidyr)




# Week 1


############################## Week 2 ############################## 

# Estimated New Cancer Cases in the US in 2018
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




