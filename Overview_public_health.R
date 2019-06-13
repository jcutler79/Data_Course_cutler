# OVERVIEW OF PUBLIC HEALTH

# Libraries:
library(ggplot2)
library(forcats) # turns out I don't need this
library(dplyr)

# Top 10 causes of death in the US - comparing Turnock and CDC causes:
## CDC - Number of deaths for leading causes of death
### CDC data from: https://www.cdc.gov/nchs/fastats/leading-causes-of-death.htm
## Table 3-2 of Turnock - Actual causes of death in the US and potential contribution to reduction

# VERY HELPFUL RESOURCE FOR REORDERING THE ORDERS ACCORDING TO THEIR 
## CORRESPONDING VALUES (DESCENDING/ASCENDING ORDER OF VALUES RATHER THAN 
## ALPHABETICAL ORDER OF FACTORS -- SEE COMMENTED-OUT CODE BELOW, 
## WITH STR(DF) STUFF): https://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html

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
            size = 3, angle = 60, hjust = .3, vjust = .3) +
  ggtitle("Comparing Turnock's top 9 (preventable?) causes of death to the CDC's") +
  xlab("") + ylab("Number of deaths (in the year 2016?)")

########################################################################################
## FAILED ATTEMPTS AT DOING WHAT ENDED UP WORKING WITH THE ABOVE CODE:
# turnock = data.frame(causes = c("Tobacco","Diet/activity patterns","Alcohol","Microbial agents",
#                                 "Toxic agents","Motor vehicles","Firearms","Sexual behavior",
#                                 "Illicit drug use"),
#                      deaths = c(435000,400000,85000,75000,55000,43000,29000,20000,20000))
# causesT = c("Tobacco","Diet/activity patterns","Alcohol","Microbial agents",
#             "Toxic agents","Motor vehicles","Firearms","Sexual behavior",
#             "Illicit drug use")
# 
# turnock2 = data.frame(causes = c("Tobacco","Diet/activity patterns","Alcohol","Microbial agents",
#                                  "Toxic agents","Motor vehicles","Firearms","Sexual behavior",
#                                  "Illicit drug use","Nothing"),
#                       deaths = c(435000,400000,85000,75000,55000,43000,29000,20000,20000,0))
# causesT2 = c("Tobacco","Diet/activity patterns","Alcohol","Microbial agents",
#              "Toxic agents","Motor vehicles","Firearms","Sexual behavior",
#              "Illicit drug use","Nothing")
# deathsT2 = c(435000,400000,85000,75000,55000,43000,29000,20000,20000,0)
# 
# 
# cdc = data.frame(causes = c("Heart disease","Cancer","Accidents","Chronic lower resp","Stroke",
#                             "Alzheimer's","Diabetes","Influenza and pneumonia","Nephritis/-otic/-osis",
#                             "Suicide"),
#                  deaths = c(635260,598038,161374,154596,142142,116103,80058,51537,50046,44965))
# causesC = c("Heart disease","Cancer","Accidents","Chronic lower resp","Stroke",
#             "Alzheimer's","Diabetes","Influenza and pneumonia","Nephritis/-otic/-osis",
#             "Suicide")
# deathsC = c(635260,598038,161374,154596,142142,116103,80058,51537,50046,44965)
# 
# 
# ggplot(turnock2, aes(causes,deaths)) +
#   geom_bar(stat = "identity")
# 
# str(turnock2)
# str(cdc)
# # sort(turnock2$deaths, decreasing = TRUE)
# 
# turnock2$causes = factor(turnock2$causes, 
#                          levels = turnock2$causes[order(turnock2$deaths)])
# str(turnock2)
# 
# ggplot(turnock2, aes(causes,deaths)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = .9)) +
#   geom_bar(data = cdc, mapping = aes(causes,deaths), stat = "identity", width = .4, position = "dodge")
# 
# 
# 
# cdc$causes = factor(cdc$causes,
#                     levels = cdc$causes[order(cdc$deaths)])
# str(cdc)
# 
# ggplot(cdc, aes(causes,deaths)) +
#   geom_bar(stat = "identity")
# 
# ggplot(turnock2, aes(causes,deaths)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = .9)) +
#   geom_bar(data = cdc, mapping = aes(causes,deaths), stat = "identity", width = .4, position = "dodge")
# 
# 
# 
# ggplot(data = turnock, mapping = aes(1:9,deaths)) +
#   geom_bar(width = .8, stat = "identity", fill = "green") +
#   coord_cartesian(ylim = c(0,600000)) +
#   xlab("") +
#   scale_x_continuous(breaks = 1:9, labels = causes1) +
#   theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = .9))
# 
# ggplot(data = cdc, mapping = aes(1:10,deaths)) +
#   geom_bar(width = .8, stat = "identity", fill = "blue") +
#   xlab("") +
#   scale_x_continuous(breaks = 1:10, labels = causes2) +
#   theme(axis.text.x = element_text(angle = 45, hjust = .9, vjust = .9))
########################################################################################







