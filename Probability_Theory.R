# Probability Theory


### Probability Theory grades:
sum(110,100,100,150)
sum(8.5,10,10,8,10,10,9,10,10,10,8) # not sure what hw#11 is yet but guessing it's an 8
150*.6 # = 90 on the final
sum(103.5,89,92,128)/460 # This would be a B in the class


### Things to learn to do in R:
## 1. 2x2 tables - custom tables with p-values
## 2. Write a function for JMP-like summary statistics for dataframe data (like NC births)
## 3. 

### Budget:

## Brooke's income 2017-2018:
June7 = sum(143)
July7 = sum(34,218,15)
August7 = 0
September7 = sum(218,240)
October7 = sum(267)
November7 = sum(218,267,267,200,227)
December7 = sum(196,266,340)
January8 = sum(266,218,266,34,266,266)
February8 = sum(170,194,218,266,266)
March8 = sum(266,48,266,400,240,240,286)
April8 = sum(237,72,43,237)
May8 = sum(43,49,43,43,237,286,388)
June8 = sum(48)
July8 = sum(286,286,286,237,43,43)
August8 = sum(48,286)
September8 = sum(286,48,286,44,286)
October8 = sum(48,48,44,286,286)
November8 = sum(237,73,286,286)
December8 = sum(286,286)

allmonths = c(June7,July7,August7,September7,October7,November7,December7,January8,
              February8,March8,April8,May8,June8,July8,August8,September8,October8,
              November8,December8)
sum(allmonths)
mean(allmonths)
median(allmonths)
# Take out the two months where she made nothing (August 2017 and June 2018):
mean(c(June7,July7,September7,October7,November7,December7,January8,February8,
       March8,April8,May8,July8,August8,September8,October8,November8,December8))

meses = c("Jun 17", "Jul 17","Aug 17","Sep 17","Oct 17","Nov 17","Dec 17","Jan 18",
          "Feb 18","Mar 18","Apr 18","May 18","Jun 18","Jul 18","Aug 18","Sep 18",
          "Oct 18","Nov 18","Dec 18")
barplot(allmonths, col = "green", names.arg = meses, las = 2)


## Eating out:
suns = seq(as.Date("2018-08-12"),as.Date("2018-12-30"), by = 7)
which(suns == "2018-12-02") # which week number
Week = c(1,4,6,6,7,7,8,9,10,11,11,14,17) 
# 1-SB, 4-JJ, 6-JJ, 6-SB, 7-D, 7-B, 8-FG, 9-D, 10-C, 11-JJ, 11-GG
mydates = suns[Week]; mydates # Just for me to see what they are before entering them in the data frame below
budget = data.frame(item = c("SnB",
                             "Jimmy Johns",
                             "Jimmy Johns",
                             "SnB",
                             "Dominos",
                             "Basil",
                             "Five Guys",
                             "Dominos",
                             "Chipotle",
                             "Jimmy Johns",
                             "Go Go Sushi",
                             "Go Go Sushi",
                             "Dominos for SAS final"),
                    cost = c(15, # SnB
                             14, # Jimmy Johns
                             14, # Jimmy Johns
                             15, # SnB
                             9,  # Dominos
                             9,  # Basil
                             20, # Five Guys
                             9,  # Dominos
                             11, # Chipotle
                             9,  # Jimmy Johns
                             14, # Go Go Sushi
                             14, # Go Go Sushi
                             8),# Dominos for SAS final
                    week = Week,
                    week.dates = suns[Week])
# class(budget$week.dates) # ALREADY A DATE SOMEHOW
library(scales)
library(ggplot2)
ggplot(budget, aes(week.dates,cost)) +
  geom_point(col = "blue", alpha = .6) +
  geom_text(data = budget, mapping = aes(week.dates,cost, label = item),
            size = 2, angle = 45, vjust = -.3, hjust = -.1) +
  scale_x_date(breaks = date_breaks("1 weeks"),
               labels = date_format("%b-%d"),
               limits = as.Date(c("2018-07-01","2018-12-30"), format = "%Y-%m-%d")) +
  coord_cartesian(ylim = c(0,40)) +
  scale_y_continuous(breaks = seq(0,40,5)) +
  theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9)) +
  ggtitle("Money spent on eating out solo") +
  xlab("Time") + ylab("Cost")

sum(budget$cost)/4 # As of December 12, it was exactly 4 months, at $40.25 a month!

########################################################################################
########################################################################################
## WEEK 1 
# Monday 1 October: (DID I START ON HLP 183 OR ON 182?? PRETTY SURE 182, THEN MOVED TO 183 ON WEEK 2)
calls1 = "1111111111111"; c1 = nchar(calls1)
calls2 = "1111111111111111111111111111111111111111111111111111111"; c2 = nchar(calls2)
calls3 = "1111111111111"; c3 = nchar(calls3)
calls4 = "11111111111"; c4 = nchar(calls4)
calls5 = "1"; c5 = nchar(calls5)
end.shift6 = "111"; c6 = nchar(end.shift6)
day1 = 4 # NOT counting the partial

# Tuesday 2 October:
calls6 = "1111111111111111111"; c7 = nchar(calls6)
calls7 = "111111111111111111111111"; c8 = nchar(calls7)
calls8 = "1111111111111111111111111111111"; c9 = nchar(calls8)
end.shift9 = "1111111111111111111111111111111111111111111111111111111111111111111"; c10 = nchar(end.shift9)   
day2 = 3

# Wednesday 3 October:
calls9 = "1"; c11 = nchar(calls9)
calls10 = "11"; c12 = nchar(calls10)
calls11 = "11111111111"; c13 = nchar(calls11)
calls12 = "11111111111111111111111111111"; c14 = nchar(calls12)
end.shift13 = "11111111111111111111111111111111111111111"; c15 = nchar(end.shift13)
day3 = 4

## WEEK 2 (STARTED HLP 183 TODAY)
# Monday 8 October:
calls13 = "1111111111111111111111111111111111111111111111111111111111111111"; c16 = nchar(calls13)
calls14 = "11111111111111111111111"; c17 = nchar(calls14)
calls15 = "111111111111111"; c18 = nchar(calls15)
end.shift16 = "1111111111111111111111111111111111111111111111"; c19 = nchar(end.shift16)
day4 = 3

# Tuesday 9 October:
calls16 = "1111111111"; c20 = nchar(calls16)
calls17 = "1111111111111111111111111111111111111"; c21 = nchar(calls17)
calls18 = "111111111111111111111111111111111111"; c22 = nchar(calls18)
day5 = 3

# Wednesday 10 October:
calls19 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c23 = nchar(calls19)  
calls20 = "111111111111111111111111111111111111111111111111111111111111111111111111111"; c24 = nchar(calls20)
end.shift21 = "1111111111111111"; c25 = nchar(end.shift21)
day6 = 2

# Friday 12 October:
calls21 = "111111111111111111111"; c26 = nchar(calls21)
calls22 = "111111111111111111111111111111111111"; c27 = nchar(calls22)
calls23 = "111111111111111111111111111111111111111111111111111111111111111"; c28 = nchar(calls23)
end.shift24 = "1111111111111111111111111111111111"; c29 = nchar(end.shift24)
day7 = 3

## WEEK 3
# Monday 15 October:
calls24 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c30 = nchar(calls24)   
calls25 = "11111111111111111111111111111111"; c31 = nchar(calls25)
end.shift26 = "11111111111111111111111111111111111111111111111111111111111111"; c32 = nchar(end.shift26)   
day8 = 2

# Tuesday 16 Oct:
calls26 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c33 = nchar(calls26)   
calls27 = "11111111111111111111111111"; c34 = nchar(calls27)
end.shift28 = "111111111111111111111111111111111111111111"; c35 = nchar(end.shift28)
day9 = 2

# Wednesday 17 Oct:
calls28 = "11111111111111"; c36 = nchar(calls28)
calls29 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c37 = nchar(calls29)  
calls30 = "11"; c38 = nchar(calls30)
calls31 = "1111"; c39 = nchar(calls31)
end.shift32 = "1111111111111111111111111111111111111"; c40 = nchar(end.shift32)
day10 = 4

# Saturday 20 Oct:
calls32 = "11111111111111111111"; c41 = nchar(calls32)
end.shift33 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c42 = nchar(end.shift33) 
day11 = 1

## WEEK 4
# Monday 22 Oct:
calls33 = "11111111111111111111111111111111111111111111111111111111111111"; c43 = nchar(calls33) # 62 dials
calls34 = "1111111111111111111111111111111111"; c44 = nchar(calls34) # 34 dials? this is where I messed up
calls35 = "11111"; c45 = nchar(calls35) 
calls36 = "1"; c46 = nchar(calls36)
calls37 = "111111111111111111111"; c47 = nchar(calls37)
end.shift38 = "1111111111111111111111111111111111111111111111111111111"; c48 = nchar(end.shift38)
day12 = 5

# Tuesday 23 Oct:
calls38 = "11111111111111111111111111111111111"; c49 = nchar(calls38)
end.shift39 = "11111111111111111111"; c50 = nchar(end.shift39)
day13 = 1

# Wednesday 24 Oct:
calls39 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c51 = nchar(calls39)
calls40 = "111111111111111111111111111111111111111111111111111111111111111111111111111"; c52 = nchar(calls40)
day14 = 2

# Thursday 25 Oct:
calls41 = "11111111111111111111111111111111111111111111111"; c53 = nchar(calls41) # Got 47 dials in the time between 4:30 and 5:14 (first 45 minutes)
calls42 = "1111111111111111"; c54 = nchar(calls42)
end.shift43 = "111111111111111111111111111111111111111111111111111111"; c55 = nchar(end.shift43)
day15 = 2

## WEEK 5
# Monday 29 Oct: (only 3 hours cuz of test)
calls43 = "1111111111111111111111111111111111111"; c56 = nchar(calls43) 
end.shift44 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c57 = nchar(end.shift44) # from 12 - 2, in the first two hours, I averaged 36.5 dials an hour and got one complete in two hours
day16 = 1

# Tuesday 30 Oct: (worked 7 hours to make up for yesterday)
calls44 = "1111111111111111111111111111111111111111111111111"; c58 = nchar(calls44)
calls45 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c59 = nchar(calls45)
calls46 = "1111111111111111"; c60 = nchar(calls46)
end.shift47 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c61 = nchar(end.shift47)
day17 = 3

# Wednesday 31 Oct: (STARTED HLP 184 TODAY) (worked 4:30 to 7 cuz of Halloween)
calls47 = "1111111"; c62 = nchar(calls47)
end.shift48 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c63 = nchar(end.shift48)
day18 = 1

# Saturday 3 Nov: (6 hours today)
calls48 = "111111111111111111111111111111111111111111111111111"; c64 = nchar(calls48)
calls49 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c65 = nchar(calls49)
calls50 = "1111111111111"; c66 = nchar(calls50)
end.shift51 = "1111111111111111111111111111111111111111111111111111111"; c67 = nchar(end.shift51); c67
day19 = 3

## WEEK 6
# Monday 5 Nov:
calls51 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c68 = nchar(calls51)
calls52 = "11111111111111111111111"; c69 = nchar(calls52)
calls53 = "111111111111111111111111111111111111111"; c70 = nchar(calls53)
end.shift54 = "111"; c71 = nchar(end.shift54)
day20 = 3

# Tuesday 6 Nov: (voting for two hours, so only three hours of work today)
calls54 = "11111111111"; c72 = nchar(calls54)
calls55 = "1111111111111111111111111111111111111111111111"; c73 = nchar(calls55)
calls56 = "1111111111111111111111111111111111111111111111111111111"; c74 = nchar(calls56)
day21 = 3 

# Wednesday 7 Nov:
calls57 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c75 = nchar(calls57)
calls58 = "11111111111111111111111111"; c76 = nchar(calls58)
calls59 = "11111111111111111111111111111111"; c77 = nchar(calls59)
day22 = 3 # or 4, cuz that guy with the spam filter answered on Joe's phone

# Friday 9 Nov: (left way early cuz of the fire)
end.shift60 = "111111111111111111111111111111111"; c78 = nchar(end.shift60)
day23 = 0

# WEEK 7
# Monday 12 Nov:
calls60 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c79 = nchar(calls60)
calls61 = "11111111111111111111"; c80 = nchar(calls61)
calls62 = "1111111111111111111111111111"; c81 = nchar(calls62)
end.shift63 = "111111111111111111111111111111111111111111111111111111"; c82 = nchar(end.shift63)
day24 = 3

# Tuesday 13 Nov:
calls63 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c83 = nchar(calls63); c83
calls64 = "111111111111111111111"; c84 = nchar(calls64)
day25 = 2

# Wednesday 14 Nov:
calls65 = "111111"; c85 = nchar(calls65)
end.shift66 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c86 = nchar(end.shift66)
day26 = 1

# Saturday 17 Nov: (STARTED HLP 185 TODAY)
calls66 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c87 = nchar(calls66)
end.shift67 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c88 = nchar(end.shift67)
day27 = 1


## WEEK 8 (Thanksgiving week, short week)
# Monday 19 Nov
calls67 = "1111111111111111"; c89 = nchar(calls67)
calls68 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c90 = nchar(calls68)
calls69 = "11111111111111111"; c91 = nchar(calls69)
calls70 = "111111111111111111111111111"; c92 = nchar(calls70)
end.shift71 = "11111"; c93 = nchar(end.shift71)
day28 = 4

# Tuesday 20 Nov
calls71 = "111111111111111111111111111111111111111111111111111"; c94 = nchar(calls71)
end.shift72 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c95 = nchar(end.shift72); c95
day29 = 1

# Wednesday 21 Nov
cat(rep("1",25),sep = "")
calls72 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c96 = nchar(calls72)
calls73 = "11111111111111111111111"; c97 = nchar(calls73)
calls74 = "111111111111111111111111111111111111111111111111111111111"; c98 = nchar(calls74)
day30 = 3

## WEEK 9
# Monday 26 Nov
calls75 = "111111111111111111111111111111111111111111111111111111111"; c99 = nchar(calls75)
calls76 = "11111111111111111"; c100 = nchar(calls76)
calls77 = "1111111111111111111"; c101 = nchar(calls77)
end.shift78 = "1111111111111111111111111"; c102 = nchar(end.shift78)
day31 = 3

# Tuesday 27 Nov
calls78 = "111111111111111111111111"; c103 = nchar(calls78)
calls79 = "111111111111111111111111111111111111111111111111111111111"; c104 = nchar(calls79)
calls80 = "1111111111111111111111111111111111"; c105 = nchar(calls80); c105
day32 = 3

# Wednesday 28 Nov
end.shift81 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c106 = nchar(end.shift81); c106
day33 = 0

# Saturday 1 Dec
calls81 = "11111111111111111111111111111111111111"; c107 = nchar(calls81)
end.shift82 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c108 = nchar(end.shift82); c108
day34 = 1


## WEEK 10
# Monday 3 Dec
calls82 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c109 = nchar(calls82); c109 # GOT A FREAKING PARTIAL AT 113
calls83 = "11111111111111111111111"; c110 = nchar(calls83)
end.shift84 = "11111111111111111111111111111111111111111111111111"; c111 = nchar(end.shift84)
day35 = 2

# Tuesday 4 Dec
calls84 = "1"; c112 = nchar(calls84)
calls85 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c113 = nchar(calls85); c113
calls86 = "1111111111111111111111111111111"; c114 = nchar(calls86)
day36 = 3

# Wednesday 5 Dec
calls87 = "1111111111111111111111111111111111111111111111111111111111111111111111"; c115 = nchar(calls87)
calls88 = "11111111111111111111111"; c116 = nchar(calls88)
calls89 = "1111111111111111111111111111111111111111111"; c117 = nchar(calls89)
calls90 = "11111111111111111111111111111"; c118 = nchar(calls90); c118
day37 = 4

# TOOK FRIDAY OFF CUZ OF SAS FINAL


## WEEK 11 
# Monday 10 Dec (STARTED HLP 186 TODAY) (ONLY WORKED THREE HOURS TODAY BECAUSE OF EPI FINAL)
calls91 = "1111111111111111111111111111111111111111111111111111111111111111111111"; c119 = nchar(calls91); c119
end.shift92 = "11111111111111111111111111111111111111111"; c120 = nchar(end.shift92); c120
day38 = 1

# Tuesday 11 Dec (4 hours because of Christmas party downstairs)
calls92 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c121 = nchar(calls92)
calls93 = "111111111111"; c122 = nchar(calls93); c122
day39 = 2

# Wednesday 12 Dec
calls94 = "1111111111111111111111111111111111111111111111111111"; c123 = nchar(calls94)
calls95 = "11111111111111111"; c124 = nchar(calls95)
end.shift96 = "1111111111111111111111111111111111111111111111111111111111111"; c125 = nchar(end.shift96)
day40 = 2


# Friday 14 Dec (WORKED 7 HOURS TODAY)
calls95 = "1"; c126 = nchar(calls95)
calls96 = "111111111111111111111111111111111"; c127 = nchar(calls96)
calls97 = "111111111111111111111111111111111111111111111111111111111111111111111111111111"; c128 = nchar(calls97)
calls98 = "11111111111"; c129 = nchar(calls98)
calls99 = "111111111111111"; c130 = nchar(calls99)
calls100 = "11111"; c131 = nchar(calls100)
day41 = 6


# Saturday 15 Dec 
calls101 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c132 = nchar(calls101)
end.shift102 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c133 = nchar(end.shift102)
day42 = 1

## WEEK 12
# Monday 17 Dec
calls102 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c134 = nchar(calls102)
calls103 = "111111111111111111111111111111111111111111111111111111111111111111111111"; c135 = nchar(calls103)
calls104 = "111111111111111111111"; c136 = nchar(calls104)
sum(c134,c135,c136)/5
day43 = 3

# Tuesday 18 Dec
calls105 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c137 = nchar(calls105)
calls106 = "11"; c138 = nchar(calls106)
calls107 = "1111111111111111111111111111111111111111111111111"; c139 = nchar(calls107)
day44 = 3

# Wednesday 19 Dec (4-hour day)
calls108 = "11111111111111111"; c140 = nchar(calls108)
end.shift109 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c141 = nchar(end.shift109)
day45 = 1

# Friday 21 Dec
calls109 = "111111"; c142 = nchar(calls109); c142
calls110 = "111111111"; c143 = nchar(calls110); c143
calls111 = "1"; c144 = nchar(calls111); c144
end.shift112 = "11111111111111111111111111111"; c145 = nchar(end.shift112); c145
day46 = 3


## WEEK 13 (short week - New Year's)
# Wednesday 2 Jan 2019 (STARTED HLP 187 TODAY) (HAD A SHORT DAY - 2.5 HOURS - CUZ OF ICE)
calls112 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c146 = nchar(calls112)
end.shift113 = "1111111111111111111"; c147 = nchar(end.shift113); c147
day47 = 1

# Friday 4 Jan 2019 (LONG DAY - 7 HOURS - CUZ OF MISSING TWO HOURS ON FRIDAY BEFORE BREAK)
calls113 = "111111111111111111111111111111"; c148 = nchar(calls113)
calls114 = "1111111111111111"; c149 = nchar(calls114)
calls115 = "1111111111111111111111111111111111111"; c150 = nchar(calls115)
calls116 = "1111111111111111111111111111111111111"; c151 = nchar(calls116)
calls117 = "11111111111111111111111111111111"; c152 = nchar(calls117)
end.shift118 = "111111111111111"; c153 = nchar(end.shift118)
day48 = 5



## WEEK 14 (NEW SCHEDULE STARTED THIS WEEK)
# Monday 7 Jan 2019
calls118 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c154 = nchar(calls118); c154
calls119 = "111111111111111111111111111111111111111"; c155 = nchar(calls119)
Aend.shift120 = "11111111111111111111111111111111111111111111111111111"; c156 = nchar(Aend.shift120)
day49 = 2

# Tuesday 8 Jan
Bend.shift120 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c157 = nchar(Bend.shift120)
day50 = 0
### SINCE I HAD TWO END.SHIFT120'S, I NEED TO COMBINE THEM MANUALLY :(
end.shift120 = paste0(Aend.shift120,Bend.shift120)

# Wednesday 9 Jan
calls120 = "111111111111111111111111111111111111111111111111111111111111111"; c158 = nchar(calls120)
calls121 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c159 = nchar(calls121)
end.shift122 = "11111111111"; c160 = nchar(end.shift122); c160
day51 = 2

# Saturday 12 Jan
calls122 = "111111111111111111111111111111111111111111111111111111"; c161 = nchar(calls122)
calls123 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c162 = nchar(calls123)
end.shift124 = "111111111111111111111111111111111111111111"; c163 = nchar(end.shift124)
day52 = 2


## WEEK 15
# Monday 14 Jan
calls124 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c164 = nchar(calls124)
calls125 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c165 = nchar(calls125)
end.shift126 = "1111111111111111"; c166 = nchar(end.shift126)
day53 = 2

# Tuesday 15 Jan
calls126 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c167 = nchar(calls126)
calls127 = "111111111111111111111111111"; c168 = nchar(calls127)
calls128 = "11"; c169 = nchar(calls128)
end.shift129 = "1111111111111111"; c170 = nchar(end.shift129); c170
day54 = 3

# Wednesday 16 Jan (MLK PRESENTATION TODAY - SHORT DAY)
calls129 = "1111111111111111111111111111111"; c171 = nchar(calls129)
calls130 = "11"; c172 = nchar(calls130)
end.shift131 = "1111111111111111111111111111111111111111111111111111111111111111111111111111"; c173 = nchar(end.shift131)
day55 = 2

# Friday 18 Jan
# THIS CALLS131 IS WRONG. I FORGOT TO STOP TALLYING AFTER I GOT A SURVEY. I PROBABLY GOT THE SURVEY AFTER LIKE 90 or 100 DIALS
calls131 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c174 = nchar(calls131); c174
end.shift132 = "1111111111111111111111111111111111111111111111111111"; c175 = nchar(end.shift132)
day56 = 1


## WEEK 16
# Tuesday 22 Jan (STARTED HLP 188 TODAY) (REALLY SHORT WEEK CUZ OF MLK, AND TRIP!)
calls132 = "1111111111111111111111111111111"; c176 = nchar(calls132)
calls133 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c177 = nchar(calls133)
end.shift134 = "11111111111"; c178 = nchar(end.shift134)
day57 = 2

# Wednesday 23 Jan
calls134 = "111111111111111111111111111111111111"; c179 = nchar(calls134)
end.shift135 = "111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c180 = nchar(end.shift135); c180
day58 = 1


## WEEK 17 
# Monday 28 Jan
calls135 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111"; c181 = nchar(calls135)
calls136 = "111111"; c182 = nchar(calls136)
calls137 = "111111111"; c183 = nchar(calls137)
calls138 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c184 = nchar(calls138)
day59 = 4

# Tuesday 29 Jan
calls139 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c185 = nchar(calls139)
end.shift140 = "1111111111111111111111111111111111111111111111111111111111111"; c186 = nchar(end.shift140)
day60 = 1

# Wednesday 30 Jan 
calls140 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c187 = nchar(calls140)
end.shift141 = "1111111111111111111111111111111111111111111111111111111111111"; c188 = nchar(end.shift141)
day61 = 1

# Friday 1 Feb
calls141 = "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c189 = nchar(calls141)
calls142 = "11111111111111111111111111111111111111111111111111111111"; c190 = nchar(calls142)
end.shift143 = "11111111111111111111111111111111111111111111111111111111111111111111"; c191 = nchar(end.shift143)
day62 = 2


## WEEK 18
# Monday 4 Feb
calls143 = "11111111111111111111111111111111111111"; c192 = nchar(calls143)
calls144 = "11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"; c193 = nchar(calls144)
calls145 = "1111111111111111111111111"; c193 = nchar(calls145)
day63 = 3


week1 = sum(day1,day2,day3)
week2 = sum(day4,day5,day6,day7)
week3 = sum(day8,day9,day10,day11)
week4 = sum(day12,day13,day14,day15)
week5 = sum(day16,day17,day18,day19)
week6 = sum(day20,day21,day22,day23)
week7 = sum(day24,day25,day26,day27)
week8 = sum(day28,day29,day30) # Short week - Thanksgiving
week9 = sum(day31,day32,day33,day34)
week10= sum(day35,day36,day37) # Short week - finals
week11= sum(day38,day39,day40,day41,day42) # Long week (cuz added the Friday missed in the previous week)
week12= sum(day43,day44,day45,day46)
week13= sum(day47,day48) # short week for New Year's
week14= sum(day49,day50,day51,day52)
week15= sum(day53,day54,day55,day56)
week16= sum(day57,day58) # super short week for MLK and Stephen's homecoming
week17= sum(day59,day60,day61,day62)
# Week 1:  182
# Week 2:  day4/calls13 - Monday 8 Oct -      started 183
# Week 5:  day18/calls47 - Wednesday 31 Oct - started 184
# Week 7:  day27/calls66 - Saturday 17 Nov -  started 185
# Week 11: day38/calls91 - Monday 10 Dec -    started 186
# Week 13: day47/calls112 - Wednesday    -    started 187
# Week 16: day57/calls132 - Tuesday 22 Jan -  started 188

#################################################################################################
############################ ******* DATA VIZ NUMERO UNO ******* ################################
############################ DIALS AND MEAN COMPLETES BY WEEKDAY ################################

library(ggplot2)
library(forcats)

#### I WANT A DATAFRAME WITH 5 COLUMNS, SO I CAN PRINT A GGPLOT WITH TWO Y AXES:
### ONE FOR # OF DIALS TO GET A COMPLETE BY WEEKDAY (blue boxplots)
### THE OTHER FOR MEAN COMPLETES BY WEEKDAY (red points)
### THE 5 COLUMNS OF THE DATAFRAME WILL BE:
## 1) WEEK (NUMBERED)
## 2) DAY (MON, TUES, WED, FRI, SAT)
## 3) DIALS (FROM ALL THE c-n VARIABLES)
## 4) A COLUMN TO VERIFY THAT THE # OF DIALS MATCH UP WITH THE RIGHT c-k VARIABLE
## 5) A MEANS COLUMN FOR MEAN COMPLETES BY WEEKDAY

### STEP 1: CONVERT SEQUENCE OF c1:n STRINGS INTO A VECTOR OF VARIABLE NAMES FOR COLUMN 3:
n1 = 179 # <---------------------------  ######## *****DECISION***** ######## (the number of c-n calls)
mystring = paste(paste0("c",1:n1), collapse = ","); mystring
stringvec = unlist(strsplit(mystring,split = ",")); stringvec
varvec = vector()
for (i in 1:length(stringvec)){
  varvec[i] = eval(parse(text = stringvec[i])) # WORKS, BUT CONVERTS THE # OF DIALS INTO FACTORS SO NOW THEY NEED TO BE CONVERTED TO NUMERICS
}

### STEP 2: ASSIGN THE day-k VARIABLES TO THEIR WEEKDAYS AND AVERAGE THEIR COMPLETES FOR COLUMN 5:
Monmean = mean(c(day1,day4,day8,day12,day16,day20,day24,day28,day31,day35,day38,day43,day49,day53)) # <----- NEEDS MORE DAYS ADDED TO IT WEEK BY WEEK
Tuemean = mean(c(day2,day5,day9,day13,day17,day21,day25,day29,day32,day36,day39,day44,day50,day54,day57)) # <----- NEEDS MORE DAYS ADDED TO IT WEEK BY WEEK
Wedmean = mean(c(day3,day6,day10,day14,day18,day22,day26,day30,day33,day37,day40,day45,day47,day51,day55,day58)) # <---- NEEDS MORE DAYS ADDED TO IT WEEK BY WEEK
# Thurmean = "just day15"
Frimean = mean(c(day7,day23,day41,day46,day48,day56)) # <--------------------- NEEDS MORE DAYS ADDED TO IT WEEK BY WEEK
Satmean = mean(c(day11,day19,day27,day34,day42,day52)) # <--------------------- NEEDS MORE DAYS ADDED TO IT WEEK BY WEEK
m.cmplts.byweekday = c(rep(Monmean,day1+2),rep(Tuemean,day2+1),rep(Wedmean,day3+1),
                       rep(Monmean,day4+1),rep(Tuemean,day5),rep(Wedmean,day6+1),rep(Frimean,day7+1),
                       rep(Monmean,day8+1),rep(Tuemean,day9+1),rep(Wedmean,day10+1),rep(Satmean,day11+1),
                       rep(Monmean,day12+1),rep(Tuemean,day13+1),rep(Wedmean,day14),rep(day15,day15+1),
                       rep(Monmean,day16+1),rep(Tuemean,day17+1),rep(Wedmean,day18+1),rep(Satmean,day19+1),
                       rep(Monmean,day20+1),rep(Tuemean,day21), rep(Wedmean,day22),  rep(Frimean,day23+1),
                       rep(Monmean,day24+1),rep(Tuemean,day25), rep(Wedmean,day26+1),rep(Satmean,day27+1),
                       rep(Monmean,day28+1),rep(Tuemean,day29+1),rep(Wedmean,day30),
                       rep(Monmean,day31+1),rep(Tuemean,day32),  rep(Wedmean,day33+1),rep(Satmean,day34+1),
                       rep(Monmean,day35+1),rep(Tuemean,day36),  rep(Wedmean,day37),
                       rep(Monmean,day38+1),rep(Tuemean,day39),  rep(Wedmean,day40+1),rep(Frimean,day41),rep(Satmean,day42+1),
                       rep(Monmean,day43),  rep(Tuemean,day44),  rep(Wedmean,day45+1),rep(Frimean,day46+1),
                                                                 rep(Wedmean,day47+1),rep(Frimean,day48),
                       rep(Monmean,day49+1),rep(Tuemean,day50+1),rep(Wedmean,day51+1),rep(Satmean,day52+1),
                       rep(Monmean,day53+1),rep(Tuemean,day54+1),rep(Wedmean,day55+1),rep(Frimean,day56+1),
                                            rep(Tuemean,day57+1),rep(Wedmean,day58+1))# <--- NEEDS UPDATING! FREAK!

### STEP 3: THE WEEK COLUMN:
week = rep(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), # <---------------------- NEEDS MORE #s ADDED TO IT WEEK BY WEEK
           c(sum(day1+2,day2+1,day3+1),
             sum(day4+1,day5,day6+1,day7+1),
             sum(day8+1,day9+1,day10+1,day11+1),
             sum(day12+1,day13+1,day14,day15+1),
             sum(day16+1,day17+1,day18+1,day19+1),
             sum(day20+1,day21,day22,day23+1),
             sum(day24+1,day25,day26+1,day27+1),
             sum(day28+1,day29+1,day30),
             sum(day31+1,day32,day33+1,day34+1),
             sum(day35+1,day36,day37),
             sum(day38+1,day39,day40+1,day41,day42+1),
             sum(day43,day44,day45+1,day46+1),
             sum(day47+1,day48), # New Year's week
             sum(day49+1,day50+1,day51+1,day52+1),
             sum(day53+1,day54+1,day55+1,day56+1),
             sum(day57+1,day58+1)) ) # <------- NEEDS MORE DAYS ADDED TO IT WEEK BY WEEK

### STEP 4: THE DAY COLUMN:
day = c(rep("Mon",day1+2), rep("Tues",day2+1), rep("Wed",day3+1),
        rep("Mon",day4+1), rep("Tues",day5),   rep("Wed",day6+1), rep("Fri",day7+1),
        rep("Mon",day8+1), rep("Tues",day9+1), rep("Wed",day10+1),rep("Sat",day11+1),
        rep("Mon",day12+1),rep("Tues",day13+1),rep("Wed",day14),  rep("Thur",day15+1),
        rep("Mon",day16+1),rep("Tues",day17+1),rep("Wed",day18+1),rep("Sat",day19+1),
        rep("Mon",day20+1),rep("Tues",day21),  rep("Wed",day22),  rep("Fri",day23+1),
        rep("Mon",day24+1),rep("Tues",day25),  rep("Wed",day26+1),rep("Sat",day27+1),
        rep("Mon",day28+1),rep("Tues",day29+1),rep("Wed",day30), 
        rep("Mon",day31+1),rep("Tues",day32),  rep("Wed",day33+1),rep("Sat",day34+1),
        rep("Mon",day35+1),rep("Tues",day36),  rep("Wed",day37),
        rep("Mon",day38+1),rep("Tues",day39),  rep("Wed",day40+1),rep("Fri",day41),rep("Sat",day42+1),
        rep("Mon",day43),  rep("Tues",day44),  rep("Wed",day45+1),rep("Fri",day46+1),
                                               rep("Wed",day47+1),rep("Fri",day48),
        rep("Mon",day49+1),rep("Tues",day50+1),rep("Wed",day51+1),rep("Sat",day52+1),
        rep("Mon",day53+1),rep("Tues",day54+1),rep("Wed",day55+1),rep("Fri",day56+1),
        rep("Tues",day57+1),rep("Wed",day58+1)) # <-------- NEEDS MORE DAYS ADDED TO IT WEEK BY WEEK

### ALL THREE VARIABLES--M.CMPLTS, WEEK, AND DAY--NEED TO HAVE THE SAME LENGTH DUH:
length(m.cmplts.byweekday) 
length(week)
length(day)

### NOW PUTTING TOGETHER THE DATAFRAME IS STRAIGHTFORWARD:
survey = data.frame(week = week,
                    day = day,
                    dials = varvec,
                    verify = stringvec,
                    weekday.means = m.cmplts.byweekday)
# plot(survey$dials ~ survey$day) # THIS IS LITERALLY THE WIERDEST THING I'VE EVER SEEN. 
# WHAT KIND OF A PLOT IS THIS? IT'S BECAUSE THE #s OF DIALS ARE FACTORS!
# survey$dials = as.numeric(survey$dials)
# plot(survey$dials ~ survey$day) # THAT'S BETTER

### A DATAFRAME JUST FOR MON, TUES, AND WED:
# mtw = c("Mon","Tues","Wed")
# MTW = survey[which(survey$day %in% mtw),] # AWESOME I LOVE HOW I JUST KNEW THIS WAS GOING TO WORK.
# plot(MTW$dials ~ MTW$day)

### NOW FOR THE PLOT WITH TWO Y AXES:
# (AND DAYS IN PROPER ORDER RATHER THAN ALPHABETICAL with forcats):
# library(forcats)
ordered.days = c("Mon","Tues","Wed","Thur","Fri","Sat")
survey = fct_relevel(day, ordered.days) %>% # THIS PART IS SOME FUNKY NEW CODING STYLE I'M NOT VERY FAMILIAR WITH ...
  data.frame(week = week,
             day = .,
             dials = varvec,
             verify = stringvec,
             weekday.means = m.cmplts.byweekday) # ... BUT IT WORKED!!!!!!! THE DAYS ARE IN ORDER!!!

ggplot(survey, aes(x = day)) +
  geom_boxplot(aes(y = dials, colour = "dials")) +
  geom_point(aes(y = weekday.means*30, colour = "mean completes")) +
  scale_y_continuous(breaks = seq(0,300,25), 
                     sec.axis = sec_axis(~./30, breaks = seq(0,4,1), name = "mean completes")) + 
  geom_hline(yintercept = c(60,90), linetype = "dashed", colour = "red") +
  scale_color_manual(values = c("blue","red")) +
  xlab("days of the week") + ylab("# of dials to get a complete") +
  theme(legend.position = c(.65,.9))

# A barplot of weekly complete totals:
week.totals = vector()
for (i in 1:length(unique(survey$week))){
  week.totals[i] = eval(parse(text = paste0("week",i)))
}

s.weeks = data.frame(week = 1:length(unique(survey$week)),
                     w.totals = week.totals)

wks = 1:length(unique(survey$week)); wks
# paste(shQuote(wks), collapse = ",") # SOOOO CLOSE TO BEING WHAT I NEED.
# dput(as.character(wks)) # BINGO!!! DANG THIS FUNCTION ROCKS.

ggplot(s.weeks, aes(week,w.totals)) +
  geom_bar(stat = "identity", fill = "orange") +
  scale_x_continuous(breaks = 1:length(unique(survey$week)),
                     labels = dput(as.character(wks))) +
  scale_y_continuous(breaks = seq(0,15,3)) +
  geom_hline(yintercept = 8, linetype = "dashed", colour = "red") +
  xlab("Week") + ylab("Completes") +
  ggtitle("Total completes per week") +
  theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
#####################################################################################################
############################## ******* DATA VIZ NUMERO DOS ******* ##################################

### COMBINING END.SHIFTS AND SUBSEQUENT CALLS:
# calls6 = paste0(end.shift6,calls6); calls6; nchar(calls6)
# calls9 = paste0(end.shift9,calls9); calls9; nchar(calls9)
# calls13 = paste0(end.shift13,calls13); calls13; nchar(calls13)
# calls16 = paste0(end.shift16,calls16); calls16; nchar(calls16)
# calls21 = paste0(end.shift21,calls21); calls21; nchar(calls21)
# The nchars: 22 68 105 56 37
##################################################################
################## HOW TO AUTOMATE THE PROCESS ###################
myindex = c(6,9,13,16,21,24,26,28,32,33,38,39,43,44,47,48,51,54,60,63,66,67,
            71,72,78,81,82,84,92,96,102,109,112,113,120,122,124,126,129,131,
            132,134,135) # <-----  ######## *****DECISION***** ######## 
newcalls = vector()
for (i in 1:length(myindex)){
  newcalls[i] = paste0(eval(parse(text = paste0("end.shift",as.character(myindex[i])) )), 
                       eval(parse(text = paste0("calls",as.character(myindex[i])) )) ) # pasting the *variables* together means pasting their strings together into one bigger string
}
for (i in 1:length(newcalls)){
  print(nchar(newcalls[i])) # VERIFY THAT THE FOR LOOP ABOVE WORKS
}

n = length(newcalls); n                                    # STEP 1: needed for step 3
lhs = paste("calls", myindex[1:length(myindex)], sep = "") # STEP 2: paste variable names
rhs = paste("newcalls[",1:n,"]", sep = "")                 # STEP 3: **THE KEY** - paste your vector-indexed variable assignments; I HAD NO IDEA YOU COULD PASTE THINGS THIS WAY, WHICH IS SOOOOO FREAKING CONVENIENT
eq = paste(paste(lhs,rhs, sep = "="), collapse = ";")      # STEP 4: THE MOST INGENIOUS, ABSTRUSE STEP - TWO STEPS IN ONE; it is also really convenient that R understands double pasting this way
eval(parse(text = eq))                                     # STEP 5: convert from string to real variables
# nchar(calls21)                                           # I CAN'T BELIEVE IT WORKED!!
######################

n2 = 134 # <---------------------------------------  ######## *****DECISION***** ######## (the number of completes duh)
callstotal = vector()
for (i in 1:n2){
  callstotal[i] = nchar(eval(parse(text = paste0("calls",i) )) ) # COOL! IT WORKS!
}
callstotal
events = rep("",n2); events
events[c(13,47,66,91,112,132)] = c("HLP 183","HLP 184","HLP 185","HLP 186","HLP 187","HLP 188"); events
df = data.frame(X = 1:length(callstotal),
                Y = callstotal,
                stuff = events)
dfsort = data.frame(X = 1:length(callstotal),
                    Y = sort(callstotal))

# HLP182 = 12; HLP182
# HLP183 = 46-12; HLP183
# HLP184 = 65-46; HLP184
# HLP185 = 90-65; HLP185
# HLP186 = 109-90; HLP186

by10.20or50 = function(x,byWhat) {
  multiplier = x%/%byWhat
  top = multiplier*byWhat+byWhat
  return(top)
} 

ggplot(df, aes(X,Y)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(data = df, mapping = aes(X,Y, label = stuff),
            size = 5, angle = 45, hjust = -.1, vjust = -.2) + 
  ggtitle("HLP # of dials") + xlab("(in chronological order)") +
  ylab("# of dials to get a complete") +
  scale_y_continuous(breaks = seq(0,350,50)) +
  coord_cartesian(ylim = c(0,320)) +
  scale_x_continuous(breaks = seq(0,by10.20or50(length(callstotal),10),10))
ggplot(dfsort, aes(X,Y)) +
  geom_bar(stat = "identity", fill = "red") +
  ggtitle("HLP # of dials") + xlab("(in ascending order)") +
  ylab("# of dials to get a complete") +
  scale_y_continuous(breaks = seq(0,350,50))

hist(callstotal, breaks = 30)
## FIVE (actually 6) NUMBER SUMMARY COLUMN FORMAT WITH MAX ON TOP:
FNS.plus.other = function(vector.of.data.pts){
  m = mean(vector.of.data.pts)
  std = sd(vector.of.data.pts)
  sm = summary(vector.of.data.pts)
  fns = as.matrix(sm)
  fnsflip = as.matrix(fns[nrow(fns):1,])
  IQR = fns[5,] - fns[2,]
  print("Five number summary (plus mean):")
  print(fnsflip)
  print(sprintf("IQR: %.3f",IQR))
  print(sprintf("Standard deviation (and 2xSD): %.3f ... %.3f",std,2*std))
}
FNS.plus.other(callstotal)
sort(callstotal)
new.fns = as.matrix(summary(callstotal)); new.fns
ub = new.fns[5,] + 1.5*(new.fns[5,] - new.fns[2,]); ub[[1]]
# Good to have boxplot after five number summary, so I can compare the two:
boxplot(callstotal, xlab = "me", ylab = "# of dials to get a complete", 
        main = "HLP survey")
points(mean(callstotal), col = "red", pch = 18)
abline(h = ub, col = "gray") # THE CUTOFF PAST WHICH DATA POINTS BECOME OUTLIERS

## So, there's ...
# 1 dials and completes per day (composite plot)
# 2 weekly completes totals (barplot)
# 3 dials in chronological order (barplot)
# 4 dials in ascending order (barplot)
# 5 histogram of dials
# 6 single boxplot of dials
# plus five-number summary, mean, sd, IQR




################################################################################################
################################################################################################
journ = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Journals82.csv", header = FALSE)
################################################################################################
################################################################################################

numerator = .967*.0038; numerator
denominator = (1-.0038)*.015 + .0038*.967; denominator
numerator/denominator



right = vector()
for (i in 1:100000){
  guess = sample(1:4,1)
  right[i] = ifelse(guess == 1,1,0)
}
sum(right)/length(right)

right = vector()
for (i in 1:100000){
  oneortwo = sample(1:2,1) # Offers two possibilities, which are described in the if else set up below:
  if (oneortwo == 2) { # In this possibility, there are two right answers
    correct = sample(1:4,2,replace = FALSE)
    guess = sample(1:4,1)
    right[i] = ifelse(guess %in% correct,1,0)
  }else{ # In this possibility, there is only one right answer
    correct = sample(1:4,1)
    guess = sample(1:4,1)
    right[i] = ifelse(guess == correct,1,0)
  }
} # In both cases, the question is answered at random (there is no strategy here to always choose one of the "duplicated" answers)
sum(right)/length(right)
# I think this code is sound because in this scenario the right answer could be one of the duplicates, 
# or it could be one of the other two answers. 50% of the time it will be the case that one of the
# duplicates is the correct answer (so there will be two choices that will get you a point). The
# other 50% of the time there will be only one of the four choices that is the correct one. This
# is because A, B, C, and D each have a 25% chance of being right. 
## I'm guessing this shows that if you choose answers randomly, you'll be right ~37% of the time. But
## what if you always guess one of the duplicated answers? Would you get ~63% of the answers right?

win = vector()
for (i in 1:10000){
  roll = sample(1:6,4,replace = TRUE)
  win[i] = ifelse(sum(ifelse(roll == 6,1,0)) > 0,1,0)
}
prob.win = sum(win)/10000



roll = sample(1:6,4,replace = TRUE)

win[i] = ifelse(sum(ifelse(roll == 6,1,0)) > 0,1,0)


########################

outcomes = vector()
X = 12
for (i in 1:X-1){
  outcomes[i] = ncol(combn(X,i))
  print(ncol(combn(X,i)))
}
outcomes[X-(X-3)]
plot(outcomes)



for (i in 10:20){
  print(ncol(combn(i,2))) # add the number to its combination and you get the combination of the next number   
}

for (i in 10:20){
  print(ncol(combn(i,3))) # So you're always adding the previous number to the one you get here, and that tells you what the next higher number's combination will be  
}
165+55
220+66

func = function(x) x^3
integrate(func, lower = 1, upper = 5)
.25*5^4 - .25


library(combinat)
ncol(combn(11,6))
ncol(combn(7,4))
6^6
####################################################################################################
best.jobs = data.frame(jobs = c("PhD statistics",
                                "Masters biostatistics",
                                "PhD computer science",
                                "Masters human computer interaction",
                                "PhD physics",
                                "Juris Doctor",
                                "Masters telecom engineering",
                                "Masters applied math",
                                "Masters statistics",
                                "Masters engineering",
                                "Masters computer science",
                                "Masters software engineering",
                                "PhD economics",
                                "MBA",
                                "Masters information science"),
                       salaries = c(131700,
                                    113400,
                                    144800,
                                    115200,
                                    132400,
                                    138200,
                                    119100,
                                    121900,
                                    109700,
                                    117200,
                                    122100,
                                    121300,
                                    122500,
                                    113000,
                                    101800))
worst.jobs = data.frame(jobs = c("Masters interior design",
                                 "Masters educational administration",
                                 "Masters early childhood education",
                                 "Masters criminal justice",
                                 "Masters reading and literacy",
                                 "PhD educational leadership",
                                 "Masters health administration",
                                 "Masters studio art",
                                 "Masters construction management",
                                 "Masters fine arts",
                                 "Masters divinity",
                                 "Masters educational leadership",
                                 "Masters social work",
                                 "Masters leadership",
                                 "Masters curriculum and instruction"),
                        salaries = c(69400,
                                     77100,
                                     48100,
                                     60500,
                                     52300,
                                     88500,
                                     73300,
                                     51300,
                                     99600,
                                     55900,
                                     52100,
                                     72600,
                                     59400,
                                     81600,
                                     58200))
library(ggplot2)
ggplot(best.jobs, aes(1:length(jobs),salaries)) +
  geom_point(col = "green") +
  geom_point(data = worst.jobs, mapping = aes(1:length(jobs),salaries), col = "red") +
  geom_text(data = best.jobs, mapping = aes(1:length(jobs),salaries, label = jobs), size = 3, angle = 20, vjust = -.3, hjust = -.1) +
  geom_text(data = worst.jobs, mapping = aes(1:length(jobs),salaries, label = jobs), size = 3, angle = 20, vjust = -.3, hjust = -.1) +
  scale_y_continuous(breaks = seq(0,150000,10000)) + 
  coord_cartesian(xlim = c(0,18), ylim = c(40000,160000))
#########################################################################################################

permn(6) # Obviously you can't actually do permutations with this.

# MY PERMUTATION FUNCTION:
mypermutationfunc = function(n,r){
  theanswer = factorial(n)/factorial(n-r)
  print(prettyNum(theanswer, big.mark = ",", scientific = FALSE))
}

# MY FUNCTION FOR GETTING THE PERMUTATION AND THE COMBINATION AT THE SAME TIME:
getpermn.and.combn = function(n,r){
  thepermn = prettyNum(factorial(n)/factorial(n-r), big.mark = ",", scientific = FALSE)
  thecombn = prettyNum(factorial(n)/(factorial(r)*factorial(n-r)), big.mark = ",", scientific = FALSE)
  print(sprintf("The permutation is: %s",thepermn))
  print(sprintf("The combination is: %s",thecombn))
}
getpermn.and.combn(6,2)
getpermn.and.combn(50,2)

ncol(combn(5,3))
factorial(3)
mypermutationfunc(3,3)

smallvector = c(4,3,5)
factorial(smallvector) # IT WORKS!

factorial(10)/36

# MY FUNCTION FOR GETTING THE NUMBER OF POSSIBLE ORDERED SETS OF AN UNORDERED SET:
numofOfU = function(k,vector.of.Ms){
  thenumerator = factorial(k)
  facted.vec = factorial(vector.of.Ms)
  thedenominator = prod(facted.vec)
  theanswer = prettyNum(thenumerator/thedenominator, big.mark = ",", scientific = FALSE)
  print(theanswer)
}
numofOfU(10,c(3,3))


# MY BINOMIAL FORMULA FOR ANSWERING "AT LEAST" QUESTIONS:
bin.vec = vector()
for (i in 1:10){
  bin.vec[i] = ncol(combn(10,i))*((1/5)^i)*(4/5)^(10-i)
}
bin.vec = na.omit(bin.vec)
sum(bin.vec)


over100 = 500000
decade.starting = 2010
for (i in 1:10){
  print(c(prettyNum(decade.starting, big.mark = "", scientific = FALSE), prettyNum(over100, big.mark = ",", scientific = FALSE)))
  over100 = over100*2
  decade.starting = decade.starting + 10
}

at.least.10 = vector()
for (i in 10:20){
  at.least.10[i] = (factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i)
}
sum(at.least.10)

thesum = 0
for (i in 10:20){
  print((factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i))
  thesum = thesum + (factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i)
  print(c(i,thesum))
}


.009922275 + .00300675

ncol(combn(20,10))
ncol(combn(20,11))
for (i in 10:20){
  at.least.10[i] = ncol(combn(20,i))
}
for (i in 10:20){
  print(c(i,ncol(combn(20,i))))
}
ncol(combn(20,20))
factorial(20)/(factorial(20)*factorial(0))

# EXAMPLE OF "NOT IN" CODE:
ac = "A B C D E F G H I J K Q R S T U V"
abc = "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
vac = unlist(strsplit(ac, " "))
vabc = unlist(strsplit(abc, " "))
vabc[! vabc %in% vac] # YOU WANT THE PART OF VABC THAT'S JUST THE VABC THAT'S NOT ALSO IN VAC ==> SO, YOU WANT THE VABC PART THAT'S EXCLUSIVE TO VABC

# Problem 1.30:
ncol(combn(11,6))
?expand.grid
trial1 = expand.grid(first = c(1,2,3), second = c(1,2,3), third = c(1,2,3))
class(trial1)
trial1[1,]
all.samps = expand.grid(first = c(1,2,7,8,14,20),
                        second = c(1,2,7,8,14,20),
                        third = c(1,2,7,8,14,20),
                        fourth = c(1,2,7,8,14,20),
                        fifth = c(1,2,7,8,14,20),
                        sixth = c(1,2,7,8,14,20))
all.samps
themeans = vector()
for (i in 1:nrow(all.samps)){
  themeans[i] = mean(all.samps[i,])
} # DOESN'T WORK
hist(themeans) # (DOESN'T WORK)

all.samps$means = mean(all.samps[,1:6]) # DOESN'T WORK

themeans = apply(all.samps,1,mean)
hist(themeans)
summary(themeans)
mean(all.samps[1,1:6])
mean(all.samps[1,])


# A PDF:
x = seq(-10,10,length.out = 10000); x
pdf.x = exp(-x)/((1 + exp(-x))^2)
plot(x,pdf.x, xlim = c(-20,20), type = "l", col = "blue")

cdf.x = 1/(1 + exp(-x))
plot(x,cdf.x, xlim = c(-20,20), type = "l", col = "red")
points(x,pdf.x, type = "l", col = "blue")



x = seq(0,1,length.out = 1000)
plot(-log(x))
abline(h = 0, v = 0, col = "grey")
y = -log(x)
plot(x,y, type = "l", col = "red")

y = function(x) exp(x)
plot(y, xlim = c(-5,5), ylim = c(-5,50))
abline(h = 0, v = 0, col = "grey")



####################################################################
### HW 3

## 1.26 - Roll a die until a 6 appears. What's the probability that it will be rolled more than
# 5 times before a 6 appears?

(5/6)^5


####################################################################
### Simulating theorem 2.1.10 (Probability Integral Transformation)

x = seq(0,10e8, length.out = 100000)
y = 1 - exp(-x/5)

plot(y)
?rexp
y = rexp(1000, rate = 5)
plot(y)
mean(y)
y = rexp(1000, rate = 1/5)
plot(y)
mean(y)
plot(density(y))
hist(y, breaks = 50)
df = data.frame(X = 1:1000, Y = y)
library(ggplot2)
ggplot(df, aes(X,Y)) + 
  geom_point(alpha = .3, col = "red")

# Now:
cdf = 1 - exp(-y/5)
hist(cdf) # THERE'S THE UNIFORM DISTRIBUTION!!!!!

y = runif(10000)
hist(y)
x = -5*log(1 - y)
hist(x, breaks = 100) # AMAZING. GETS THE EXPONENTIAL DISTRIBUTION FROM THE UNIFORM (REVERSES WHAT WE JUST DID ABOVE)

15*(16/52) + (10+9+8+7+6+5+4+3+2)*(4/52)



#################################################################################

# Homework #4

lims = 10
func = function(x) (3/8)*(x + 1)^2
curve(func, xlim = c(lims,-lims), ylim = c(lims,-lims)); abline(h = 0, v = 0, col = "grey")



#################################################################################

# Homework #5

gamma(5/2)/sqrt(pi)
sqrt(pi)/2


# Homework #6



#################################################################################

# HGeom:
ncol(combn(6,0))*ncol(combn(25-6,10))/ncol(combn(25,10))


# Binom:
?pbinom()
1 - pbinom(1,4,.5)
ncol(combn(1,0))
ncol(combn(1,1))



#################################################################################
#################################################################################

# D = diseased
# ND = not diseased
# T = test positive
# I = test negative
pD = .0038
pND = .9962
pTgD = .967
pIgD = 1-.967
pTgND = .015
pIgND = 1-.015

pT = pTgD*pD + pTgND*pND; pT
pDgT = pD*pTgD/pT; pDgT

########

# leaky valve
(.13*.018)/(.13*.018 + .2*.014 + .55*.009 +.12*.003)





##########################################################################################
##########################################################################################

### EXAM II PART (CHAPTER 3 AND 4)

# Chi-square distributions with different df's:
fx = function(x) {
  df = 30
  alpha = df/2
  beta = 2
  (1/gamma(alpha)*beta^alpha)*(x^(alpha-1))*exp(-x/2)
}
fx1 = function(x) {
  df = 40
  alpha = df/2
  beta = 2
  (1/gamma(alpha)*beta^alpha)*(x^(alpha-1))*exp(-x/2)
}
fx2 = function(x) {
  df = 50
  alpha = df/2
  beta = 2
  (1/gamma(alpha)*beta^alpha)*(x^(alpha-1))*exp(-x/2)
}
ub = 1e4 # WHY CAN'T I GET MY WORKING PORTION OF THE DISTN. TO MOVE TO THE RIGHT WITHOUT IT BLOWING UP VERTICALLY?
xub = 500
curve(fx, from = 0, to = xub, ylim = c(0,ub), n = 1000)
curve(fx1, from = 0, to = xub, ylim = c(0,ub), n = 1000, col = "red", add = TRUE)
curve(fx2, from = 0, to = xub, ylim = c(0,ub), n = 1000, col = "green", add = TRUE)


####################################################################################

### HW 7


q = 0
m = 6 # actually, 6 or more, so P(X > x) of lower.tail FALSE would require x to be 5
n = 100
k = 30
phyper(q,m,n,k, lower.tail = TRUE)

myhypergeom = function(N,M,K,x) {
  p = ( factorial(M)/( factorial(x)*factorial(M-x) ) )*( factorial(N-M)/( factorial(K-x)*factorial((N-M)-(K-x)) ) )/( factorial(N)/( factorial(K)*factorial(N-K) ) )
  print(sprintf("The probability of getting a %s -size lot with %s bad items if their sample of size %s has %s bad items in it is %s",N,M,K,x,p))
}
myhypergeom(N=100,M=6,K=5,x=0) # trial and error
myhypergeom(N=100,M=6,K=15,x=0) # trial and error
myhypergeom(N=100,M=6,K=25,x=0) # trial and error
myhypergeom(N=100,M=6,K=35,x=0) # trial and error
myhypergeom(N=100,M=6,K=31,x=0) # just over (0.10056)
myhypergeom(N=100,M=6,K=32,x=0) # just under (0.0918)

# ncol(combn(94,30)) # impossible to compute in R
# prettyNum(ncol(combn(30,11)), big.mark = ",", scientific = FALSE) # takes too long to compute in R

myhygeom.addition = function(N,M,K,x1,x2) {
  p = ( factorial(M)/( factorial(x1)*factorial(M-x1) ) )*( factorial(N-M)/( factorial(K-x1)*factorial((N-M)-(K-x1)) ) )/( factorial(N)/( factorial(K)*factorial(N-K) ) ) +
    ( factorial(M)/( factorial(x2)*factorial(M-x2) ) )*( factorial(N-M)/( factorial(K-x2)*factorial((N-M)-(K-x2)) ) )/( factorial(N)/( factorial(K)*factorial(N-K) ) )
  return(p)
}
myhygeom.addition(N=100,M=6,K=30,x1=0,x2=1)
myhygeom.addition(N=100,M=6,K=20,x1=0,x2=1)
myhygeom.addition(N=100,M=6,K=51,x1=0,x2=1) # bingo


### 3.5
probvec = vector()
for (i in 85:100){
  probvec[i-84] = (factorial(100)/( factorial(i)*factorial(100-i) ))*(.8^i)*.2^(100-i) 
}
probvec
sum(probvec)


### 3.10

newhgeom = function(cN){
  p = ( factorial(cN)/( factorial(4)*factorial(cN-4) ) )*( factorial(496-cN)/( factorial(2)*factorial(496-cN-2) ) )
  print(p)
}
newhgeom(cN=320)

( factorial(496)/( factorial(4)*factorial((496)-4) ) )*(factorial(492)/( factorial(2)*factorial(492-2) ) )

factorial(496)/( factorial(4)*factorial(496-4) )

factorial(492)


###############################################

pbinom(13,25,.6)
pnorm(13.5,mean=15,sd=sqrt(6))


## matrix of Beta distribution plots:
x = seq(0,1,length=100)

par(mfrow=c(5,5), mar=c(3,3,0.5,0.5) )
z = c(0.5,1,2,3,4)
for(i in z){
  for(j in z){
    a = j
    b = i
    y = dbeta(x,a,b)
    
    plot(x,y,ylim=range(0,3.0),type="l",lwd=2,col="blue",main=NULL,ylab=" ",xlab=" ")
    legend("top","center",paste("a=",a,", b=",b),bty="n")
    mtext(expression(paste("p(",theta,")")),2,line=2,cex=.75)
    mtext(expression(theta),1,line=2,cex=.75)
  }
}


#############################################################################
#############################################################################

library(tidyr)

smarties = c(100,99,106,110,100,101,104,102,96,99,
             106,103,103,100,115,99,99,105,105,100,
             104,102,104,100,104,105,106,112,106,119,
             119,105,106,105,102,98,97,101,100,106,
             103,105,98,110,102,106,102,106,92)
length(smarties)
hist(smarties, breaks = 10)
mean(smarties)
sd(smarties)

nsmarties = smarties/100
m = mean(nsmarties)
sd(nsmarties)
v = var(nsmarties)

s = data.frame(matrix(NA, nrow = 100, ncol = 3))
for (i in 1:100){
  s[i,] = sample(nsmarties,3)
}
# slong = gather(s, key = "obs", value = "weights", c("X1","X2","X3"))
# slong$mean = tapply(slong$weights,slong$obs,mean)
# slong = slong[,-3] # This was a dead end! I don't want 3 means! I want 100!

mymeans = vector()
for (i in 1:100){
  mymeans[i] = mean(c(s[i,1],s[i,2],s[i,3]))
}
mymeans
s$means = mymeans

?var() # THIS FUNCTION USES n-1 AS THE DENOMINATOR! SO IT SHOULDN'T BE BIASED!
myvars = vector()
for (i in 1:100){
  myvars[i] = var(c(s[i,1],s[i,2],s[i,3]))
}
myvars
# prettyvars = prettyNum(myvars, scientific = FALSE)
s$vars = myvars
# s$pvars = prettyvars

# THE HISTOGRAM OF THE VARIANCES IS ACTUALLY REALLY INTERESTING (WHAT DISTRIBUTION DOES IT FOLLOW??),
# THE HISTOGRAM OF THE MEANS IS NOT THAT NORMAL FOR A HUNDRED SAMPLE MEANS.
hist(s$means, breaks = 30)
hist(s$vars, breaks = 30)

a = 7
plot(1:a,rep(v,a), ylim = c(0,max(s$vars)), type = "l", lwd = 2)
v1 = sample(s$vars,a); points(1:a, v1, col = "red") #; round(v1,5)
v2 = sample(s$vars,a); points(1:a, v2, col = "green") #; round(v2,5)
v3 = sample(s$vars,a); points(1:a, v3, col = "blue") #; round(v3,5)
v4 = sample(s$vars,a); points(1:a, v4, col = "orange") #; round(v4,5)
v5 = sample(s$vars,a); points(1:a, v5, col = "yellow") #; round(v5,5)
v6 = sample(s$vars,a); points(1:a, v6, col = "purple") #; round(v6,5)
v7 = sample(s$vars,a); points(1:a, v7, col = "pink") #; round(v7,5)

M = data.frame(matrix(NA, nrow = 7, ncol = 7))
for (i in 1:7){
  for (j in 1:7){
    M[i,j] = eval(parse(text = paste0("v",i)))[j]
  }
}
Mmeans = vector()
for (i in 1:7){
  Mmeans[i] = mean(M[,i])
}
Mmeans
points(1:a, Mmeans, pch = 18, col = "red")

thevars = c(v1,v2,v3,v4,v5,v6,v7); thevars
mean(thevars)
points(4,mean(thevars), pch = 22, col = "blue")


#############################################################################
#############################################################################

# 3.36 in Statistical Inference 2nd ed.
curve((63/4)*(x^6 - x^8), from = -1, to = 5.5, 
      n = 1000, ylim=c(0,2))
abline(h = 0, v = 0, col = "gray")
curve((63/4)*((x-3)^6 - (x-3)^8), col = "red",
      n = 1000, add = TRUE)
curve((63/4)*(1/2)*(((x-3)/2)^6 - ((x-3)/2)^8), col = "blue", 
      n = 1000, add = TRUE)
abline(h = .75, col = "gray", lty = 2)


qnorm(.05, lower.tail = FALSE)

pnbinom(5,20,.5)
rnbinom(20,5,.5)



#############################################################################
#############################################################################

sample(1:276,9)
M = as.data.frame(matrix(NA, nrow = 4, ncol = 9))
for (i in 1:nrow(M)){
  M[i,] = sample(1:276, 9)
}
k1 = max(M[1,]); k1
m1 = ncol(M); m1
Nhat1 = ((k1+1)/k1)*m1 - 1; Nhat1
N1b = m1 + m1/k1 - 1; N1b


#############################################################################


dbinom(7,22,.5)
dnbinom(7,22,.5)
pbinom(7,22,.5, lower.tail = TRUE)
pnbinom(7,22,.5, lower.tail = TRUE)









#############################################################################
#############################################################################
#############################################################################
#############################################################################

#############################################################################
################################# MATH STATS ################################ 
#############################################################################

sum(1:7)/7
mean(1:7)
gamma(.5)/sqrt(pi)

pnorm(-3)
z = seq(-3,3,1); z
w = vector()
for (i in 1:length(z)){
  w[i] = pnorm(z[i])
}
w[5] - w[3]
w[6] - w[2]
w[7] - w[1]

a = runif(5,0,100); a
b = runif(10,0,100); b
c = runif(50,0,100); c

median(a)
median(b)
median(c)
sample(a,1)
sample(b,1)
sample(c,1)


### FROM TOPIC 2 NOTES, PART ABOUT THE VARIANCE OF THE MEDIAN:
## Compare the means and medians of 1000 random samples of size n = 3 from 
# the uniform(0,1):
mymedians = vector()
mymeans = vector()
for (i in 1:1000){
  x.vec = runif(3,0,1)
  mymedians[i] = median(x.vec)
  mymeans[i] = mean(x.vec)
}
par(mfrow = c(2,1))
hist(mymedians)
hist(mymeans)

var(mymedians) # compare to 1/20
var(mymeans)
mean(mymedians)
mean(mymeans)














