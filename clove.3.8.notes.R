### clove 3/8/18 all 450 read

c.3.8.450 = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/clove_3.8_450.csv")


  
plot(c.3.8.450.top$X450)
library(data.table)
one450 = c.3.8.450.top
one450[one450$Well %like% "D",] = NA
one450[one450$Well %like% "E",] = NA
one450 = na.omit(one450)
plot(one450$X450)

A = one450[one450$Well %like% "A",]
B = one450[one450$Well %like% "B",]
C = one450[one450$Well %like% "C",]

ABC = rbind(A,B,C)
plot(ABC$X450)
EF = one450[one450$Well %like% "F",]
G = one450[one450$Well %like% "G",]
H = one450[one450$Well %like% "H",]
FGH = rbind(EF,G,H)

par(mfrow = c(2,1)) # This means 2 rows, one column
plot(ABC$X450)
plot(FGH$X450)

library(ggplot2)

ABC$nums = 1:36
ggplot(ABC, aes(x = nums)) + 
  geom_line(aes(y = ABC$X450), col = "green") +
  geom_line(aes(y = FGH$X450), col = "red")

ggplot(ABC, aes(x = nums)) + 
  geom_point(aes(y = ABC$X450), col = "green") +
  geom_point(aes(y = FGH$X450), col = "red")


