#### Data Camp practice with tidyr ####


### Labor Supply dataset from bundock github web page
L = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/LaborSupply.csv")

L$real.wages = exp(L$lnwg)
L$real.hrs = exp(L$lnhr)

wgs = L[1:12,c(5,8,9)]
hrs = L[1:12,c(5,8,10)]
colnames(hrs)

library(tidyr)
library(dplyr)
?inner_join
ilo.data = wgs %>% inner_join(hrs, by = c("age","year"))
ilo.data %>% count()
ilo.data

names(L)
dim(L)
class(L)
row.names(L)
length(row.names(L))

class(L$year)

### UN crime data

Cr = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/UN_crime_data.csv")






