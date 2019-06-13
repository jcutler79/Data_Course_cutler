### Unemployment rate in US by year since 1947

library(ggplot2)
library(scales)

# DO THIS (1) :
################################################################################################################
unempl = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/unemployment.csv")
unempl = unempl[12:82,1:2]

colnames(unempl) = c("year","rate")
unempl$rate = as.character(unempl$rate)
unempl$rate = as.numeric(unempl$rate)
unempl$year = as.character(unempl$year)
unempl$year = as.numeric(unempl$year)
################################################################################################################

a = ggplot(data = unempl, aes(x = year, y = rate)) + geom_line() 
a
D_Truman = 1945:1952
R_Eisen = 1953:1960
D_JFK = 1961:1963
D_LBJ = 1964:1968 # vline at 1964
R_Nixon = 1969:1973
R_Ford = 1974:1976 # vline at 1974
D_Carter = 1977:1980
R_Reagan = 1981:1988
R_HWBush = 1989:1992 # vline at 1989
D_Clinton = 1993:2000
R_Bush = 2001:2008
D_Obama = 2009:20016

# 2 : 
################################################################################################################
unempl$presdnts = c(rep("Truman",7),rep("Eisen",8),rep("JFK",3),rep("LBJ",5),rep("Nixon",5),
                    rep("Ford",3),rep("Carter",4),rep("Reagan",8),rep("HWBush",4),rep("Clinton",8),
                    rep("Bush",8),rep("Obama",8))
################################################################################################################

pres_and_rates = unempl[,c("rate","presdnts")]
pres_and_rates$presdnts = as.factor(pres_and_rates$presdnts)
class(pres_and_rates$presdnts)
prsd.rates = spread(pres_and_rates, rate, presdnts)

master.frame = data.frame()

Democrat = c(1945:1952,1961:1968,1977:1980,1993:2000,2009:2016)
Republican = c(1953:1960,1969:1976,1981:1992,2001:2008)

# 3 :
################################################################################################################
pres.dat = data.frame(years = 1947:2017, party = 
                        c(rep("Democrat",7),rep("Republican",8),rep("Democrat",8),
                          rep("Republican",8),rep("Democrat",4),rep("Republican",12),
                          rep("Democrat",8),rep("Republican",8),rep("Democrat",8)))
unempl$party = pres.dat$party
unempl$rows = 1:nrow(unempl)
################################################################################################################

colnames(unempl)
b = a + geom_rect(data = unempl, mapping = aes(xmin=1947,xmax=2017,ymin=2.9,ymax=9.7),
                  color = party, alpha = .2)
b

# 4 : 
################################################################################################################
fr = data.frame(x1 = c(1947,1953,1961,1969,1977,1981,1993,2001,2009), 
                x2 = c(1953,1961,1969,1977,1981,1993,2001,2009,2017), 
                y1 = rep(2.9,9), 
                y2 = rep(9.7,9),
                prty = c("D","R","D","R","D","R","D","R","D"))
ggplot() + geom_rect(data = fr, aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2, fill = prty)) + 
  scale_fill_manual(values = alpha(c("blue","red"), .3)) + 
  geom_line(data = unempl, mapping = aes(x = year, y = rate)) + # FINALLY GOT THIS PIECE OF CRAP TO WORK
  geom_point(x = 2000, y = 8) + 
  scale_x_continuous(breaks = seq(1945,2020,5)) +
  scale_y_continuous(breaks = seq(3,10,.5))
################################################################################################################

da.prezidents = data.frame(nombres = c("Truman","Eisenhower","JFK","LBJ","Nixon","Ford",
                                       "Carter","Reagan","HW Bush","Clinton","Bush","Obama"))
Truman.start = pres_and_rates %>% filter(presdnts == "Truman") %>% head(1) 
real.Tr.srt = Truman.start[1,1]
Truman.end = pres_and_rates %>% filter(presdnts == "Truman") %>% tail(1)
real.Tr.end = Truman.end[1,1]; real.Tr.srt - real.Tr.end 

Eisen.start = pres_and_rates %>% filter(presdnts == "Eisen") %>% head(1)
real.Eis.srt = Eisen.start[1,1]
Eisen.end = pres_and_rates %>% filter(presdnts == "Eisen") %>% tail(1)
real.Eis.end = Eisen.end[1,1]; real.Eis.srt - real.Eis.end

JFK.start = pres_and_rates %>% filter(presdnts == "JFK") %>% head(1)
real.JFK.strt = JFK.start[1,1]
JFK.end = pres_and_rates %>% filter(presdnts == "JFK") %>% tail(1)
real.JFK.end = JFK.end[1,1]; real.JFK.strt - real.JFK.end

LBJ.start = pres_and_rates %>% filter(presdnts == "LBJ") %>% head(1)
real.LBJ.strt = LBJ.start[1,1]
LBJ.end = pres_and_rates %>% filter(presdnts == "LBJ") %>% tail(1)
real.LBJ.end = LBJ.end[1,1]; real.LBJ.strt - real.LBJ.end



### What criteria would you use to rate a president? A dominant party?
# 1. CHECK unemployment
# 2. median household income
  # bottom 50% income
  # bottom 20% income
  # economic inequality (measured by the Gini?)
  # how well the top 1% and top .1% does
  # upward mobility
  # benefits - what the heck are benefits aside from variance forms of insurance and retirement savings?
# 3. cost of energy?
# 4. quality of health care? ratio of doctors to patients?
# 5. CHECK abortion rate
# 6. crime rate
# 7. deficit
# 8. rate of health uninsured
  # diabetes, obesity, heart disease
  # rate of preventable health problems going unaddressed due to cost
# 9. quality of education - funding, student-teacher ratio, etc.
#10. fraction of power generated by clean energy (includes nuclear, exludes all fossil fuels)
#11. R&D budget - changes in various areas of federally funded research
#12. military fatalities, absolute numer, and rate
#13. terror attacks
#14. gun violence - homicides, suicides, accidental
#15. human trafficking
#16. economic growth by percent GDP
#17. cost of higher education
#18. rate of matriculation into higher education, of degree completion
#19. cleanliness of air, water
#20. number of innocent people killed as a (hopefully collateral) result of military operations
#21. number and magnitude of war crimes/crimes against humanity committed by that president's administration
#22. military spending, just to see what their priorities are or aren't
#23. which industries get subsidized - fossil fuels, certain crops or foods, etc.



#######################

# Gini index applied project from calc textbook (Ch. 5.1 page 365)

curve(1*x^3, from = -.1, to = 1, xlab = "x", ylab = "y", main = "Power Model")
curve(x^1, add = TRUE)
abline(h=0,v=0)
?nls

# par(mar = c(5,4,4,4))
plot(x,y, xlab = "percent of households", ylab = "", col = "red")
mtext("US 2010", side = 2, line = 2, col = "red")
par(new=TRUE)
plot(x,x, xlab = "", ylab = "", col = "green"); mtext("egalitarian", side = 4, col = "green")


## DUMBEST LONGEST MOST REPETITIVE CODE IN THE UNIVERSE:
US1 = data.frame(fract.households = c(0,.2,.4,.6,.8,1), Y2010 = c(0,.034,.12,.266,.498,1))
US2 = data.frame(fract.households = c(0,.2,.4,.6,.8,1), Y2000 = c(0,.036,.125,.273,.503,1))
US3 = data.frame(fract.households = c(0,.2,.4,.6,.8,1), Y1990 = c(0,.038,.134,.293,.53,1))
US4 = data.frame(fract.households = c(0,.2,.4,.6,.8,1), Y1980 = c(0,.042,.144,.312,.559,1))
US5 = data.frame(fract.households = c(0,.2,.4,.6,.8,1), Y1970 = c(0,.041,.149,.323,.568,1)) 
# WHY DOES nls NEED DATA FRAMES WITH ONLY TWO COLUMNS??? WHY DOES EXPANDING THE DATA FRAME BREAK R?
# x = as.vector(US$hshlds.prcnt)
# y = as.vector(US$prcnt.income)
# nls(y ~ b*x^z, start = list(b = 0, z = 1)) # Doesn't work
########## BUT THIS DOES!!!!!!!!!!!!!!!!!:
nl2010 = nls(US1$Y2010 ~ a*US1$fract.households^b, data = US1, start = list(a = 1, b = 1)) # Not sure if the trick is to set a equal to 1 instead of 0
nl2010 # 0.9836 & 2.6790 
plot(US$fraction.households,US$Y2010, xlab = "", ylab = "", main = "US Lorenz curve over 40 yrs")
p = coef(nl2010) # This is what's money right here. This is how I'll get 500 predictions out of my Boston linear model
curve(p["a"]*x^p["b"], lwd = 2, col = "red", lty = 2, add = TRUE) # AMAZING FIT.
########## WAHOOO!!!!!!!!!!!!!!!!

nl2000 = nls(US2$Y2000 ~ a*US2$fract.households^b, data = US2, start = list(a = 1, b = 1)) # Not sure if the trick is to set a equal to 1 instead of 0
nl2000
par(new = TRUE) # right?
plot(US2$fract.households,US2$Y2000, xlab = "", ylab = "")
p2 = coef(nl2000)
curve(p2["a"]*x^p2["b"], lwd = 2, col = "orange", lty = 3, add = TRUE)

nl1990 = nls(US3$Y1990 ~ a*US3$fract.households^b, data = US3, start = list(a = 1, b = 1))
nl1990
par(new = TRUE)
plot(US3$fract.households, US3$Y1990, xlab = "", ylab = "")
p3 = coef(nl1990)
curve(p3["a"]*x^p3["b"], lwd = 2, col = "yellow", lty = 4, add = TRUE)

nl1980 = nls(US4$Y1980 ~ a*US4$fract.households^b, data = US4, start = list(a = 1, b = 1))
nl1980 # 0.9859 2.2922 
par(new = TRUE)
plot(US4$fract.households, US4$Y1980, xlab = "", ylab = "")
p4 = coef(nl1980)
curve(p4["a"]*x^p4["b"], lwd = 2, col = "green", lty = 6, add = TRUE)

nl1970 = nls(US5$Y1970 ~ a*US5$fract.households^b, data = US5, start = list(a = 1, b = 1))
nl1970 # 0.9859 2.2922 # 0.9862 2.2292 
par(new = TRUE)
plot(US5$fract.households, US5$Y1970, xlab = "", ylab = "")
p5 = coef(nl1970)
curve(p5["a"]*x^p5["b"], lwd = 2, col = "blue", lty = 5, add = TRUE)









################################################################################
################################################################################

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











