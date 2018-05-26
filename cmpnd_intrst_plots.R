#########################################

### Cool stuff you can do with ggplot ###

#########################################


### Population pyramids! (from census.gov data)
# df = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/census_Nigeria_age_cohorts.csv")
# df = df[,c(3,5,6)]
# str(df)
# df$Male.Population = as.numeric(df$Male.Population)
# df$Female.Population = as.numeric(df$Female.Population)
# str(df)
# df$Male.Population = as.character(df$Male.Population)
# df$Female.Population = as.character(df$Female.Population)
# apply(df[,2:3], 2, function(x) prettyNum(x, big.mark = ","))
# 
# myurl = "https://www.census.gov/data-tools/demo/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y=2014&R=-1&C=NI"
# library(RCurl)
# urldata = getURL(myurl)
# mydata = readHTMLTable(urldata, stringsAsFactors = FALSE) # WOW THIS IS INSANE. NOTHING WORKED 
# # UNTIL I TRIED DOING THIS, AS INSTRUCTED ON A DATA CAMP WEBSITE: https://www.datacamp.com/community/tutorials/r-data-import-tutorial#data
# mydata$`Mid-year Population by Five Year Age Groups and Sex - Custom Region - Nigeria`
# mydata[1]
# class(mydata) # It's a list!
# str(mydata)
# # FALSE --> identical(mydata[1],mydata$`Mid-year Population by Five Year Age Groups and Sex - Custom Region - Nigeria`)
# ngria = mydata[1]; class(ngria)
# mydata[[1]]
# identical(mydata[[1]],mydata$`Mid-year Population by Five Year Age Groups and Sex - Custom Region - Nigeria`) # TRUE!!!!!!!
# Nigeria = mydata$`Mid-year Population by Five Year Age Groups and Sex - Custom Region - Nigeria`
# class(Nigeria)
# pyrdf = Nigeria[,c(2,4,5)]
# pyrdf[,2:3] = apply(pyrdf[,2:3],2, function(x) as.numeric(as.character(gsub(",","",x))))
# # names(pyrdf)
# # colnames(pyrdf) # same as names? okay ...
# colnames(pyrdf) = c("Age","Male","Female")
# pyrdf = pyrdf[pyrdf$Age != "Total",]
# pyrdf$Male = -1*pyrdf$Male
# # pyrdf$Age = as.factor(pyrdf$Age) # THIS IS REALLY IMPORTANT!!! PAY ATTENTION TO THIS! DOING IT
# # THIS WAY WILL ACTUALLY CAUSE A PROBLEM THAT I'M GLIDE I WAS ABLE TO REALIZE SHOULD BE TRACED
# # BACK TO THIS STEP! THE PYRAMID ENDS UP HAVING A FUNKY PROBLEM WITH IT. IT'S BECAUSE I DID THE
# # LAZY, NAIVE THING AND THOUGHT THIS SIMPLE CONVERSION WOULD WORK. INSTEAD I NEED TO DO IT THIS
# # WAY:
# pyrdf$Age = factor(pyrdf$Age, levels = pyrdf$Age, labels = pyrdf$Age)
# pyr.melt = melt(pyrdf, value.name = "Population", variable.name = "Gender", id.vars = "Age")
# 
# ggplot(pyr.melt, aes(x = Age, y = Population, fill = Gender)) + 
#   geom_bar(subset = .(Gender == "Female"), stat = "identity") + 
#   geom_bar(subset = .(Gender == "Male"), stat = "identity") + 
#   scale_y_continuous(breaks = seq(-15000000,15000000,5000000), 
#                      labels = paste0(as.character(c(seq(15,0,-5),seq(5,15,5))), "m")) + 
#   coord_flip() + 
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw()

## Function for creating population pyramids in ggplot from census.gov data:
library(XML) # To get the HTML Table data off the internet
library(RCurl) # Because XML's readHTMLTable by itself doesn't work
library(reshape2) # for the melt function?
library(plyr) # for ... ???
library(ggplot2)

get_censdata = function(country, year){
  c1 = "https://www.census.gov/data-tools/demo/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y="
  c2 = "&R=-1&C="
  myurl = paste0(c1, year, c2, country)
  urldata = getURL(myurl)
  mydata = readHTMLTable(urldata, stringsAsFactors = FALSE)
  df = mydata[[1]]
  keep = c(2,4,5)
  df = df[,keep]
  names(df) = c("Age","Male","Female")
  cols = 2:3
  df[,cols] = apply(df[,cols],2, function(x) as.numeric(as.character(gsub(",", "", x))))
  df = df[df$Age != 'Total',]
  df$Male = -1*df$Male
  df$Age = factor(df$Age, levels = df$Age, labels = df$Age)
  df.melt = melt(df, value.name = "Population", variable.name = "Gender", id.vars = "Age")
  return(df.melt)
}

nigeria = get_censdata("NI",2014)
n1 = ggplot(nigeria, aes(x = Age, y = Population, fill = Gender)) + 
  geom_bar(data = nigeria[which(nigeria$Gender == "Male"),], stat = "identity") + 
  geom_bar(data = nigeria[which(nigeria$Gender == "Female"),], stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(15,10,5,0,5,10,15)),"m")) +
  ggtitle("Nigeria - population pyramid") + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") +
  theme_bw()
n1 # WITHOUT THE STAT = 'IDENTITY' IN EACH OF THE GEOM_BARS, IT DOESN'T WORK ("Error: stat_count() must not be used with a y aesthetic  ")

###

rnd2millions = c(.05,.1,.2,.3,.4,.5,1,2,3,4,5,6,7,8,9,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,
                 110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,
                 325,350,375,400,425,450,475,500,525,550,575,600,625,650,675,700,725,750,775,800)
# plot(1:length(rnd2millions),rnd2millions)
rnd2millions = rnd2millions*1e6
the2logs = log10(rnd2millions); the2logs

rnd2num = function(yournum){
  i = 1
  while (log10(yournum) > the2logs[i]){
    i = i+1
  }
  return(seq(-10^the2logs[i],10^the2logs[i],length.out = 9))
}

get_censplot = function(country, year){
  pais = readline(prompt = "Enter name of country you want displayed on graph: ")
  c1 = "https://www.census.gov/data-tools/demo/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y="
  c2 = "&R=-1&C="
  myurl = paste0(c1, year, c2, country)
  urldata = getURL(myurl)
  mydata = readHTMLTable(urldata, stringsAsFactors = FALSE)
  df = mydata[[1]]
  keep = c(2,4,5)
  df = df[,keep]
  names(df) = c("Age","Male","Female")
  cols = 2:3
  df[,cols] = apply(df[,cols],2, function(x) as.numeric(as.character(gsub(",", "", x))))
  df = df[df$Age != 'Total',]
  df$Male = -1*df$Male
  df$Age = factor(df$Age, levels = df$Age, labels = df$Age)
  df.melt = melt(df, value.name = "Population", variable.name = "Gender", id.vars = "Age")
  mx = max(abs(df.melt$Population))
  brks = rnd2num(mx)
  ggplot(df.melt, aes(x = Age, y = Population, fill = Gender)) + 
    geom_bar(data = df.melt[which(df.melt$Gender == "Male"),], stat = "identity") +
    geom_bar(data = df.melt[which(df.melt$Gender == "Female"),], stat = "identity") + 
    scale_y_continuous(breaks = brks, labels = paste0(as.character(abs(brks)/1e6),"m")) + 
    ggtitle(sprintf("%s - population pyramid",pais)) + 
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") +
    theme_bw()
}

get_censplot("GM",2014)
get_censplot("US",2014)
get_censplot("UK",2014)
get_censplot("CH",2014)
get_censplot("RS",2014)
get_censplot("IN",2014)
get_censplot("MX",2014)
get_censplot("NI",2014)
get_censplot("BR",2014)
get_censplot("NL",2014)
get_censplot("FR",2014)
get_censplot("JA",2014)
get_censplot("PL",2014)
get_censplot("SA",2014)


# get_censplot("RS",1980) # NO DATA FOR THAT YEAR. TURNS OUT NOT ALL YEARS HAVE DATA




















