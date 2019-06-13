# Maps in R



# Libraries:
library(grid)
# install.packages("rworldmap")
library(rworldmap)
library(mapproj)



############################################################################
############################################################################

# Practice with rworldmap
## Source: http://egallic.fr/en/european-map-using-r/

# Step 1: assign getMap() to a variable name, 
## and assign lists of country names of interest to variables
worldmap = getMap()

europe = "Albania
Andorra
Armenia
Austria
Azerbaijan
B
Belarus
Belgium
Bosnia and Herzegovina
Bulgaria
C
Croatia
Cyprus
Czech Rep.
D
Denmark
E
Estonia
F
Finland
France
G
Georgia
Germany
Greece
H
Hungary
I
Iceland
Ireland
Italy
K
Kazakhstan
Kosovo
L
Latvia
Liechtenstein
Lithuania
Luxembourg
M
Malta
Moldova
Monaco
Montenegro
N
Netherlands
North Macedonia (formerly Macedonia)
Norway
P
Poland
Portugal
R
Romania
Russia
S
San Marino
Serbia
Slovakia
Slovenia
Spain
Sweden
Switzerland
T
Turkey
U
Ukraine
United Kingdom"
europe = unlist(strsplit(europe,"\n"))
which(nchar(europe) == 1)
europe = europe[-which(nchar(europe) == 1)]
europe
europe = europe[-35]
eur2 = europe[which(europe %in% worldmap$NAME)]

m = match(eur2,europe)
plot(m,rep(1,length(m)))
df = data.frame(Europe = europe,
                WMeurope = NA)
df$WMeurope[m] = europe[m]
eur2 = eur2[-which(eur2 == "Russia")]
eur2
eur2 = eur2[-which(eur2 == "Kazakhstan")]
eur2

crafts = c("Belgium","Denmark","Germany","Spain","France","Ireland","Italy",
           "Netherlands","Portugal","Sweden","United Kingdom")

# Step 2: Create an index of which worldmap NAMEs are included in the eur2 list
indEurope = which(worldmap$NAME %in% eur2); indEurope

# Step 3: Use the index in the most sophisticated step: creating dataframes of coordinates using lapply
europeCoords = lapply(indEurope, function(i){
  EUdf = data.frame(worldmap@polygons[[i]]@Polygons[[1]]@coords)
  EUdf$region = as.character(worldmap$NAME[i])
  colnames(EUdf) = list("long","lat","region")
  return(EUdf)
})

# Step 4: Combine all of the dataframes into one
europeCoords = do.call("rbind",europeCoords)

# Step 5: Add a column with fill categories
europeCoords$newvalue = NA                     # I HAVE NO IDEA WHY THIS STEP IS NECESSARY WHEN IT IS DEFINITELY NOT NECESSARY TO ADD A NEW COLUMN IN MTCARS
europeCoords$newvalue[which(europeCoords$region %in% crafts)] = "included"
europeCoords$newvalue[-which(europeCoords$region %in% crafts)] = "not included"

# Step 6: Plot using geom_polygon, with group, fill, colour and size in aes()
ggplot() +
  geom_polygon(data = europeCoords,
               aes(long,lat,group = region, fill = newvalue),
               colour = "black", size = .15) +
  coord_map(xlim = c(-25,53), ylim = c(30,72)) +
  labs(title = "Countries included in Nicholas Crafts' book", fill = "Criterion")





##################################################################################

# European populations
## Source: https://www.worldometers.info/population/countries-in-europe-by-population/

euroPop = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/European_countries.csv")
head(euroPop)
colnames(euroPop) = c("country","pop_2019","yearly_change","net_change","density_km",
                      "area_km","migrants_net","fert_rate","median_age","urban_perc",
                      "world_share")
euroPop = euroPop[-1,]
class(euroPop$pop_2019)
euroPop$pop_2019 = as.character(euroPop$pop_2019)
euroPop$pop_2019 = gsub(",","",euroPop$pop_2019)
euroPop$pop_2019 = as.numeric(euroPop$pop_2019)

euroPop$country[c(1,7,8,9,13,16,17,18,19,21,24,27,28,29,30,31,32,33,34,35,36)]
euroPop$side = NA
euroPop$side[c(1,7,8,9,13,16,17,18,19,21,24,27,28,29,30,31,32,33,34,35,36)] = "east"
euroPop$side[-c(1,7,8,9,13,16,17,18,19,21,24,27,28,29,30,31,32,33,34,35,36)] = "west"

east.pop = sum(euroPop$pop_2019[which(euroPop$side == "east")])
prettyNum(east.pop, big.mark = ",", scientific = FALSE)
west.pop = sum(euroPop$pop_2019[which(euroPop$side == "west")])
prettyNum(west.pop, big.mark = ",", scientific = FALSE)

east.pop/sum(euroPop$pop_2019)

west = euroPop$country[which(euroPop$side == "west")]
east = euroPop$country[which(euroPop$side == "east")]

worldmap = getMap()

# Create index:
indEurope = which(worldmap$NAME %in% euroPop$country)

# wrldeuro = worldmap$NAME[which(worldmap$NAME %in% euroPop$country)]
"%ni%" = Negate("%in%")
# euroPop$country[which(euroPop$country %ni% wrldeuro)]
# euroPop$side[which(euroPop$country %ni% wrldeuro)] = "not in Coords"
# euroPop$side
# newWest = euroPop$country[which(euroPop$side == "west")]; newWest
# west[which(west %ni% newWest)]
# newEast = euroPop$country[which(euroPop$side == "east")]

# Create new reference dataframe for adding the fill values to the europeCoords dataframe:
Popeuro = euroPop[-which(euroPop$side == "not in Coords"),]

# Create coords dataframes:
europeCoords = lapply(indEurope, function(i){
  EUdf = data.frame(worldmap@polygons[[i]]@Polygons[[1]]@coords)
  EUdf$region = as.character(worldmap$NAME[i])
  colnames(EUdf) = list("long","lat","region")
  return(EUdf)
})

europeCoords = do.call("rbind",europeCoords)

# fill categories in europeCoords:
europeCoords$side = Popeuro$side[match(europeCoords$region,Popeuro$country)]
europeCoords$pop = Popeuro$pop_2019[match(europeCoords$region,Popeuro$country)]

EW = ggplot() +
  geom_polygon(data = europeCoords,
               aes(long,lat, group = region, fill = side),
               colour = "black", size = .15) +
  labs(title = "Eastern and non-eastern Europe", fill = "Eastern or not")
EW

Pop = ggplot() +
  geom_polygon(data = europeCoords,
               aes(long,lat, group = region, fill = pop),
               colour = "black", size = .15) +
  labs(title = "Countries by population", fill = "Population")
Pop

grid.arrange(EW,Pop)

sum(euroPop$pop_2019[which(euroPop$side == "east")]) - 
  euroPop$pop_2019[1] + 
  euroPop$pop_2019[which(euroPop$country == "Greece")]




####################################################################################

# Needed for the steps further below:
euroPop = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/European_countries.csv")
head(euroPop)
colnames(euroPop) = c("country","pop_2019","yearly_change","net_change","density_km",
                      "area_km","migrants_net","fert_rate","median_age","urban_perc",
                      "world_share")
euroPop = euroPop[-1,]

# Life expectancy in Europe
## Source: Wikipedia List of European countries by life expectancy

euLife = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/Europe_life.expect.csv")

worldmap = getMap()

# Compare the countries from euroPop above to those in Wikipedia's life expectancy table:
O = as.matrix(euroPop$country[order(euroPop$country)])
as.matrix(O[-c(2,5,8,13,17,19,23)])
euLife = euLife[-c(7,19,42),]                         # GET RID OF THE ONES NOT IN EUROPOP

# Get rid of the countries that aren't in Wikipedia:
europeLife = cbind(as.matrix(O[-c(2,5,8,13,17,19,23)]),euLife)
europeLife = as.data.frame(europeLife)
colnames(europeLife) = c("country","life_expect")

europeLife$life_expect = as.character(europeLife$life_expect)
europeLife$life_expect = as.numeric(europeLife$life_expect)

# Change "Czechia" to "Czech Rep.":
## First, you need to change the countries from factors to characters:
europeLife$country = as.character(europeLife$country)
europeLife[7,1] = "Czech Rep."

# Don't forget to define the function "%ni%" here:
"%ni%" = Negate("%in%")
europeLife$country[which(europeLife$country %ni% worldmap$NAME)]

europeLife2 = europeLife[-which(europeLife$country %ni% worldmap$NAME),]
europeLife2 = europeLife2[-which(europeLife2$country == "Monaco"),]

# Create index:
indLife = which(worldmap$NAME %in% europeLife2$country)

# Create coords dataframes:
europeCoords = lapply(indLife, function(i){
  EUdf = data.frame(worldmap@polygons[[i]]@Polygons[[1]]@coords)
  EUdf$region = as.character(worldmap$NAME[i])
  colnames(EUdf) = list("long","lat","region")
  return(EUdf)
})

europeCoords = do.call("rbind",europeCoords)

# Create fill values column in europeCoords:
europeCoords$values = 
  europeLife2$life_expect[match(europeCoords$region,europeLife2$country)]

# Plot:
ggplot() +
  geom_polygon(data = europeCoords,
               aes(long,lat, group = region, fill = values),
               colour = "black", size = .15) +
  scale_fill_gradient(low = "#FF0000FF",high = "#FFFF00FF") +
  labs(title = "European countries by life expectancy", 
       fill = "Life expectancy (yrs)")

# Check to make sure you matched the life expectancy values to the right countries:
europeCoords[which(europeCoords$region == "Netherlands"),]





##############################################

# Life expectancy in Latin America 2017
## Source: https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=ZJ

L = "Country	Most Recent Year	Most Recent Value	
Antigua and Barbuda	2017	77	
Argentina	2017	77	
Aruba	2017	76	
Bahamas, The	2017	76	
Barbados	2017	76	
Belize	2017	71	
Bolivia	2017	69	
Brazil	2017	76	
British Virgin Islands			
Cayman Islands	2010	82	
Chile	2017	80	
Colombia	2017	75	
Costa Rica	2017	80	
Cuba	2017	80	
Curacao	2017	78	
Dominica	2002	77	
Dominican Republic	2017	74	
Ecuador	2017	77	
El Salvador	2017	74	
Grenada	2017	74	
Guatemala	2017	74	
Guyana	2017	67	
Haiti	2017	64	
Honduras	2017	74	
Jamaica	2017	76	
Mexico	2017	77	
Nicaragua	2017	76	
Panama	2017	78	
Paraguay	2017	73	
Peru	2017	75	
Puerto Rico	2017	80	
Sint Maarten (Dutch part)	2012	73	
St. Kitts and Nevis	2002	71	
St. Lucia	2017	76	
St. Martin (French part)	2017	80	
St. Vincent and the Grenadines	2017	73	
Suriname	2017	72	
Trinidad and Tobago	2017	71	
Turks and Caicos Islands			
Uruguay	2017	78	
Venezuela, RB	2017	75	
Virgin Islands (U.S.)	2017	79"
L = gsub("2017","",L)
L
L2 = unlist(strsplit(L,"\t"))
L2
L2 = L2[-1]
L2 = L2[-1]
L2 = L2[-1]
L2 = gsub("\n","",L2)
L2

countries = L2[seq(1,length(L2),3)]
LE = L2[seq(3,length(L2),3)]
latin = as.data.frame(cbind(countries,LE))
latin = latin[-which(latin$LE == ""),]

rownames(latin) = 1:nrow(latin)       # VERY IMPORTANT!!!!

class(latin$countries)
latin$countries = as.character(latin$countries)
class(latin$LE)
latin$LE = as.character(latin$LE)
latin$LE = as.numeric(latin$LE)

worldmap = getMap()

"%ni%" = Negate("%in%")

# Getting all the names reconciled between the two lists:
latin[which(latin$countries %ni% worldmap$NAME),1]
latin$countries[39] = "Venezuela"
worldmap$NAME[grep("Bahamas",worldmap$NAME)]
which(latin$countries == "Bahamas, The")
latin$countries[4] = "Bahamas"

worldmap$NAME[grep("Antigua",worldmap$NAME)]
which(latin$countries == "Antigua and Barbuda")
latin$countries[1] = "Antigua and Barb."

latin[which(latin$countries %ni% worldmap$NAME),1]
worldmap$NAME[grep("Dominican",worldmap$NAME)]
latin$countries[16] = "Dominican Rep."
worldmap$NAME[grep("Cayman",worldmap$NAME)]
latin$countries[9] = "Cayman Is."

worldmap$NAME[239]
grep("Virgin I",worldmap$NAME)
latin$countries[40] = "U.S. Virgin Is."

# Now for the map
# Create index:
latInd = which(worldmap$NAME %in% latin$countries)
length(latInd)

# Create coordinates dataframes:
coordsLatin = lapply(latInd, function(i){
  latdf = data.frame(worldmap@polygons[[i]]@Polygons[[1]]@coords)
  latdf$region = as.character(worldmap$NAME[i])
  colnames(latdf) = list("longitude","latitude","region")
  return(latdf)
})

coordsLatin = do.call("rbind",coordsLatin)

# Create fill values column in coordsLatin:
coordsLatin$values = 
  latin$LE[match(coordsLatin$region,latin$countries)]

# plot:
ggplot() +
  geom_polygon(data = coordsLatin,
               aes(longitude,latitude, group = region, fill = values),
               colour = "black", size = .15) +
  scale_fill_gradient(low = "#FF0000FF", high = "#FFFF00FF") +
  labs(title = "Life expectancy in Latin America", fill = "Life expectancy (yrs)")







