#########################################

### Cool stuff you can do with ggplot ###

#########################################


### Population pyramids! (from census.gov data; inspired by the fact that tableau is supposed to be worth one's time to learn; in reality, R can make things like population pyramids just fine!) 

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
  df[,cols] = apply(df[,cols],2, function(x) as.numeric(as.character(gsub(",", "", x)))) # The '2' means it's being applied by column, not be row, right?
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






############################################################################################



### greta

library(greta)
t = normal(0, 32, dim = 2)
length(t)



############################################################################################


### keras!!!!!!!!!!

install.packages("devtools")
devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")
library(keras)
install_keras() # Error: Prerequisites for installing TensorFlow not available. Execute the following at a terminal to install the prerequisites:
# $ sudo /usr/bin/easy_install pip
# $ sudo /usr/local/bin/pip install --upgrade virtualenv

# Load MNIST images datasets (the MNIST dataset is built into Keras)
c(c(x_train, y_train), c(x_test, y_test)) %<-% dataset_mnist()

# Flatten images and transform RGB values into [0,1] range
x_train = array_reshape(x_train, c(nrow(x_train), 784))
x_test = array_reshape(x_test, c(nrow(x_test), 784))
x_train = x_train/255
x_test = x_test/255

# Convert class vectors to binary class matrices
y_train = to_categorical(y_train, 10)
y_test = to_categorical(y_test, 10)

# Define the model
mymodel = keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = .4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = .3) %>%
  layer_dense(units = 10, activation = 'softmax')

# Compile the model
mymodel %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Print a summary
summary(mymodel)

# Fit the model
history = mymodel %>% fit(
  x_train, y_train,
  batch_size = 128,
  epochs = 10,
  validation_split = .2 # So you don't overfit (and essentially train it to just memorize its training dataset), hold 20% of the data out
)

# Plot the training history
plot(history)

# Evaluate the model
mymodel %>% evaluate(x_test, y_test)

# Generate some predictions
mymodel %>% predict_classes(x_test[1:100,]) # spits out 100 single-digit numbers


## So, layers is the core concept of keras. There are 65 layers available. You can also
# create your own layers. One layer example is layer_dense(), which adds a densely-
# connected NN layer to an output. Dense layers are the staple of NNs. They are classic
# 'fully connected' NN layers. These layers are basically just a bunch of weights and
# biases that are applied. Convolutional layers are for computer vision. 
## Another example of a type of layer is ' recurrent layers ' which are layers that 
# maintain state based on previously seen data. In other words, if it doesn't just matter
# what you're seeing now, but what you've seen in the past, then you need recurrent 
# layers.

## Compiling models: Model compilation prepares the model for training by:
# Converting the layers into a TensorFlow graph
# Applying the specified loss function and optimizer
# Arranging for the collection of metrics during training

## There are a wide variety of loss functions in keras.

## There are a variety of optimizers.

## There are a variety of metrics.

## The cheat sheet for all this stuff in keras is in my cheat sheets folder!

## Machine learning tends to need lots of computing power. That's where the cloud comes 
# in. There are cloud GPUs that let you do batch jobs. 




############################################################################################



rent = 1600

utilit = 225
cars = 400
phones = 80
gas = 100

bills = utilit + cars + phones + gas; bills

12*sum(rent,utilit,cars,phones,gas)
miscell = 600
12*sum(rent,utilit,cars,phones,gas,miscell) - 12*sum(rent,utilit,cars,phones,gas)

(38000 - 12*sum(rent,utilit,cars,phones,gas,miscell))/12

######

f.utilit = 250
f.phones = 90
f.gas = 140
h.insur = 250
f.misc = 1000
food = 1300
loans = 1200

mrtg = 1500

f.bills = sum(c(f.utilit,f.phones,f.gas,h.insur,f.misc,food,loans)); f.bills*12 # $50,760





#########################################################
## Trial runs for the flashcard app:
# mysamp = 1:10
# output = 1
# output.cum = c(output)
# while (length(output.cum) <= 9){
#   output = sample(mysamp[-output.cum],1)
#   print(output)
#   output.cum = c(output.cum,output)
# }
# 
# mysamp = 1:5
# inum.cum = 1:3
# sample(mysamp[-inum.cum],1)
# inum.cum = 1:4
# mysamp[! mysamp %in% inum.cum]
# 
# mylist = list("one","two","three","four","five")
# trial = function(yourlist){
#   continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
#   mysamp = 1:length(yourlist)
#   inum = 1
#   inum.cum = c(inum)
#   i = 1
#   while (continue != ""){
#     if (length(inum.cum) == length(yourlist)-1){
#       inum = mysamp[! mysamp %in% inum.cum]
#       print(c(length(yourlist)-1, yourlist[[inum]]))
#       inum.cum = c(inum.cum,inum)
#       print(inum.cum)
#       continue = ""
#     } else {
#       inum = sample(mysamp[-inum.cum],1)
#       print(c(i, yourlist[[inum]]))
#       inum.cum = c(inum.cum,inum)
#       print(inum.cum)
#       i = i+1
#       continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
#     }
#   }
#   print("see ya later")
# }
#########################################################

##########################################################################################

                  ### Mesopotamian, Egyptian, and Israelite history ###

pharaohs = data.frame(X = c(-3100,-2670,-2613,-2589,-2558,-2460,-2278,-2180,-1971,-1878,-1541,-1479,-1458,-1425,-1390,-1352,-1332,-1292,-1279,-1213,-1186,-943,-530,-332,-51),
                      Y = rep(1.7,25))
rownames(pharaohs) = c("Narmer","Djoser","Sneferu","Khufu","Khafre","Neferefre","Pepi II",
                       "Nitocris","Sesostris I","Sesostris III","Amenhotep I","Hatshepsut",
                       "Thutmose III","Amenhotep II","Amenhotep III","Amenhotep IV",
                       "Tutankhamun","Ramesses I","Ramesses II","Merneptah",
                       "Ramesses III","Sheshonk","Cambyses II","Alexander the Great","Cleopatra")
phar2 = data.frame(X = c(-1550,-1492,-1401,-1334,-1290,-1191),
                   Y = rep(1.7,6))
rownames(phar2) = c("Ahmose I","Thutmose II","Thutmose IV","Neferneferuaten","Seti I","Twosret")

Mesop.kings = data.frame(X = c(-2334,-2112,-2046,-2025,-1728,-1114,-883,-859,-727,-705,-668,-626,-605,-556,-559,-530,-486,-465),
                         Y = c(rep(.25,14),rep(.05,4)))
rownames(Mesop.kings) = c("Sargon I","Ur-Nammu","Amar-Sin","Ushpia","Hammurabi","Tiglath-Pileser I",
                          "Ashurnasirpal II","Shalmaneser III","Shalmaneser V","Sennacherib","Ashurbanipal","Nabopolassar",
                          "Nebuchadnezzar II","Nabonidus","Cyrus the Great","Cambyses II",
                          "Xerxes I","Artaxerxes I")
SgnII.DrsI = data.frame(X = c(-722,-522),
                        Y = c( .25, .05))
rownames(SgnII.DrsI) = c("Sargon II","Darius I")

Israel.kings = data.frame(X = c(-1025,-1000,-915,-879,-828,-765,-730),
                          Y = c(    1,    1, 1.1, 1.1, 1.1, 1.1, 1.1))
rownames(Israel.kings) = c("Saul","David","Jeroboam I","Omri","Jehu","Jeroboam II","Hoshea")
Judah.kings = data.frame(X = c(-950,-920,-860,-760,-730,-708,-665,-625,-597),
                         Y = c(   1,  .9,  .9,  .9,  .9,  .9,  .9,  .9,  .9))
rownames(Judah.kings) = c("Solomon","Rehobaom","Jehoshaphat","Uzziah","Ahaz","Hezekiah","Manasseh","Josiah","Zedekiah")

other.dudes = data.frame(X = c(-1850,-1270,-5),
                         Y = c(    1, 1.26, 1))
rownames(other.dudes) = c("Abraham???","Moses???","Jesus")

periods = data.frame(x1 = c(-3150,-2900,-2686,-2334,-2181,-2154,-2112,-2061,-2004,-1790,-1674,-1650,-1600,-1549,-1392,-1200,-1069,-1000, -911, -672, -626, -587, -539, -539, -332, -322, -312),
                     x2 = c(-2686,-2350,-2181,-2154,-2061,-2112,-2004,-1674,-1790,-1392,-1549,-1180,-1200,-1069,-1056,-1000, -672, -587, -626, -332, -539, -539, -330, -330,  -30,  -63,  -63),
                     y1 = c( 1.25,    0, 1.25,    0, 1.25,    0,    0, 1.25,    0,    0, 1.25,   .5,  .75, 1.25,    0,  .75, 1.25,  .75,    0, 1.25,    0,  .75,    0,  .75, 1.25,  .75,    0),
                     y2 = c(    2,  .75,    2,  .75,    2,  .75,  .75,    2,  .75,  .75,    2,  .75, 1.25,    2,  .75, 1.25,    2, 1.25,  .75,    2,  .75, 1.25,  .75, 1.25,    2, 1.25,  .75),
                     period = c("AA - Early Dynastic",
                                "AB - Early Dynastic (Sumer)",
                                "BA - Old Kingdom",
                                "BB - Akkadian Empire",
                                "CA - 1st Int.",
                                "CB - Gutian Dynasty",
                                "DA - Ur III (Neo-Sumerian)",
                                "DB - Middle Kingdom",
                                "DC - Old Assyrian Empire",
                                "EA - First Babylonian Dynasty",
                                "EB - 2nd Int.",
                                "EC - Hittite Empire",
                                "ED - Late Bronze Age (Israel)",
                                "FA - New Kingdom",
                                "FB - Middle Assyrian Empire",
                                "FC - Iron Age I (Israel)",
                                "GA - 3rd Int.",
                                "GB - Iron Age II (Israel)",
                                "GC - Neo-Assyrian Empire",
                                "HA - Late Period",
                                "HB - Neo-Babylonian Empire",
                                "HC - Babylonian Period (Judah)",
                                "HD - Achaemenid Empire",
                                "HE - Persian Period (Judah)",
                                "IA - Ptolemaic (Hellenistic)",
                                "IB - Hellenistic Period (Judah)",
                                "IC - Seleucid Empire"))
nrow(periods)
Egypt.clrs = c("turquoise","turquoise3","chartreuse","maroon","gray85","yellow","orange","green","steelblue1","red2","gray90","indianred4","khaki1","springgreen","royalblue1","khaki2","gray95","khaki3","blue","forestgreen","red","red","forestgreen","forestgreen","chocolate1","chocolate","chocolate")
length(Egypt.clrs)
ggplot() + geom_rect(data = periods, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = period)) + 
  scale_fill_manual(values = alpha(Egypt.clrs,.8)) +
  theme_classic() + 
  geom_point(data = pharaohs, mapping = aes(x = X, y = Y), shape = 3) + 
  geom_point(data = Mesop.kings, mapping = aes(x = X, y = Y), shape = 3) +
  geom_point(data = SgnII.DrsI, mapping = aes(x = X, y = Y), shape = 3) + 
  geom_point(data = phar2, mapping = aes(x = X, y = Y), shape = 3) + 
  geom_point(data = Israel.kings, mapping = aes(x = X, y = Y), shape = 1) +
  geom_point(data = Judah.kings, mapping = aes(x = X, y = Y), shape = 5) +
  geom_point(data = other.dudes, mapping = aes(x = X, y = Y), shape = 3) +
  ggtitle("Famous rulers in Egypt, Israel and Mesopotamia") + xlab("Years BC (shown as -3000 to 0)") + ylab("Mesopotamia        Israel                 Egypt") + 
  geom_text(data = pharaohs, mapping = aes(x = X, y = Y, label = rownames(pharaohs)), size = 2, angle = 45, vjust = -.3, hjust = -.1) +
  annotate(geom = "text", x = c(-2555,-1897,-1420,-1240), y = rep(1.97,4), 
           label = c("|  4th |","|   12th |","|     18th     |","| 19th |"), size = 3) + 
  annotate(geom = "text", x = c(-1400,-1100,-793), y = rep(1.23,3),
           label = c("Late BA","IA 1","IA 2"), size = 3) +
  annotate(geom = "text", x = -1415, y = .7, label = "Hittite Empire", size = 3, color = "white") + 
  geom_text(data = Mesop.kings, mapping = aes(x = X, y = Y, label = rownames(Mesop.kings)), size = 2, angle = 45, vjust = -.3, hjust = -.1) +
  geom_text(data = SgnII.DrsI, mapping = aes(x = X, y = Y, label = rownames(SgnII.DrsI)), size = 2, angle = 45, vjust = 1.1, hjust = 1.1) +
  geom_text(data = phar2, mapping = aes(x = X, y = Y, label = rownames(phar2)), size = 2, angle = 45, vjust = 1.1, hjust = 1.1) +
  geom_text(data = Israel.kings, mapping = aes(x = X, y = Y, label = rownames(Israel.kings)), size = 2, angle = 45, vjust = -.3, hjust = -.1) +
  geom_text(data = Judah.kings, mapping = aes(x = X, y = Y, label = rownames(Judah.kings)), size = 2, angle = 45, vjust = 1.1, hjust = 1.1) +
  geom_text(data = other.dudes, mapping = aes(x = X, y = Y, label = rownames(other.dudes)), size = 2, angle = 45, vjust = -.3, hjust = -.1) +
  scale_x_continuous(breaks = seq(-3000,0,500), sec.axis = dup_axis()) +
  theme(legend.position = "bottom")



####################################################################################


install.packages("ape")
library(ape)
trape = read.tree(text = "(((((((Homo,Pan),Gorilla),Ponginae),Hylobatidae),Cercopithecoidea),Platyrrhines),Tarsiiformes);")
plot(trape, x.lim = c(0, 9))
nodelabels("23 Ma",9,frame = "c",bg = "white")
nodelabels("21 Ma",10,frame = "c",bg = "white")
nodelabels("19 Ma",11,frame = "c",bg = "white")
nodelabels("16 Ma",12,frame = "c",bg = "white")
nodelabels("12 Ma",13,frame = "c",bg = "white")
nodelabels("6.4 Ma",14,frame = "c",bg = "white")
nodelabels("5.4 Ma",15,frame = "c",bg = "white")


########################################################################################## 


# How much can a two-point difference in mean IQ make on the outliers of two populations?

df = data.frame(col1 = rnorm(10000,100,15), 
                col2 = rnorm(10000,102,15), 
                col3 = rnorm(10000,104,15), 
                col4 = rnorm(10000,106,15))

plot(density(df$col2), xlim = c(10,190))
points(density(df$col1), type = "l", col = "red")
points(density(df$col3), type = "l", col = "green")
points(density(df$col4), type = "l", col = "blue")

pnorm(2) # 97.7th percentile
pnorm(3) # 99.865th percentile

length(which(df$col1 >= 145)) # 16
length(which(df$col2 >= 145)) # 21
length(which(df$col3 >= 145)) # 32
length(which(df$col4 >= 145)) # 42

d2 = data.frame(col1 = rnorm(100000,100,15), 
                col2 = rnorm(100000,102,15), 
                col3 = rnorm(100000,104,15), 
                col4 = rnorm(100000,106,15))

plot(density(d2$col2), xlim = c(10,190))
points(density(d2$col1), type = "l", col = "red")
points(density(d2$col3), type = "l", col = "green")
points(density(d2$col4), type = "l", col = "blue")

length(which(d2$col1 >= 145)) # 126
length(which(d2$col2 >= 145)) # 197
length(which(d2$col3 >= 145)) # 332
length(which(d2$col4 >= 145)) # 464


plot(x = c(100,102,104,106),y = c(16,21,32,42), ylim = c(0,45))
plot(x = c(100,102,104,106),y = c(126,197,332,464))

dt = data.frame(popu = rep(c(10000,100000), each = 4),
                means = rep(c(100,102,104,106),2),
                u.3rd = c(16,21,32,42,126,197,332,464))

dt$popu = as.factor(dt$popu)
dt$means = as.factor(dt$means)

dt.ten = dt[which(dt$popu == 10000),]; dt.ten

ggplot(dt, aes(x = means, y = u.3rd)) + 
  geom_bar(stat = "identity") +
  facet_grid(facets = ~popu)

ggplot(dt.ten, aes(x = means, y = u.3rd)) +
  geom_bar(stat = "identity", width = .5)

dt.hun = dt[which(! dt$u.3rd %in% dt.ten$u.3rd),]; dt.hun

ggplot(dt.hun, aes(x = means, y = u.3rd)) +
  geom_bar(stat = "identity", width = .5)

100/4.6825
21.35*3.6825 + 21.35


pnorm(4) # 99.9968
length(which(d2$col1 >= 160)) # 4
length(which(d2$col2 >= 160)) # 8
length(which(d2$col3 >= 160)) # 14
length(which(d2$col4 >= 160)) # 17

###

dfs = NULL
for (i in 1:10){
  z = i
  dfs[i] = paste0("dfs",z)
  assign(dfs[i], data.frame(col1 = rnorm(100000,100,15), col2 = rnorm(100000,102,15), col3 = rnorm(100000,104,15), col4 = rnorm(100000,106,15)))
}

dflist1 = list()
for (i in 1:10){
  z = i
  sdfs = paste0("dfs",z)
  real.dfs = get(paste0("dfs",z))
  dflist1[[sdfs]] = real.dfs
}
# dflist1[[1]][1]

f = function(){
  for(i in 1:10){
    for (j in 1:4){
      foursds = length(which(dflist1[[i]][j] >= 160))
      print(foursds)
    }
  }
}
output = capture.output(f()); output
df.160s = data.frame(X1 = rep(c(100,102,104,106),10), X2 = output)
df.160s$X2 = gsub("\\[1] ","",df.160s$X2)
df.160s$X2 = as.numeric(df.160s$X2)
tapply(df.160s$X2, df.160s$X1, mean)
plot(as.factor(df.160s$X1),df.160s$X2)
plot(df.160s$X1,df.160s$X2)

cum.160s = rbind(cum.160s,df.160s)
t = tapply(cum.160s$X2, cum.160s$X1, mean); t # After 4 runs (4 million people total, 1 million in each distribution), the 102 mean is 200% the 100 mean
# par(mar = c(2,4,0,3))
plot(t, ylim = c(.7,20))

####################################################################
# my160s = data.frame(matrix(NA,nrow = 4, ncol = 10))
# f = function(){
#   for(i in 1:10){
#     for (j in 1:4){
#       my160s[j,i] = length(which(dflist[[i]][j] >= 160))
#     }
#   }
#   return(my160s)
# }
# f() # AT THIS POINT, IT LOOKS LIKE EVERYTHING WORKS. BUT IT'S ACTUALLY A DEAD END:
# output2 = capture.output(f()); output2 # THAT'S WEIRD
# my160s # UH ... WHAT HAPPENED TO THE NUMBERS???
# means = numeric(4)
# for (i in 1:4){
#   means[i] = mean(as.numeric(my160s[i,]))
# }
# means # DOESN'T WORK
####################################################################


### Tax brackets 2018

# 12% ... 19,051 to 77,400 ... 58349

# 22% ... 77,401 to 165,000 ... 87599

# 24% ... 165,001 to 315,000 ... 149999

# 32% ... 315,001 to 400,000 ... 84999

library(dplyr)
taxes = function(income){
  if (between(income,315001,400001)){
    remainder = income-315001
    tax = remainder*.32 + 58349*.12 + 87599*.22 + 149999*.24
    takehome = income-tax
  } else if (between(income,165001,315001)){
    remainder = income-165001
    tax = remainder*.24 + 58349*.12 + 87599*.22
    takehome = income-tax
  } else if (between(income,77401,165001)){
    remainder = income-77401
    tax = remainder*.22 + 58349*.12
    takehome = income-tax
  } else if (between(income,19051,77401)){
    remainder = income-19051
    tax = remainder*.12
    takehome = income-tax
  } else if (income < 19051){
    tax = "congratulations!"
    takehome = "get a better job"
  } else if (income > 400001){
    tax = "a lot"
    takehome = "you're rich"
  } 
  tax = prettyNum(tax, big.mark = ",", scientific = FALSE)
  takehome = prettyNum(takehome, big.mark = ",", scientific = FALSE)
  return(cat(sprintf("Your taxes are $%s \nAnd your takehome is $%s",tax,takehome)))
}



#########################################################################################
#########################################################################################

### ACCELERATED SEA LEVEL RISE - PNAS STUDY FEB. 2018 ###

######### ######### ######### ######### ######### ######### ######### ######### #########

level = 0
feetLevel = 0
rate = 3
years = 1993:2100; length(years) # 108 years
for (i in 1:length(years)){
  print(c(years[i],sprintf(" The level is %s mm. That is %s feet",level,feetLevel)))
  print(c(years[i],sprintf(" The rate is %s mm per year",rate)))
  level = level+rate
  rate = rate+.084
  feetLevel = level*0.00328084
} 
# Shows 2.6 feet of sea level rise from 1993 to 2100. The rate of rise in 2100 shown is 12 mm.

# Now let's try it with the more correct amount of 2.9 mm per year (mentioned in the Nerem et al. study's abstract):
level = 0
feetLevel = 0
rate = 2.9
years = 1993:2100; length(years) # 108 years
for (i in 1:length(years)){
  print(c(years[i],sprintf(" The level is %s mm. That is %s feet",level,feetLevel)))
  print(c(years[i],sprintf(" The rate is %s mm per year",rate)))
  level = level+rate
  rate = rate+.084
  feetLevel = level*0.00328084
} 
## And compare 2100 to 2005, which Nerem et al. also clarify is the interval from which they're
# getting the 65 cm figure.
786-40 # Still not down to 65 cm. It's 75 cm of sea level rise from 2005 to 2100.

library(ggplot2)
years = 1993:2150
no.acc.ft.lev = numeric(length(years))
no.acc.ft.lev[1] = 0
no.acc.rate = 2.9
no.acc.level = 0
for (i in 1:length(years)){
  no.acc.level = no.acc.level + no.acc.rate
  no.acc.ft.lev[i] = no.acc.level*.00328084
}
df.no.acc = data.frame(sea.level_no.acc = no.acc.ft.lev, Year = years)

feet.level = numeric(length(years))
feet.level[1] = 0
rate = 2.9
level = 0
for (i in 1:length(years)){
  level = level+rate
  rate = rate+.084
  feet.level[i] = level*.00328084
}
df.feet = data.frame(sea.level_feet = feet.level, Year = years)
ggplot(df.feet, aes(x = Year, y = sea.level_feet)) + geom_point() +
  geom_point(data = df.no.acc, mapping = aes(x = Year, y = sea.level_no.acc), col = "green")

#########################################################################################
#########################################################################################














