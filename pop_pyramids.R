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



############################################################################################



### Flash card app

emeBM = "1 Nephi 4:17, 2 Nephi 10:15, Alma 9:25 - 'for this cause that' = in order that
1 Nephi 6:3 - 'desire' = require
1 Nephi 7:15 - 'choice' = judgment, sound judgment, discernment
1 Nephi 8:12 - 'desirous' = desirable
1 Nephi 8:21, Alma 14:27 - 'obtain' = reach (a place)
1 Nephi 18:9 - 'to that' = until
1 Nephi 22:13 - 'turn upon' = fall upon
2 Nephi 1:26 - 'manifest' = expound, declare
Mosiah 3:19 (original manuscript says 'but if' instead of 'unless') - 'but if' = unless
Alma 1:9, 5:53, 8:13 - 'withstand ' = oppose, deny, contradict
Alma 7:5, 15:3 - 'by the cause of' = on account of, by reason of
Alma 11:2 - 'stripe' = whip, beat
Alma 11:25, 24:13, 59:19, 3 Nephi 3:10, Moroni 7:8 - 'retain' = hold back
Alma 37:37 (original manuscript says 'counsel the Lord'), 39:10 - 'counsel' = to consult, ask counsel of
Alma 44:7 - 'extinct' = dead (individual)
Alma 46:17 (original manuscript says 'gave' instead of 'named') - 'give' = describe
Alma 63:5 - 'curious' = ingenious
Helaman 1:15 - 'pitch' (battle) = set in array
Helaman 7:16 - 'hurl' = drag
Helaman 8:11 - 'depart' = divide
Helaman 9:17 - 'detect' = expose
3 Nephi 1:29 - 'became' = began to act
Ether 6:10 - 'mar' = hinder
Ether 6:10 - 'break' = stop
Ether 12:41 - 'commend' = recommend (to do a thing)
Moroni 10:26 - 'do away' = dismiss, reject
title page - 'scattered' = separated (from the main body)"
emeBM
vemeBM = unlist(strsplit(emeBM, "\n")); vemeBM
length(vemeBM)

BMabbrev = "tpage fN sN Jc E Jr O WM Ms A H tN fthN Mrm Eth Mni"

fN6_3 = c("desire","require")
fN7_15 = c("choice","judgment")
fN8_12 = c("desirous","desirable")
fN8_21_A14_27 = c("obtain","reach (a place)")
fN18_9 = c("to that","until")
fN22_13 = c("turn upon","fall upon")
sN1_26 = c("manifest","expound, declare")
Ms3_19 = c("but if","unless")
A1_9_A5_53_A8_13 = c("withstand","oppose, deny, contradict")
A7_5_A15_3 = c("by the cause of","on account of, by reason of")
A11_2 = c("stripe","whip, beat")
A11_25_A24_13_A59_19_tN3_10_Mni7_8 = c("retain","hold back")
A37_37_A39_10 = c("counsel","to consult, ask counsel of")
A44_7 = c("extinct","dead (of an individual)")
A46_17 = c("gave (instead of 'named')","give = describe")
A63_5 = c("curious","ingenious")
H1_15 = c("pitch (battle)","set in array")
H7_16 = c("hurl","drag")
H8_11 = c("depart","divide")
H9_17 = c("detect","expose")
tN1_29 = c("became","began to act")
Eth6_10a = c("mar","hinder")
Eth6_10b = c("break","stop")
Eth12_41 = c("commend","recommend (to do a thing)")
Mni10_26 = c("do away","dismiss, reject")
tpage_scattered = c("scattered","separated (from the main body)")


# 27 (26) items in the BMList flashcard deck:
BMList = list(one = "filler",
              fN6_3 = c("desire","require","And it mattereth not to me that I am particular to give a full account of all the things of my father, for they cannot be written upon these plates, for I desire the room that I may write of the things of God."),
              fN7_15 = c("choice","judgment","Now behold, I say unto you that if ye will return unto Jerusalem ye shall also perish with them. And now, if ye have choice, go up to the land, and remember the words which I speak unto you, that if ye go ye will also perish"),
              fN8_12 = c("desirous","desirable",""),
              fN8_21_A14_27 = c("obtain","reach (a place)","1 Nephi 8:21 - And I saw numberless concourses of people, many of whom were pressing forward, that they might obtain the path which led unto the tree by which I stood. Alma 14:27 - And it came to pass that so great was their fear that they fell to the earth, and did not obtain the outer door of the prison"),
              fN18_9 = c("to that","until","And after we had been driven forth before the wind for the space of many days, behold, my brethren and the sons of Ishmael and also their wives began to make themselves merry, insomuch that they began to dance, and to sing, and to speak with much rudeness, yea, even **to** that they did forget by what power they had been brought thither"),
              fN22_13 = c("turn upon","fall upon","And the blood of that great and abominable church, which is the whore of all the earth, shall turn upon their own heads; for they shall war among themselves, and the sword of their own hands shall fall upon their own heads"),
              sN1_26 = c("manifest","expound, declare","and that which ye call anger was the truth, according to that which is in God, which he could not restrain, manifesting boldly concerning your iniquities"),
              Ms3_19 = c("but if","unless","For the natural man is an enemy to God, and has been from the fall of Adam, and will be, forever and ever, **but if** he yields to the enticings of the Holy Spirit, and putteth off the natural man and becometh a saint through the atonement of Christ the Lord"),
              A1_9_A5_53_A8_13 = c("withstand","oppose, deny, contradict","1:9 - Now, because Gideon withstood him with the words of God he was wroth with Gideon, and drew his sword and began to smite him. 5:53 - And now my beloved brethren, I say unto you, can ye withstand these sayings; 8:13 - Now when the people had said this, and withstood all his words, and reviled him, and spit upon him, and caused that he should be cast out of their city, he departed thence"),
              A7_5_A15_3 = c("by the cause of","on account of, by reason of",""),
              A11_2 = c("stripe","whip, beat"),
              A11_25_A24_13_A59_10_tN3_10_Mni7_8 = c("retain","hold back"),
              A37_37_A39_10 = c("counsel","to consult, ask counsel of"),
              A44_7 = c("extinct","dead (of an individual)"),
              A46_17 = c("gave (instead of 'named')","give = describe"),
              A63_5 = c("curious","ingenious"),H1_15 = c("pitch (battle)","set in array"),
              H7_16 = c("hurl","drag"),
              H8_11 = c("depart","divide"),
              H9_17 = c("detect","expose"),
              tN1_29 = c("became","began to act"),
              Eth6_10a = c("mar","hinder"),
              Eth6_10b = c("break","stop"),
              Eth12_41 = c("commend","recommend (to do a thing)"),
              Mni10_26 = c("do away","dismiss, reject"),
              tpage_scattered = c("scattered","separated (from the main body)"))

flashcards.beta = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  while (continue != ""){
    inum = sample(1:length(yourlist),1)
    print(names(yourlist)[inum])
    show = readline(prompt = "Hit 'y' to show other side: ")
    if (show == "y"){
      print(yourlist[[inum]])
    } else{print("Puck you, then.")}
    continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  }
  print("Puck you.")
}



flashcards = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    inum = sample(mysamp[-inum.cum],1)
    print(c(i, names(yourlist)[inum]))
    show1 = readline(prompt = "Hit 'y' to show other side: ")
    if (show1 == "y"){
      print(yourlist[[inum]][1])
    } else{print("Alrighty, then.")}
    show2 = readline(prompt = "Hit 'y' to show the other other side: ")
    if (show2 == 'y'){
      print(yourlist[[inum]][2])
    } else{print("Uh ... WTF.")}
    inum.cum = c(inum.cum,inum)
    i = i+1
    continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  }
  print("F U.")
}

mysamp = 1:10
output = 1
output.cum = c(output)
while (length(output.cum) <= 9){
  output = sample(mysamp[-output.cum],1)
  print(output)
  output.cum = c(output.cum,output)
}

BMList[3] # gives you everything
BMList[[3]] # gives you just what's "in" scripture reference #3
BMList[[3]][2] # gives you just the second sub-item in contents of scripture reference #3

flashback = function(yourlist){
  continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  mysamp = 1:length(yourlist)
  inum = 1
  inum.cum = c(inum)
  i = 1
  while (continue != ""){
    inum = sample(mysamp[-inum.cum],1)
    print(c(i, yourlist[[inum]][2]))
    show1 = readline(prompt = "Hit 'y' to show other side: ")
    if (show1 == "y"){
      print(yourlist[[inum]][1])
    } else{print("Alrighty, then.")}
    show2 = readline(prompt = "Hit 'y' to show the other other side: ")
    if (show2 == 'y'){
      print(names(yourlist)[inum])
    } else{print("Uh ... WTF.")}
    inum.cum = c(inum.cum,inum)
    i = i+1
    continue = readline(prompt = "Hit 'y' to continue, enter to quit: ")
  }
  print("F U.")
}


####################################################################################

# 31 (30) items in the PharaohList flaschard deck:
PharaohList = list(one = "filler", 
                   BC3100a = c("Narmer","Pharaoh of the 1st dynasty in the Early Dynastic Period. First to unite the lands of upper and lower Egypt."),
                   BC3100b = c("Menes","Believed to be the same person as Narmer. Founded the 1st dynasty, according to Herodotus."),
                   BC2670 = c("Djoser","Pharaoh of the 3rd dynasty during the Old Kingdom. Built the step pyramid as part of his funeral complex at Saqqara. Later pharaohs thought his reign to be the beginning of pharaonic history."),
                   BC2613 = c("Sneferu","First pharaoh of the 4th dynasty, Old Kingdom. Built the first true pyramid, changed the orientation of the funerary complex to east-west. Buried in the Red Pyramid."),
                   BC2589 = c("Khufu","aka Greek 'Cheops' / 'Suphis'. 4th dynasty, Old Kingdom. Built the Great Pyramid, with burial chamber in the center rather than at the bottom of the pyramid. Depicted as a cruel tyrant by ancient Greek authors, but as a pious, generous ruler by [contemporary?] Egyptian sources. Main protagonist of the famous Westcar Papyrus. The first imprinted papyri originate from his reign."),
                   BC2558 = c("Khafre","aka Greek 'Cephren'. 4th dynasty, Old Kingdom. Second largest pyramid at Giza. Built the Great Sphinx."),
                   BC2460 = c("Neferefre","5th dynasty, Old Kingdom. His pyramid only reached its lowest courses. Records state he built a sun-temple. A store of papyri was discovered in his pyramid temple providing information about the economic, administrative, and religious practices of his time."),
                   BC2278 = c("Neferkare Pepi II","6th dynasty, Old Kingdom. His rule was Egypt's longest at 94 years. The first half of his reign was prosperous, the second half was a time of economic crisis."),
                   BC2180 = c("Neitiqerty Siptah and Nitocris","Last pharaoh of the 6th dynasty, Old Kingdom. The male king gave rise to the legendary queen mentioned by Herodotus and Manetho. Modern scholars doubt she ever existed."),
                   BC1971 = c("Kheperkare Senusret I","aka Sesostris I. 12th dynasty, Middle Kingdom. Built the White Chapel. Reigned during a time of peace with no records of military campaigns found to date. First pharaoh to begin irrigating the Faiyum. His statues show signs of his actual appearance, marking a new trend in Egyptian art."),
                   BC1878 = c("Sesostris III","12th dynasty, Middle Kingdom. Most powerful pharaoh of the 12th dynasty. The pharaoh of Abraham, according to John Gee. Also Amenemhat III, the next pharaoh, could have been a pharaoh of Abraham's day."),
                   BC1550 = c("Ahmose I","Founder of the 18th dynasty, beginning the New Kingdom. Began the reunification of Egypt after the second intermediate period. Fought battles throughout Egypt, Palestine and Kush as he sought to banish the Hyksos. Goods and artwork show a Minoan influence during this time. He built projects at Memphis and Thebes (his religious capital), especially at Karnak."),
                   BC1541 = c("Amenhotep I","18th dynasty, New Kingdom. Continued his father's building projects and military campaigns. The workmen at Deir el-Medina (the town of the builders of the tombs in the Valley of the Kings) worshipped him and his mother as their patron gods for centuries."),
                   BC1492 = c("Thutmose II","18th dynasty, New Kingdom. His wife, Hatshepsut, attempted to replace his name on monuments with hers. His son later tried to restore his father's name, resulting in conflicting information about this pharaoh's life. His mummy was found in the royal cache at the Temple of Hatshepsut, showing signs of diseases that caused his death."),
                   BC1479 = c("Hatshepsut","18th dynasty, New Kingdom. Second known female ruler of Egypt. Married her uncle. Ruled during the height of Egypt's power. Began her reign merely as regent because her son, the next heir, was still a child. She claimed to be the child of Amun and transformed herself into a king by wearing the symbols of kingship. Known for her building projects. Sent trade missions to the land of Punt and brought back various exotic goods."),
                   BC1458 = c("Thutmose III","18th dynasty, New Kingdom. Conducted military campaigns in the Levant, conquered most of Palestine. His mother's name and monuments were not dishonored until the end of his reign. Built many monuments."),
                   BC1425 = c("Amenhotep II","18th dynasty, New Kingdom. Completed the dishonoring of Hatshepsut's monuments to end any claims by her family to the right to rule. Built various temples, including one to worship Horemakhet, a god associated with the Great Sphinx."),
                   BC1401 = c("Thutmose IV","18th dynasty, New Kingdom. Son of Amenhotep II but not the crown prince. His unexpected rule was legitimized by the Dream Stele, according to which he had a vision of Horemakhet-Khepri-Ra-Atum, while sleeping under the shadow of the Great Sphinx, telling him to restore the Sphinx by removing the sand that had partially buried it, and he would be given the kingship."),
                   BC1390 = c("Amenhotep III","18th dynasty, New Kingdom. Harvests during his time were rich, and he later became a fertility god. Called his palace 'the gleaming Aten' and emphasized the worship of various solar deities. Built a large tomb in the Valley of the Kings and the Colossi of Memnon near his mortuary temple."),
                   BC1352 = c("Amenhotep IV","aka Akhenaten. 18th dynasty, New Kingdom. Came to the throne at a time when the priests of Amun were wealthy and powerful. Built a temple to Aten at Karnak during the first few years of his reign. Built a new capital at Akhetaten (now Amarna), changed his name, and declared Aten the only god in Egypt (described as monolatristic, henotheistic, or even quasi-monotheistic. Started what is known as the Amarna Period."),
                   BC1334 = c("Neferneferuaten","18th dynasty, New Kingdom. Many scholars believe she was Akhenaten's queen, Nefertiti, others that she was Meritaten, the daughter of Akhenaten and Nefertiti."),
                   BC1332 = c("Tutankhamun","18th dynasty, New Kingdom. Son of Akhenaten, became pharaoh at age 9. During the first year of his reign, he abandoned Amarna and restored the cults of the traditional deities. He restored the power of Thebes. Egyptologists found his treasures and body intact when they excavated his tomb in the 1920s."),
                   BC1292 = c("Ramesses I","Founded the 19th dynasty, New Kingdom. Succeeded Horemheb, who was born a commoner, and became a general during the Amarna Period, and who obliterated images of the Amarna pharaohs and destroyed buildings and monuments associated with them. And this pharaoh, who succeeded Horemheb, was also of non-royal birth. This pharaoh's reign marked the transition between the stabilizing reign of Horemheb and the rule of the powerful pharaohs of the 19th dynasty (in particular his son Seti I and grandson Ramesses II)."),
                   BC1290 = c("Seti I","19th dynasty, New Kingdom. Conducted military campaigns to raise money for his building projects. After the enormous social upheavals of Akhenaten's religious reform, Horemheb, Ramesses I and this pharaoh's main priority was to re-establish order in the kingdom and reaffirm Egypt's sovereignty over Canaan and Syria, by confronting the Hittites."),
                   BC1279 = c("Ramesses II (The Great)","19th, NK. Known as Ozymandias in the Greek sources (transliteration of his throne name, Usermaatre Setepenre - 'The justice of Re is powerful--chosen of Re'). One of the most powerful pharaohs of all time. He had at least 95 children. Reigned for 67 years. Built a massive tomb for his children in the Valley of the Kings. He usurped monuments made by older pharaohs by erasing their names and carving his own instead. He declared himself a god before the tenth year of his reign and outlived his 12 oldest sons. He is famous for the Battle of Kadesh, against the Hittites. During his reign, the Egyptian army is estimated to have totaled 100,000 men."),
                   BC1213 = c("Merenptah / Merneptah","19th dynasty, New Kingdom. 13th son of Ramesses II. The Israel Stele is a name commonly given to his victory stele in which he is said to have laid waste to 'ysriar'--the earliest apparent textual reference to Israel known to date, and the first from Egypt."),
                   BC1191 = c("Twosret","19th dynasty, New Kingdom. A female pharaoh, wife of Seti II. Third female pharaoh to rule during the New Kingdom."),
                   BC1186 = c("Ramesses III","20th dynasty, New Kingdom. Not a relative of the previous Ramesses's. After Twosret's death, there was a period of lawlessness that this pharaoh's father ended. This pharaoh had to fight various invaders trying to take advantage of Egypt's internal turmoil. He reorganized temple administrations and land allocations, giving one third of the farm land over to the temples, causing a food shortage which led to one of the first recorded strikes of the workers at Deir el-Medina. It also led to a weakening of the central government."),
                   BC530 = c("Cambyses II","1st Persian dynasty (Achaemenid Empire) = 27th dynasty, in the Late Period. First foreign (absentee) pharaoh, ruling from Persia."),
                   BC332 = c("Alexander the Great","In Egypt, he was portrayed as the son of Nectanebo II, the last pharaoh before the Persian conquest. His defeat of Darius was depicted as Egypt's salvation, 'proving' Egypt was still ruled by an Egyptian. The greatest city he founded was Alexandria, in Egypt. He was regarded as a liberator in Egypt. He was pronounced son of the deity Amun at the Oracle of Siwa Oasis in the Libyan desert. Henceforth, Alexander often referred to Zeus-Ammon as his true father, and after his death, currency depicted him adorned with the horns of a ram as a symbol of his divinity."),
                   BC51_30 = c("Cleopatra VII","Last pharaoh of the Ptolemaic dynasty. Rome was moving to invade Egypt but Mark Antony, her lover, helped her stand against Rome. Then Augustus invaded Egypt and killed Mark Antony. This pharaoh then committed suicide, marking the end of pharaonic Egypt."))


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


# 69 (68) dichotomous branchings in this tree of life:
TreeLifeList = list(one = "filler",
                    LUCA = c("Eubacteria","Archaea"),
                    Eubacteria = c("Alphaproteobacteria --> mitochondria","All the other bacteria"),
                    Archaea = c("Lokiarchaeota --> Eukarya","All the other archaea"),
                    Eukarya = c("Bikonta --> plants and chromalveolates","Unikonta --> Amoebazoa, Fungi, Choanozoa + Animalia"),
                    Animalia = c("Porifera, Placozoa, Cnidarians","Eumetazoa"),
                    Eumetazoa = c("Ctenophora","Bilateria"),
                    Bilateria = c("Xenacoelomorpha","Nephrozoa --> Kimberella (possibly a protostome?)"),
                    Nephrozoa = c("Protostomes (~million species alive today)","Deuterostomes (~70,000 species alive today--incliuding 66,000 vertebrates)"),
                    Protostomes = c("Spiralia","Ecdysozoa"),
                    Spiralia = c("Rotifers and Platyhelminthes","Mollusks (largest marine phylum--at 23% of all named marine organisms) and Annelids"),
                    Ecdysozoa = c("Priapulids and Kinorhynchs","Nematodes and Panarthropoda"),
                    Panarthropoda = c("Onychophora","Tardigrades and Arthropods"),
                    Mollusca = c("Gastropods (70k)","Bivalves (20k - clams, oysters, scallops, and mussels) and Cephalopods (900)"),
                    Annelida = c("earthworms","leeches and ragworms"),
                    Gastropods = c("snails","slugs"),
                    Cephalopods = c("squid","octopus (and other things: nautilus, cuttlefish"),
                    Deuterostomes = c("Ambulacraria","Chordates"),
                    Chordates = c("Cephalochordates","Olfactores"),
                    Olfactores = c("Urochordates","Vertebrates"),
                    Vertebrates = c("Cyclostomes","Gnathostomes"),
                    Gnathostomes = c("Chondrychthyes","Osteichthyes"),
                    Osteichthyes = c("Actinopterygii (and Teleostomes)","Sarcopterygii"),
                    Sarcopterygii = c("lobe-finned fish","Tetrapods"),
                    Tetrapods = c("Anamniotes (amphibians)","Amniotes"),
                    Amniotes = c("Sauropsids","Synapsids"),
                    Sauropsids = c("Diapsids","?"),
                    Diapsids = c("Lepidosaurs","Archosaurs"),
                    Lepidosaurs = c("lizards (and don't forget tuataras)","snakes"),
                    Archosaurs = c("Pterosaurs","Dinosaurs"),
                    Dinosaurs = c("Ornithischia","Saurischia"),
                    Ornithischia_ = c("Ceratops","Stegasaurs"),
                    Saurischia = c("Theropods (originated in Middle Triassic, ancestrally carnivorous, but also included other -vores; eventually gave rise to birds)","Sauropods"),
                    Sauropods = c("Brachiosaurus and Brontosaurus","Diplodocus and Apatosaurus"),
                    Synapsids = c("Eupelycaosauria","?"),
                    Therapsids = c("Biarmosuchia","Eutherapsids"),
                    Eutherapsids = c("Dinocephalia","Neotherapsids"),
                    Theriodontia = c("Gorgonopsid","Eutheriodontia"),
                    Cynodontia = c("Cynognathia","Probainognathia"),
                    Mammaliaformes = c("examples include Morganucodon, Docodonta --> Castorocauda, and Hadrocodium","Mammalia"),
                    Mammalia = c("Prototheria --> Monotremes","Theria"),
                    Theria = c("Metatheria --> Marsupials","Eutheria --> Placentals"),
                    Placentals = c("Atlantogenata","Boreoeutheria"),
                    Atlantogenata = c("Xenarthra","Afrotheria"),
                    Xenarthra = c("armadillos","anteaters and sloths"),
                    Afrotheria = c("elephant shrews, tenrecs, and aardvarks","elephants, hyraxes and manatees"),
                    Boreoeutheria = c("Laurasiatheria","Euarchontoglires"),
                    Laurasiatheria = c("Eulipotyphla --> moles, shrews, and hedgehogs","Scrotifera"),
                    Scrotifera = c("Chiroptera","Ferungulata"),
                    Ferungulata = c("Euungulata","Ferae"),
                    Euungulata = c("Cetartiodactyla","Perissodactyla"),
                    Cetartiodactyla = c("Cetaceans","Artiodactyls"),
                    Artiodactyls = c("e.g. pigs, peccaries, hippos, camels, llamas","e.g. giraffes, deer, cattle, buffalos, antelopes"),
                    Perissodactyla = c("horses, donkeys, zebras","rhinos ; and tapirs"),
                    Ferae = c("Pholidota --> pangolins","Carnivora"),
                    Carnivora = c("Feliforms","Caniforms"),
                    Feliforms = c("cats","mongooses; hyenas; civets"),
                    Caniforms = c("bears, wolves, wolverines/red pandas/skunks/etc.","Pinnipeds"),
                    Euarchontaglires = c("Glires ... and also treeshrews","Euarchonta"),
                    Glires = c("rodents","lagomorphs"),
                    Euarchonta = c("Dermoptera (flying lemurs, aka colugos)","Primates"),
                    Primates = c("Strepsirrhines --> lemurs and lorises","Haplorhines"),
                    Haplorhines = c("tarsiers","Simiiformes"),
                    Simiiformes = c("Platyrrhines","Catarrhines"),
                    Catarrhines = c("Cercopithicoidea","Hominoidea"),
                    Hominoidea = c("Hylobates --> gibbons","Hominidae"),
                    Hominidae = c("Ponginae","Homininae"),
                    Homininae = c("Gorillas","Hominini"),
                    Hominini = c("Pan","Homo"))
length(TreeLifeList)

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

## DE class

## y'' - 9y' + 14y = 3x^2 - 5sin(2x) + 7xe^(6x) ... Find all solutions. Good example of a [non?]-homogeneous problem
# This is example 6 on page 144. This is something we should know for the test.

## 4.6 - Variation of Parameters

# A2(x)y'' + A1(x)y' + A0(x)y = g(x)
# y'' + [A1(x)/A2(x)]y' + [A0(x)/A2(x)]y = g(x)/A2(x)
# y'' + P(x)y' + Q(x)y = f(x)
# y'' + P(x)y' + Q(x)y = 0

# y' + P(x)y = f(x)
# y' + P(x)y = 0
# ...
# y = ce^(-int[P(x)])

# Yp(x) = u1(x)y1(x) + u2(x)y2(x)
# Yp' = u1'y1 + u2y2' + u2'y2 + u2y2'
# Yp'' = u1''y1 + 2u1'y1' + u1y1'' +
#        u2''y2 + 2u2'y2' + u2y2''

# Cramer's Rule (linear algebra stuff)

## solve: y'' - 4y' + 4y = (x+1)e^(2x)


########################################################################################## 

## Ancient peoples known by two phonetically dissimilar names:

ancient.names = list(one = "filler",
                     Phrygians = c("Mushki","The Mushki were an Iron Age people of Anatolia who appear in sources from Assyria but not from the Hittites. Several authors have connected them with the Moschoi of Greek sources and the Georgian tribe of the Meskhi. Josephus Flavius identified the Moschoi with the Biblical Meshech. ***Assyrian sources identify the Western Mushki with the Phrygians,*** but Greek sources clearly distinguish between the Phrygians and the Moschoi. The Encyclopedia of Indo-European Culture notes that the Armenians according to Diakonoff, are then an amalgam of the Hurrian (and Urartians), Luvians and the Proto-Armenian Mushki (or Armeno-Phrygians) who carried their IE language eastwards across Anatolia.According to Greek mythographers,[9] the first Phrygian Midas had been king of the Moschi (Mushki), also known as Bryges (Brigi) in the western part of archaic Thrace.Assyrian sources from the 8th century BC speak of a king Mita of the Mushki, identified with king Midas of Phrygia. The Phrygians were an ancient Indo-European people, initially dwelling in the southern Balkans – according to Herodotus – under the name of Bryges (Briges), changing it to Phryges after their final migration to Anatolia, via the Hellespont. - Wikipedia 'Mushki' and 'Phrygians'"))


########################################################################################## 


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















