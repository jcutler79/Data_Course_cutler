### KJ OT research

load("/Users/jamescutler/Desktop/Data_Course_cutler/The_real_KJOT.RData")
otbooks = data.frame(xu = c(2,190399,354967,478545,649398,791827,889074,984933,997532,1122940,1226249,1349598,1466843,1573897,1709333,1748492,1803707,1833164,1924541,2141211,2218969,2246556,2259784,2447851,2664950,2682557,2882055,2942146,2968446,2978842,3000053,3003549,3009965,3025842,3032554,3040250,3048522,3054063,3086067),
                     xd = c(190399,354967,478545,649398,791827,889074,984933,997532,1122940,1226249,1349598,1466843,1573897,1709333,1748492,1803707,1833164,1924541,2141211,2218969,2246556,2259784,2447851,2664950,2682557,2882055,2942146,2968446,2978842,3000053,3003549,3009965,3025842,3032554,3040250,3048522,3054063,3086067,3094982),
                     yu = rep(0,39),
                     yd = rep(2,39),
                     livres = c("Genesis","Exodus","Leviticus","Numbers","Deuteronomy","Joshua",
                                "Judges","Ruth","1Samuel","2Samuel","1Kings","2Kings",
                                "1Chron","2Chron","Ezra","Nehemiah","Esther","Job","Psalms",
                                "Proverbs","Ecclesiastes","Song","Isaiah","Jeremiah",
                                "Lamentations","Ezekiel","Daniel","Hosea","Joel","Amos",
                                "Obadiah","Jonah","Micah","Nahum","Habakkuk","Zephaniah",
                                "Haggai","Zechariah","Malachi"))

livres = c("Genesis","Exodus","Leviticus","Numbers","Deuteronomy","Joshua","Judges","Ruth","1Samuel","2Samuel","1Kings","2Kings","1Chron","2Chron","Ezra","Nehemiah","Esther","Job","Psalms","Proverbs","Ecclesiastes","Song","Isaiah","Jeremiah","Lamentations","Ezekiel","Daniel","Hosea","Joel","Amos","Obadiah","Jonah","Micah","Nahum","Habakkuk","Zephaniah","Haggai","Zechariah","Malachi")
x = as.data.frame(sort(livres))
x$ordered = livres
x$colors = c("red","blue","green","yellow","purple","red","blue","green","brown1","yellow","brown3","yellow3","brown1","yellow","blue","red","green","purple","yellow","green","aquamarine","red","tan1","forestgreen","purple","orange","aquamarine","yellow","blue","red","green","darkblue","red","blue","yellow","green","orange","turquoise","hotpink")
y = x[,2:3]
y[order(y$ordered),]
y[order(y$ordered),2] # THIS IS WHAT NEEDS TO BE INSERTED INTO SCALE_FILL_MANUAL

library(ggplot2)

otStn = gregexpr(pattern = "Satan", theOT); length(otStn[[1]])
otSatan = data.frame(X = otStn[[1]][1:19], Y = rep(1,19)) # 14 in Job right on top of each other; 3 in Zechariah right on top of each other
ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
  scale_fill_manual(values = alpha(y[order(y$ordered),2],.3)) + 
  theme_classic() + geom_point(data = otSatan, mapping = aes(x = X, y = Y), shape = 3) +
  ggtitle("instances of 'Satan' (19)")

otatnment = gregexpr(pattern = "atonement", theOT); length(otatnment[[1]])
otatonement = data.frame(X = otatnment[[1]][1:81], Y = rep(1,81))
ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
  scale_fill_manual(values = alpha(y[order(y$ordered),2],.3)) + 
  theme_classic() + geom_point(data = otatonement, mapping = aes(x = X, y = Y), shape = 3) +
  ggtitle("instances of 'atonement' (81)")

otwsdm = gregexpr(pattern = "wisdom", theOT); length(otwsdm[[1]])
otwisdom = data.frame(X = otwsdm[[1]][1:170], Y = rep(1,170))
ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
  scale_fill_manual(values = alpha(y[order(y$ordered),2],.3)) + 
  theme_classic() + geom_point(data = otwisdom, mapping = aes(x = X, y = Y), shape = 3) +
  ggtitle("instances of 'wisdom' (170)")

otG = gregexpr(pattern = "God", theOT); length(otG[[1]])
otGod = data.frame(X = otG[[1]][1:2751], Y = rep(1,2751))
ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
  scale_fill_manual(values = alpha(y[order(y$ordered),2],.3)) + 
  theme_classic() + geom_point(data = otGod, mapping = aes(x = X, y = Y), shape = 3) +
  ggtitle("instances of 'God' (2751)")

otLrd = gregexpr(pattern = "LORD", theOT); length(otLrd[[1]])
otLORD = data.frame(X = otLrd[[1]][1:6620], Y = rep(1,6620))
ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
  scale_fill_manual(values = alpha(y[order(y$ordered),2],.3)) + 
  theme_classic() + geom_point(data = otLORD, mapping = aes(x = X, y = Y), shape = 3) +
  ggtitle("instances of 'LORD' (6620)")

otSoG = gregexpr(pattern = "Son of God", theOT); length(otSoG[[1]])
otSonofGod = data.frame(X = otSoG[[1]][1], Y = rep(1,1))
ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
  scale_fill_manual(values = alpha(y[order(y$ordered),2],.3)) + 
  theme_classic() + geom_point(data = otSonofGod, mapping = aes(x = X, y = Y), shape = 3) +
  ggtitle("instances of 'Son of God' (1)")

otAictp = gregexpr(pattern = "And it came to pass", theOT); length(otAictp[[1]])
otAndictpass = data.frame(X = otAictp[[1]][1:323], Y = rep(1,323))
ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
  scale_fill_manual(values = alpha(y[order(y$ordered),2],.3)) + 
  theme_classic() + geom_point(data = otAndictpass, mapping = aes(x = X, y = Y), shape = 3) +
  ggtitle("instances of 'And it came to pass' (323)")

gregexpr(pattern = "Son of God", theOT)
str_sub(theOT, 2897252-200,2897252+200)




############################################################################

### WORD FREQUENCY ###

############################################################################

# Putting all the steps together in an orderly way:

### Word frequency by book (in 7 steps):
# 1. create book bounds (Mrmn.b)
# 2. create individual books (titulos, which is a character list(?) of 16 vNames)
# 3. create individual book word vectors (Ltitulos, a character list(?) of 16 LvNames)
# 4. get a total word count for each book (Mrmn.wcount)
## 5. for every target word, get a target word count by book
## 6. for every target word, divide target count by total count to get frequency
## 7. for every target word, plot frequency

# Preliminaries:
load("/Users/jamescutler/Desktop/Data_Course_cutler/The_real_KJOT.RData")
livres = c("Genesis","Exodus","Leviticus","Numbers","Deuteronomy","Joshua","Judges","Ruth","1Samuel","2Samuel","1Kings","2Kings","1Chron","2Chron","Ezra","Nehemiah","Esther","Job","Psalms","Proverbs","Ecclesiastes","Song","Isaiah","Jeremiah","Lamentations","Ezekiel","Daniel","Hosea","Joel","Amos","Obadiah","Jonah","Micah","Nahum","Habakkuk","Zephaniah","Haggai","Zechariah","Malachi")
xu = c(2,190399,354967,478545,649398,791827,889074,984933,997532,1122940,1226249,1349598,1466843,1573897,1709333,1748492,1803707,1833164,1924541,2141211,2218969,2246556,2259784,2447851,2664950,2682557,2882055,2942146,2968446,2978842,3000053,3003549,3009965,3025842,3032554,3040250,3048522,3054063,3086067)
xd = c(190399,354967,478545,649398,791827,889074,984933,997532,1122940,1226249,1349598,1466843,1573897,1709333,1748492,1803707,1833164,1924541,2141211,2218969,2246556,2259784,2447851,2664950,2682557,2882055,2942146,2968446,2978842,3000053,3003549,3009965,3025842,3032554,3040250,3048522,3054063,3086067,3094982)

# 1. CREATE BOOK BOUNDS:
otbkbounds = data.frame(matrix(NA,nrow = 39, ncol = 3))
for (i in 1:39){
  otbkbounds[i,1] = livres[i]
  otbkbounds[i,2] = xu[i]
  otbkbounds[i,3] = xd[i]-1
}

# 2. CREATE INDIVIDUAL BOOKS (iBooks):
nome = NULL
for (i in 1:39){
  nome[i] = paste0("i",livres[i])
  assign(nome[i], str_sub(theOT, otbkbounds[i,2],otbkbounds[i,3]))
}
nome # These are the books

# 3. CREATE WORD VECTORS OF EACH BOOK (viBooks):
nom.vecs = NULL
for (i in 1:39){
  nom.vecs[i] = paste0("v",nome[i])
  assign(nom.vecs[i], unlist(strsplit(eval(parse(text = nome[i])), " ")))
}
nom.vecs # These are the books separated out into lists of their words (so the words can be counted)

# 4. TOTAL WORD COUNT FOR EACH BOOK:
otbk.wcount = NULL
for (i in 1:39){
  otbk.wcount[i] = length(eval(parse(text = nom.vecs[i])))
}
otbk.wcount

# STEPS 5 - 7:
OT.wfp = function(myword){
  gregop = NULL
  gregopl = numeric(39)
  for (i in 1:39){
    gregop[i] = gregexpr(pattern = myword, eval(parse(text = nome[i]))) # Find the word in each OT book
    if (gregop[[i]][1] < 0){
      gregopl[i] = 0
    } else{
      gregopl[i] = length(gregop[[i]])
    }
  }
  freqtheword = numeric(39)
  for (i in 1:39){
    freqtheword[i] = as.numeric(gregopl[i])/as.numeric(otbk.wcount[i])
  }
  freqtheword = as.numeric(freqtheword)
  ggplot(as.data.frame(freqtheword), aes(x = 1:39, y = freqtheword)) + geom_line() + 
    ggtitle(sprintf("frequency of '%s' by book",myword)) + 
    xlab("") + ylab("frequency") +
    scale_x_continuous(breaks = seq(1,39,1), labels = otbkbounds[,1]) + 
    theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9)) # +
  # coord_cartesian(ylim = c(0,.15))
}

#########################################################
# A function for just plotting target word count by book:
OT.wcp = function(myword){
  ot.i = NULL
  ot.il = numeric(39)
  for (i in 1:39){
    ot.i[i] = gregexpr(pattern = myword, eval(parse(text = nome[i])))
    if (ot.i[[i]][1] < 0){
      ot.il[i] = 0
    } else{
      ot.il[i] = length(ot.i[[i]])
    }
  }
  ggplot(as.data.frame(ot.il), aes(x = 1:39, y = ot.il)) + geom_line() +
    ggtitle(sprintf("how many times '%s' occurs in each book",myword)) + 
    xlab("") + ylab("number of hits") + 
    scale_x_continuous(breaks = seq(1,39,1), labels = otbkbounds[,1]) + 
    theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9))
}

#########################################################
# A function for plotting hits in the color rectangles:
# Preliminaries:
otbooks = data.frame(xu = c(2,190399,354967,478545,649398,791827,889074,984933,997532,1122940,1226249,1349598,1466843,1573897,1709333,1748492,1803707,1833164,1924541,2141211,2218969,2246556,2259784,2447851,2664950,2682557,2882055,2942146,2968446,2978842,3000053,3003549,3009965,3025842,3032554,3040250,3048522,3054063,3086067),
                     xd = c(190399,354967,478545,649398,791827,889074,984933,997532,1122940,1226249,1349598,1466843,1573897,1709333,1748492,1803707,1833164,1924541,2141211,2218969,2246556,2259784,2447851,2664950,2682557,2882055,2942146,2968446,2978842,3000053,3003549,3009965,3025842,3032554,3040250,3048522,3054063,3086067,3094982),
                     yu = rep(0,39),
                     yd = rep(2,39),
                     livres = c("Genesis","Exodus","Leviticus","Numbers","Deuteronomy","Joshua",
                                "Judges","Ruth","1Samuel","2Samuel","1Kings","2Kings",
                                "1Chron","2Chron","Ezra","Nehemiah","Esther","Job","Psalms",
                                "Proverbs","Ecclesiastes","Song","Isaiah","Jeremiah",
                                "Lamentations","Ezekiel","Daniel","Hosea","Joel","Amos",
                                "Obadiah","Jonah","Micah","Nahum","Habakkuk","Zephaniah",
                                "Haggai","Zechariah","Malachi"))

livres = c("Genesis","Exodus","Leviticus","Numbers","Deuteronomy","Joshua","Judges","Ruth","1Samuel","2Samuel","1Kings","2Kings","1Chron","2Chron","Ezra","Nehemiah","Esther","Job","Psalms","Proverbs","Ecclesiastes","Song","Isaiah","Jeremiah","Lamentations","Ezekiel","Daniel","Hosea","Joel","Amos","Obadiah","Jonah","Micah","Nahum","Habakkuk","Zephaniah","Haggai","Zechariah","Malachi")
x = as.data.frame(sort(livres))
x$ordered = livres
x$colors = c("red","blue","green","yellow","purple","red","blue","green","brown1","yellow","brown3","yellow3","brown1","yellow","blue","red","green","purple","yellow","green","aquamarine","red","tan1","forestgreen","purple","orange","aquamarine","yellow","blue","red","green","darkblue","red","blue","yellow","green","orange","turquoise","hotpink")
y = x[,2:3]
y[order(y$ordered),]
y[order(y$ordered),2]

# The function:
OT.whclrs = function(myword){
  gregop = gregexpr(pattern = myword, theOT)
  gregopl = length(gregop[[1]])
  ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
    scale_fill_manual(values = alpha(y[order(y$ordered),2], .3)) + 
    theme_classic() + xlab("") + ylab("") +
    geom_point(data = as.data.frame(gregop[[1]][1:gregopl]), mapping = aes(x = gregop[[1]][1:gregopl], y = rep(1,gregopl)), shape = 3) + 
    ggtitle(sprintf("instances of '%s' (%s)", myword, gregopl)) + theme(legend.position = "none")
}


###################

# GRID EXTRA
# library(gridExtra)

# e.g. - wisdom
wisdom.ot.f = OT.wfp("wisdom")
wisdom.ot.n = OT.wcp("wisdom")
wisdom.ot.clrs = OT.whclrs("wisdom")
grid.arrange(wisdom.ot.f,wisdom.ot.n,wisdom.ot.clrs)


# e.g. - commandment (obviously will include commandments, but not command)
gregexpr(pattern = "commandment", theOT)
commndmnt.ot.f = OT.wfp("commandment")
commndmnt.ot.n = OT.wcp("commandment")
commndmnt.ot.clrs = OT.whclrs("commandment")
grid.arrange(commndmnt.ot.f, commndmnt.ot.n, commndmnt.ot.clrs) # COMMANDMENT IS A DEUTERONOMY WORD (also Esther with reference to Ahasuerus)
# and "commanded"
commande.ot.f = OT.wfp("commande")
commande.ot.n = OT.wcp("commande")
commande.ot.clrs = OT.whclrs("commande")
grid.arrange(commande.ot.f, commande.ot.n, commande.ot.clrs) # COMMANDED IS AN EXODUS, JOSHUA, AND MAYBE JEREMIAH WORD


# covenant(s)
covenant.ot.f = OT.wfp("covenant")
covenant.ot.n = OT.wcp("covenant")
covenant.ot.clrs = OT.whclrs("covenant")
grid.arrange(covenant.ot.f,covenant.ot.n,covenant.ot.clrs)

# "conditions" doesn't appear in the OT. "conditions of repentance" is a BofM phrase (never appears in the NT)
gregexpr(pattern = "condition", theOT)
str_sub(theOT, 1033695-300,1033695+300)



