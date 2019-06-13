### Functions for NT, OT, and BofM

## Names of texts:
# bkofm
# theNT
# theOT

## Functions:
# wfp (word frequency plotting)
# wc (word count plotting)
# whclrs (word hits in the colors)
# Then gridextra (functions: g.NT, g.OT, g.BofM, and g.all.three)
# word.proximity (type in two words, a radius, and a book, spits out the passages)
##### I NEED TO GET WORD.PROXIMITY TO SPIT OUT GRAPHS TOO

## Then printouts of the surrounding context of the hits (function name: get.passages)
# e.g. - get.passages.2("Levite",theOT,todos = FALSE)
## Then simple word/phrase search functions, with a list of phrases/words that
# are unique to the Book of Mormon (not found in the Bible; no info on D&C or PofGP)
## And finally, simultaneous multiple word/phrase color plots (whclrs.9)

BofM.whclrs("come unto Christ")
g.all.three("come unto Christ")
g.all.three("follow Christ")

########################################

# Libraries:
library(stringr)
library(ggplot2)
library(gridExtra)
library(pracma) # for the strcmp function for comparing two strings to see if they're identical

########################################

#### The NT:
### Word frequency by book (in 7 steps):
# 1. create book bounds (NT.b)
# 2. create individual books (indbks, which is a character list(?) of 27 Names)
# 3. create individual book word vectors (Vindbks, a character list(?) of 27 vNames)
# 4. get a total word count for each book (NT.wcount)
## 5. for every target word, get a target word count by book
## 6. for every target word, divide target count by total count to get frequency
## 7. for every target word, plot frequency

# Preliminaries:
load("/Users/jamescutler/Desktop/Data_Course_cutler/The_real_KJNT.RData")
x1 = c(1,120395,196936,327243,421900,547914,596692,644181,675269,691055,706951,718216,728671,738112,743469,755827,764785,769792,772084,808362,820126,833234,841753,854154,855691,857272,860708)
x2 = c(120395,196936,327243,421900,547914,596692,644181,675269,691055,706951,718216,728671,738112,743469,755827,764785,769792,772084,808362,820126,833234,841753,854154,855691,857272,860708,920993)
libros = c("Matthew","Mark","Luke","John","Acts","Romans","1Cor","2Cor","Gal","Eph","Philipp","Col","1Thess","2Thess","1Tim","2Tim","Titus","Philemon","Hebrews","James","1Peter", "2Peter","1John","2John","3John","Jude","Revelation")

# 1.
NT.b = data.frame(matrix(NA, nrow = 27, ncol = 3))
for (i in 1:27){
  NT.b[i,1] = libros[i]
  NT.b[i,2] = x1[i]
  NT.b[i,3] = x2[i]-1
}

# 2.
indbks = NULL
for (i in 1:27){
  indbks[i] = paste0("i",libros[i])
  assign(indbks[i], str_sub(theNT, NT.b[i,2], NT.b[i,3]))
}
indbks

# 3. 
Vindbks = NULL
for (i in 1:27){
  Vindbks[i] = paste0("v",indbks[i])
  assign(Vindbks[i], unlist(strsplit(eval(parse(text = indbks[i])), " ")))
}

# 4. 
NT.wcount = NULL
for (i in 1:27){
  NT.wcount[i] = length(eval(parse(text = Vindbks[i])))
}
NT.wcount

###################################
         #### NT.wfp ####
###################################
# Function for steps 5-7:
NT.wfp = function(aword){
  gregop = paste0("n",as.character(aword))
  gregopl = paste0(gregop,".l")
  assign(gregop, NULL)
  assign(gregopl, numeric(27))
  for (i in 1:27){
    gregop[i] = gregexpr(pattern = aword, eval(parse(text = indbks[i])))
    if (gregop[[i]][1] < 0){
      gregopl[i] = 0
    } else{
      gregopl[i] = length(gregop[[i]])
    }
  }
  freqaword = paste0("freq.",aword)
  assign(freqaword, numeric(16))
  for (i in 1:27){
    freqaword[i] = as.numeric(gregopl[i])/as.numeric(NT.wcount[i])
  }
  freqaword = as.numeric(freqaword)
  ggplot(as.data.frame(freqaword), aes(x = 1:27, y = freqaword)) + geom_line() +
    ggtitle(sprintf("frequency of '%s' by book", aword)) + 
    xlab("") + ylab("frequency") + 
    scale_x_continuous(breaks = seq(1,27,1), labels = NT.b[,1]) +
    theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9))
}

###################################
         #### NT.wcp ####
###################################
# A function for just plotting target word count by book:
NT.wcp = function(myword){
  nt.i = NULL
  nt.il = numeric(27)
  for (i in 1:27){
    nt.i[i] = gregexpr(pattern = myword, eval(parse(text = indbks[i])))
    if (nt.i[[i]][1] < 0){
      nt.il[i] = 0
    } else{
      nt.il[i] = length(nt.i[[i]])
    }
  }
  ggplot(as.data.frame(nt.il), aes(x = 1:27, y = nt.il)) + geom_line() +
    ggtitle(sprintf("how many times '%s' occurs in each book",myword)) + 
    xlab("") + ylab("number of hits") + 
    scale_x_continuous(breaks = seq(1,27,1), labels = NT.b[,1]) + 
    theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9))
}

###################################
        #### NT.whclrs ####
###################################
# A function for plotting hits in the color rectangles:
# Preliminaries:
ntbooks = data.frame(x1 = c(1,120395,196936,327243,421900,547914,596692,644181,675269,691055,706951,718216,728671,738112,743469,755827,764785,769792,772084,808362,820126,833234,841753,854154,855691,857272,860708),
                     x2 = c(120395,196936,327243,421900,547914,596692,644181,675269,691055,706951,718216,728671,738112,743469,755827,764785,769792,772084,808362,820126,833234,841753,854154,855691,857272,860708,920993),
                     y1 = rep(0,length(x1)),
                     y2 = rep(2,length(x1)),
                     libros = c("Matthew","Mark","Luke","John","Acts","Romans","1Cor","2Cor","Gal","Eph","Philipp","Col","1Thess","2Thess","1Tim","2Tim","Titus","Philemon","Hebrews","James","1Peter", "2Peter","1John","2John","3John","Jude","Revelation"))

libros = c("Matthew","Mark","Luke","John","Acts","Romans","1Cor","2Cor","Gal","Eph","Philipp","Col","1Thess","2Thess","1Tim","2Tim","Titus","Philemon","Hebrews","James","1Peter", "2Peter","1John","2John","3John","Jude","Revelation")

NT.whclrs = function(myword){
  gregop = gregexpr(pattern = myword, theNT)
  gregopl = length(gregop[[1]])
  ggplot() + geom_rect(data = ntbooks, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = libros)) + 
    scale_fill_manual(values = alpha(c("deepskyblue","greenyellow","navyblue","steelblue","mediumpurple","slateblue1","darkgoldenrod1","goldenrod1","tan1","olivedrab1","aquamarine","yellow","blue","green","red","yellow","limegreen","purple","purple1","cyan","brown1","violet","olivedrab","coral","orangered","forestgreen","red"), .3)) + 
    theme_classic() + xlab("") + ylab("") +
    geom_point(data = as.data.frame(gregop[[1]][1:gregopl]), mapping = aes(x = gregop[[1]][1:gregopl], y = rep(1,gregopl)), shape = 3) + 
    ggtitle(sprintf("instances of '%s' (%s)", myword, gregopl)) + theme(legend.position = "none")
}

########################################################################################################


#### The OT:
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


###################################
        #### OT.wfp ####
###################################
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


###################################
         #### OT.wcp ####
###################################
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


###################################
       #### OT.whclrs ####
###################################
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

########################################################################################################


#### The BofM:
### Word frequency by book (in 7 steps):
# 1. create book bounds (Mrmn.b)
# 2. create individual books (titulos, which is a character list(?) of 16 vNames)
# 3. create individual book word vectors (Ltitulos, a character list(?) of 16 LvNames)
# 4. get a total word count for each book (Mrmn.wcount)
## 5. for every target word, get a target word count by book
## 6. for every target word, divide target count by total count to get frequency
## 7. for every target word, plot frequency

# Preliminaries:
load("/Users/jamescutler/Desktop/Data_Course_cutler/real_bofm.RData") # Load bkofm, the one with the multiple spaces problem fixed
books = data.frame(xa = c(1,1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169),
                   xb = c(1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169,1382696),
                   ya = rep(0,16),
                   yb = rep(2,16),
                   book = c("Title","1Nephi","2Nephi","Jacob","Enos","Jarom","Omni","WofM","Mosiah","Alma","Helaman","3Nephi","4Nephi","Mormon","Ether","Moroni"))

# 1. CREATE BOOK BOUNDS:
book = c("Title","1Nephi","2Nephi","Jacob","Enos","Jarom","Omni","WofM","Mosiah","Alma","Helaman","3Nephi","4Nephi","Mormon","Ether","Moroni")
xa = c(1,1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169)
xb = c(1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169,1382696)
Mrmn.b = data.frame(matrix(NA, nrow = 16, ncol = 3))
for (i in 1:16){
  Mrmn.b[i,1] = book[i]
  Mrmn.b[i,2] = xa[i]
  Mrmn.b[i,3] = xb[i]-1
}
# nchar(bkofm)
# str_sub(bkofm, 1382600,1382695) # The last character is a space, so it doesn't matter after all!

# 2. CREATE INDIVIDUAL BOOKS:
titulos = NULL
for (i in 1:16){
  titulos[i] = paste0("i",book[i])
  assign(titulos[i], str_sub(bkofm, Mrmn.b[i,2], Mrmn.b[i,3]))
}
titulos

# 3. CREATE WORD VECTORS OF EACH BOOK:
titulo.vecs = NULL
for (i in 1:16){
  titulo.vecs[i] = paste0("v",titulos[i])
  assign(titulo.vecs[i], unlist(strsplit(eval(parse(text = titulos[i])), " ")))
}
titulo.vecs

# 4. TOTAL WORD COUNT FOR EACH BOOK:
Mrmn.wcount = NULL
for (i in 1:16){
  Mrmn.wcount[i] = length(eval(parse(text = titulo.vecs[i])))
}
Mrmn.wcount

###################################
        #### BofM.wfp ####
###################################
# Function for doing steps 5-7:
BofM.wfp = function(theword){
  gregop = NULL
  gregopl = numeric(16)
  for (i in 1:16){
    gregop[i] = gregexpr(pattern = theword, eval(parse(text = titulos[i])))
    if (gregop[[i]][1] < 0){
      gregopl[i] = 0
    } else{
      gregopl[i] = length(gregop[[i]])
    }
  }
  freqtheword = numeric(16)
  for (i in 1:16){
    freqtheword[i] = as.numeric(gregopl[i])/as.numeric(Mrmn.wcount[i])
  }
  freqtheword = as.numeric(freqtheword)
  ggplot(as.data.frame(freqtheword), aes(x = 1:16, y = freqtheword)) + geom_line() + 
    ggtitle(sprintf("frequency of '%s' by book",theword)) + 
    xlab("") + ylab("frequency") +
    scale_x_continuous(breaks = seq(1,16,1), labels = Mrmn.b[,1]) + 
    theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9)) # +
  # coord_cartesian(ylim = c(0,.15))
}


###################################
        #### BofM.wcp ####
###################################
# A function for just plotting target word count by book:
BofM.wcp = function(myword){
  bm.i = NULL
  bm.il = numeric(16)
  for (i in 1:16){
    bm.i[i] = gregexpr(pattern = myword, eval(parse(text = titulos[i])))
    if (bm.i[[i]][1] < 0){
      bm.il[i] = 0
    } else{
      bm.il[i] = length(bm.i[[i]])
    }
  }
  ggplot(as.data.frame(bm.il), aes(x = 1:16, y = bm.il)) + geom_line() +
    ggtitle(sprintf("how many times '%s' occurs in each book",myword)) + 
    xlab("") + ylab("number of hits") + 
    scale_x_continuous(breaks = seq(1,16,1), labels = Mrmn.b[,1]) + 
    theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9))
}


###################################
      #### BofM.whclrs ####
###################################
# A function for plotting hits in the color rectangles:
# Preliminaries:
books = data.frame(xa = c(1,1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169),
                   xb = c(1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169,1382696),
                   ya = rep(0,length(xa)),
                   yb = rep(2,length(xa)),
                   book = c("Title","1Nephi","2Nephi","Jacob","Enos","Jarom","Omni","WofM","Mosiah","Alma","Helaman","3Nephi","4Nephi","Mormon","Ether","Moroni"))

# The function:
BofM.whclrs = function(myword){
  gregop = gregexpr(pattern = myword, bkofm)
  gregopl = length(gregop[[1]])
  ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
    scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"), .3)) + 
    theme_classic() + xlab("") + ylab("") +
    geom_point(data = as.data.frame(gregop[[1]][1:gregopl]), mapping = aes(x = gregop[[1]][1:gregopl], y = rep(1,gregopl)), shape = 3) + 
    ggtitle(sprintf("instances of '%s' (%s)", myword, gregopl)) + theme(legend.position = "none") + 
    geom_vline(xintercept = c(193650,231790))
}

########################################################################################################


# gridExtra for NT:
g.NT = function(aword){
  word.NT.f = NT.wfp(aword)
  word.NT.n = NT.wcp(aword)
  word.NT.clrs = NT.whclrs(aword)
  grid.arrange(word.NT.f,word.NT.n,word.NT.clrs)
}

# gridExtra for OT:
g.OT = function(aword){
  word.OT.f = OT.wfp(aword)
  word.OT.n = OT.wcp(aword)
  word.OT.clrs = OT.whclrs(aword)
  grid.arrange(word.OT.f,word.OT.n,word.OT.clrs)
}

# gridExtra for BofM:
g.BofM = function(aword){
  word.BofM.f = BofM.wfp(aword)
  word.BofM.n = BofM.wcp(aword)
  word.BofM.clrs = BofM.whclrs(aword)
  grid.arrange(word.BofM.f,word.BofM.n,word.BofM.clrs)
}

#########

# gridExtra for all books of scripture:
g.all.three = function(aword){
  gregop.nt = gregexpr(pattern = aword, theNT)
  gregopl.nt = length(gregop.nt[[1]])
  gregop.ot = gregexpr(pattern = aword, theOT)
  gregopl.ot = length(gregop.ot[[1]])
  gregop.bm = gregexpr(pattern = aword, bkofm)
  gregopl.bm = length(gregop.bm[[1]])
  g.NT(aword)
  g.OT(aword)
  g.BofM(aword)
  print(sprintf("NT:   %s",gregopl.nt))
  print(sprintf("OT:   %s",gregopl.ot))
  print(sprintf("BofM: %s",gregopl.bm))
}
# g.all.three("Gentile")


########################################################################################################

### All book titles:
## NT:
# The Gospel According to Saint Matthew
# The Gospel According to Saint Mark
# The Gospel According to Saint Luke
# The Gospel According to Saint John
# The Acts of the Apostles
# The Epistle of Paul to the Romans
# gregexpr(pattern = "The Epistle of Paul",theNT)

###################################
      #### get.passages ####
###################################
## Function for getting passage extracts containing target words in the three 
# books of scripture:
get.passages.2 = function(aword,abook,todos = TRUE){
  word.greg = gregexpr(pattern = aword,abook)
  if (isTRUE(todos)){
    for (i in 1:length(word.greg[[1]])){
      print(str_sub(abook, word.greg[[1]][i]-300,word.greg[[1]][i]+300))
    } 
  } else{
    comienzo = readline(prompt = "Where to start the range of passages: ")
    finish = readline(prompt = "Where to end the range of passages: ")
    for (i in comienzo:finish){
      print(str_sub(abook, word.greg[[1]][i]-300,word.greg[[1]][i]+300))
    }
  }
}
# NOW MAKE A FUNCTION THAT WILL ITERATIVELY FILL A LIST SO I CAN LOOK AT 
## WHICHEVER PASSAGE(S) IN THE LIST I WANT


########################################################################################################

## NT phrases in the Book of Mormon (order in which I find them):
# liberty wherewith
NT.phrases.in.BofM = list("liberty wherewith","Lamb of God","rumo(u)rs of wars")

## Phrases that are unique to the Book of Mormon (in alphabetical order):
# costly apparel
# dwindle
# probation - days of [my/your/his] probation; probationary state/time
# things pertaining unto righteousness
BofM.phrases = list("costly apparel","dwindle","day(s) of probation / probationary state/time","things pertaining unto righteousness")

BofM.notes.phrases = list(LambofGod = "'Lamb of God' is a John 1 phrase. It's not a Gospel of John phrase, generally. It is specifically a John chapter 1 phrase. It occurs twice there, and no where else in the entire New Testament--in the entire Bible. I had no idea this phrase was so isolated in the New Testament. And yet it's all over in the Book of Mormon. It appears 35 times! ~27 of which are in 1 Nephi! What is up with this??? Apart from Nephi, only Alma the Younger (in Alma 7) and Moroni (in Mormon 9) use this phrase--once for Alma and twice for Moroni. This is a very very strange phrase.")

## Simple word search in each book of scripture:
# The NT:
NT.search = function(aword){
  print(gregexpr(pattern = aword,theNT))
  word.greg = gregexpr(pattern = aword,theNT)
  how.many.hits = length(word.greg[[1]])
  print(how.many.hits)
}

# The OT:
OT.search = function(aword){
  print(gregexpr(pattern = aword,theOT))
  word.greg = gregexpr(pattern = aword,theOT)
  how.many.hits = length(word.greg[[1]])
  print(how.many.hits)
}

# The BofM:
BofM.search = function(aword){
  print(gregexpr(pattern = aword,bkofm))
  word.greg = gregexpr(pattern = aword,bkofm)
  how.many.hits = length(word.greg[[1]])
  print(how.many.hits)
}

# All three:
all.three.search = function(aword){
  NT.search(aword)
  OT.search(aword)
  BofM.search(aword)
}
# all.three.search("come unto Christ")
# get.passages.2("come unto Christ",bkofm)

## Complex (multiple simultaneous) word search colors plot:
# For the NT:
NT.whclrs.9 = function(myword1,myword2,myword3 = "no input",myword4 = "no input",myword5 = "no input",myword6 = "no input",myword7 = "no input",myword8 = "no input",myword9 = "no input"){
  gregop1 = gregexpr(pattern = myword1, theNT)
  gregop2 = gregexpr(pattern = myword2, theNT)
  if (myword3 == "no input"){
    gregop3 = gregexpr(pattern = "no input", theNT)
  } else{gregop3 = gregexpr(pattern = myword3, theNT)}
  if (myword4 == "no input"){
    gregop4 = gregexpr(pattern = "no input", theNT)
  } else{gregop4 = gregexpr(pattern = myword4, theNT)}
  if (myword5 == "no input"){
    gregop5 = gregexpr(pattern = "no input", theNT)
  } else{gregop5 = gregexpr(pattern = myword5, theNT)}
  if (myword6 == "no input"){
    gregop6 = gregexpr(pattern = "no input", theNT)
  } else{gregop6 = gregexpr(pattern = myword6, theNT)}
  if (myword7 == "no input"){
    gregop7 = gregexpr(pattern = "no input", theNT)
  } else{gregop7 = gregexpr(pattern = myword7, theNT)}
  if (myword8 == "no input"){
    gregop8 = gregexpr(pattern = "no input", theNT)
  } else{gregop8 = gregexpr(pattern = myword8, theNT)}
  if (myword9 == "no input"){
    gregop9 = gregexpr(pattern = "no input", theNT)
  } else{gregop9 = gregexpr(pattern = myword9, theNT)}
  gregopl1 = length(gregop1[[1]])
  gregopl2 = length(gregop2[[1]])
  gregopl3 = length(gregop3[[1]])
  gregopl4 = length(gregop4[[1]])
  gregopl5 = length(gregop5[[1]])
  gregopl6 = length(gregop6[[1]])
  gregopl7 = length(gregop7[[1]])
  gregopl8 = length(gregop8[[1]])
  gregopl9 = length(gregop9[[1]])
  Y = 1.8
  ggplot() + geom_rect(data = ntbooks, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = libros)) + 
    scale_fill_manual(values = alpha(c("deepskyblue","greenyellow","navyblue","steelblue","mediumpurple","slateblue1","darkgoldenrod1","goldenrod1","tan1","olivedrab1","aquamarine","yellow","blue","green","red","yellow","limegreen","purple","purple1","cyan","brown1","violet","olivedrab","coral","orangered","forestgreen","red"), .3)) + 
    theme_classic() + xlab("") + ylab("") +
    geom_point(data = as.data.frame(gregop1[[1]][1:gregopl1]), mapping = aes(x = gregop1[[1]][1:gregopl1], y = rep(Y,gregopl1)), shape = 1) + 
    geom_point(data = as.data.frame(gregop2[[1]][1:gregopl2]), mapping = aes(x = gregop2[[1]][1:gregopl2], y = rep(Y-.2,gregopl2)), shape = 2) +
    geom_point(data = as.data.frame(gregop3[[1]][1:gregopl3]), mapping = aes(x = gregop3[[1]][1:gregopl3], y = rep(Y-.4,gregopl3)), shape = 3) +
    geom_point(data = as.data.frame(gregop4[[1]][1:gregopl4]), mapping = aes(x = gregop4[[1]][1:gregopl4], y = rep(Y-.6,gregopl4)), shape = 4) +
    geom_point(data = as.data.frame(gregop5[[1]][1:gregopl5]), mapping = aes(x = gregop5[[1]][1:gregopl5], y = rep(Y-.8,gregopl5)), shape = 5) +
    geom_point(data = as.data.frame(gregop6[[1]][1:gregopl6]), mapping = aes(x = gregop6[[1]][1:gregopl6], y = rep(Y-1,gregopl6)), shape = 6) +
    geom_point(data = as.data.frame(gregop7[[1]][1:gregopl7]), mapping = aes(x = gregop7[[1]][1:gregopl7], y = rep(Y-1.2,gregopl7)), shape = 7) +
    geom_point(data = as.data.frame(gregop8[[1]][1:gregopl8]), mapping = aes(x = gregop8[[1]][1:gregopl8], y = rep(Y-1.4,gregopl8)), shape = 8) +
    geom_point(data = as.data.frame(gregop9[[1]][1:gregopl9]), mapping = aes(x = gregop9[[1]][1:gregopl9], y = rep(Y-1.6,gregopl9)), shape = 9) +
    ggtitle(sprintf("instances of '%s' (%s, circles),\n and of '%s' (%s, triangles),\n and of '%s' (%s, crosses),\n and of '%s' (%s, X's),\n and of '%s' (%s, diamonds),\n and of '%s' (%s, inverted triangle),\n and of '%s' (%s, crossed boxes),\n and of '%s' (%s, stars),\n and of '%s' (%s, crossed diamonds)", myword1, gregopl1, myword2, gregopl2,myword3, gregopl3,myword4, gregopl4,myword5, gregopl5,myword6, gregopl6,myword7, gregopl7,myword8, gregopl8,myword9, gregopl9)) + 
    theme(plot.title = element_text(size = 10),legend.position = "none")
}

# For the OT:
OT.whclrs.9 = function(myword1,myword2,myword3 = "no input",myword4 = "no input",myword5 = "no input",myword6 = "no input",myword7 = "no input",myword8 = "no input",myword9 = "no input"){
  gregop1 = gregexpr(pattern = myword1, theOT)
  gregop2 = gregexpr(pattern = myword2, theOT)
  if (myword3 == "no input"){
    gregop3 = gregexpr(pattern = "no input", theOT)
  } else{gregop3 = gregexpr(pattern = myword3, theOT)}
  if (myword4 == "no input"){
    gregop4 = gregexpr(pattern = "no input", theOT)
  } else{gregop4 = gregexpr(pattern = myword4, theOT)}
  if (myword5 == "no input"){
    gregop5 = gregexpr(pattern = "no input", theOT)
  } else{gregop5 = gregexpr(pattern = myword5, theOT)}
  if (myword6 == "no input"){
    gregop6 = gregexpr(pattern = "no input", theOT)
  } else{gregop6 = gregexpr(pattern = myword6, theOT)}
  if (myword7 == "no input"){
    gregop7 = gregexpr(pattern = "no input", theOT)
  } else{gregop7 = gregexpr(pattern = myword7, theOT)}
  if (myword8 == "no input"){
    gregop8 = gregexpr(pattern = "no input", theOT)
  } else{gregop8 = gregexpr(pattern = myword8, theOT)}
  if (myword9 == "no input"){
    gregop9 = gregexpr(pattern = "no input", theOT)
  } else{gregop9 = gregexpr(pattern = myword9, theOT)}
  gregopl1 = length(gregop1[[1]])
  gregopl2 = length(gregop2[[1]])
  gregopl3 = length(gregop3[[1]])
  gregopl4 = length(gregop4[[1]])
  gregopl5 = length(gregop5[[1]])
  gregopl6 = length(gregop6[[1]])
  gregopl7 = length(gregop7[[1]])
  gregopl8 = length(gregop8[[1]])
  gregopl9 = length(gregop9[[1]])
  Y = 1.8
  ggplot() + geom_rect(data = otbooks, aes(xmin = xu, xmax = xd, ymin = yu, ymax = yd, fill = livres)) + 
    scale_fill_manual(values = alpha(y[order(y$ordered),2], .3)) + 
    theme_classic() + xlab("") + ylab("") +
    geom_point(data = as.data.frame(gregop1[[1]][1:gregopl1]), mapping = aes(x = gregop1[[1]][1:gregopl1], y = rep(Y,gregopl1)), shape = 1) + 
    geom_point(data = as.data.frame(gregop2[[1]][1:gregopl2]), mapping = aes(x = gregop2[[1]][1:gregopl2], y = rep(Y-.2,gregopl2)), shape = 2) +
    geom_point(data = as.data.frame(gregop3[[1]][1:gregopl3]), mapping = aes(x = gregop3[[1]][1:gregopl3], y = rep(Y-.4,gregopl3)), shape = 3) +
    geom_point(data = as.data.frame(gregop4[[1]][1:gregopl4]), mapping = aes(x = gregop4[[1]][1:gregopl4], y = rep(Y-.6,gregopl4)), shape = 4) +
    geom_point(data = as.data.frame(gregop5[[1]][1:gregopl5]), mapping = aes(x = gregop5[[1]][1:gregopl5], y = rep(Y-.8,gregopl5)), shape = 5) +
    geom_point(data = as.data.frame(gregop6[[1]][1:gregopl6]), mapping = aes(x = gregop6[[1]][1:gregopl6], y = rep(Y-1,gregopl6)), shape = 6) +
    geom_point(data = as.data.frame(gregop7[[1]][1:gregopl7]), mapping = aes(x = gregop7[[1]][1:gregopl7], y = rep(Y-1.2,gregopl7)), shape = 7) +
    geom_point(data = as.data.frame(gregop8[[1]][1:gregopl8]), mapping = aes(x = gregop8[[1]][1:gregopl8], y = rep(Y-1.4,gregopl8)), shape = 8) +
    geom_point(data = as.data.frame(gregop9[[1]][1:gregopl9]), mapping = aes(x = gregop9[[1]][1:gregopl9], y = rep(Y-1.6,gregopl9)), shape = 9) +
    ggtitle(sprintf("instances of '%s' (%s, circles),\n and of '%s' (%s, triangles),\n and of '%s' (%s, crosses),\n and of '%s' (%s, X's),\n and of '%s' (%s, diamonds),\n and of '%s' (%s, inverted triangle),\n and of '%s' (%s, crossed boxes),\n and of '%s' (%s, stars),\n and of '%s' (%s, crossed diamonds)", myword1, gregopl1, myword2, gregopl2,myword3, gregopl3,myword4, gregopl4,myword5, gregopl5,myword6, gregopl6,myword7, gregopl7,myword8, gregopl8,myword9, gregopl9)) + 
    theme(plot.title = element_text(size = 10),legend.position = "none")
}
# OT.whclrs.9("God of Israel","Holy One of Israel","LORD","the LORD God","Almighty","the Lord","the gods")

# For the BofM:
BofM.whclrs.9 = function(myword1,myword2,myword3 = "no input",myword4 = "no input",myword5 = "no input",myword6 = "no input",myword7 = "no input",myword8 = "no input",myword9 = "no input"){
  gregop1 = gregexpr(pattern = myword1, bkofm)
  gregop2 = gregexpr(pattern = myword2, bkofm)
  if (myword3 == "no input"){
    gregop3 = gregexpr(pattern = "no input", bkofm)
  } else{gregop3 = gregexpr(pattern = myword3, bkofm)}
  if (myword4 == "no input"){
    gregop4 = gregexpr(pattern = "no input", bkofm)
  } else{gregop4 = gregexpr(pattern = myword4, bkofm)}
  if (myword5 == "no input"){
    gregop5 = gregexpr(pattern = "no input", bkofm)
  } else{gregop5 = gregexpr(pattern = myword5, bkofm)}
  if (myword6 == "no input"){
    gregop6 = gregexpr(pattern = "no input", bkofm)
  } else{gregop6 = gregexpr(pattern = myword6, bkofm)}
  if (myword7 == "no input"){
    gregop7 = gregexpr(pattern = "no input", bkofm)
  } else{gregop7 = gregexpr(pattern = myword7, bkofm)}
  if (myword8 == "no input"){
    gregop8 = gregexpr(pattern = "no input", bkofm)
  } else{gregop8 = gregexpr(pattern = myword8, bkofm)}
  if (myword9 == "no input"){
    gregop9 = gregexpr(pattern = "no input", bkofm)
  } else{gregop9 = gregexpr(pattern = myword9, bkofm)}
  gregopl1 = length(gregop1[[1]])
  gregopl2 = length(gregop2[[1]])
  gregopl3 = length(gregop3[[1]])
  gregopl4 = length(gregop4[[1]])
  gregopl5 = length(gregop5[[1]])
  gregopl6 = length(gregop6[[1]])
  gregopl7 = length(gregop7[[1]])
  gregopl8 = length(gregop8[[1]])
  gregopl9 = length(gregop9[[1]])
  Y = 1.8
  ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
    scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"), .3)) + 
    theme_classic() + xlab("") + ylab("") +
    geom_point(data = as.data.frame(gregop1[[1]][1:gregopl1]), mapping = aes(x = gregop1[[1]][1:gregopl1], y = rep(Y,gregopl1)), shape = 1) + 
    geom_point(data = as.data.frame(gregop2[[1]][1:gregopl2]), mapping = aes(x = gregop2[[1]][1:gregopl2], y = rep(Y-.2,gregopl2)), shape = 2) +
    geom_point(data = as.data.frame(gregop3[[1]][1:gregopl3]), mapping = aes(x = gregop3[[1]][1:gregopl3], y = rep(Y-.4,gregopl3)), shape = 3) +
    geom_point(data = as.data.frame(gregop4[[1]][1:gregopl4]), mapping = aes(x = gregop4[[1]][1:gregopl4], y = rep(Y-.6,gregopl4)), shape = 4) +
    geom_point(data = as.data.frame(gregop5[[1]][1:gregopl5]), mapping = aes(x = gregop5[[1]][1:gregopl5], y = rep(Y-.8,gregopl5)), shape = 5) +
    geom_point(data = as.data.frame(gregop6[[1]][1:gregopl6]), mapping = aes(x = gregop6[[1]][1:gregopl6], y = rep(Y-1,gregopl6)), shape = 6) +
    geom_point(data = as.data.frame(gregop7[[1]][1:gregopl7]), mapping = aes(x = gregop7[[1]][1:gregopl7], y = rep(Y-1.2,gregopl7)), shape = 7) +
    geom_point(data = as.data.frame(gregop8[[1]][1:gregopl8]), mapping = aes(x = gregop8[[1]][1:gregopl8], y = rep(Y-1.4,gregopl8)), shape = 8) +
    geom_point(data = as.data.frame(gregop9[[1]][1:gregopl9]), mapping = aes(x = gregop9[[1]][1:gregopl9], y = rep(Y-1.6,gregopl9)), shape = 9) +
    ggtitle(sprintf("instances of '%s' (%s, circles),\n and of '%s' (%s, triangles),\n and of '%s' (%s, crosses),\n and of '%s' (%s, X's),\n and of '%s' (%s, diamonds),\n and of '%s' (%s, inverted triangle),\n and of '%s' (%s, crossed boxes),\n and of '%s' (%s, stars),\n and of '%s' (%s, crossed diamonds)", myword1, gregopl1, myword2, gregopl2,myword3, gregopl3,myword4, gregopl4,myword5, gregopl5,myword6, gregopl6,myword7, gregopl7,myword8, gregopl8,myword9, gregopl9)) + 
    theme(plot.title = element_text(size = 10),legend.position = "none") +
    geom_vline(xintercept = c(193650,231790))
}
# BofM.whclrs.9("God of Israel","Holy One of Israel","the Lord God","Savior","Redeemer","Only Begotten","Son of God","Christ")



#####################################################################################
#####################################################################################
##### WORD PROXIMITY!!! #####

word.proximity = function(word1,word2,proximity,book){
  gregop1 = gregexpr(word1,book)
  gregop2 = gregexpr(word2,book)
  word1.word2 = data.frame(matrix(NA, nrow = length(gregop1[[1]]), ncol = length(gregop2[[1]])))
  for (i in 1:length(gregop1[[1]])){
    for (j in 1:length(gregop2[[1]])){
      word1.word2[i,j] = ifelse(abs(gregop1[[1]][i] - gregop2[[1]][j]) < proximity, 1, 0)
    }
  }
  thesums = vector()
  for (i in 1:length(gregop1[[1]])){
    thesums[i] = sum(word1.word2[i,])
  }
  theones = which(thesums == 1)
  for (i in 1:length(theones)){
    print(str_sub(book,gregop1[[1]][theones[i]]-200,gregop1[[1]][theones[i]]+200))
  }
}
word.proximity("hunger","thirst",40,theOT)
word.proximity("faith","works",60,theNT)
word.proximity("thirst","thee",20,theOT)
word.proximity("pertain","righteousness",20,bkofm)
get.passages.2("things of righteousness",bkofm)
word.proximity("pure","heart",20,theOT)
OT.whclrs("peace")
OT.whclrs("peacemaker")
NT.whclrs("adopt")
get.passages.2("adopt",theNT)

BofM.whclrs("month")
get.passages.2("month",bkofm)



########################################################################################################


# Searches (most recent at the top):

# 17 Feb 2019:
get.passages.2("gift of the Holy Ghost",bkofm,todos = TRUE)
get.passages.2("gift of the Holy Ghost",theNT,todos = TRUE)
BofM.whclrs.9("gift of the Holy Ghost","Holy Ghost")
NT.whclrs.9("gift of the Holy Ghost","Holy Ghost")

g.all.three("Holy Ghost")
g.all.three("contrite")
get.passages.2("contrite",bkofm,todos = TRUE)
word.proximity("contrite","broken",20,bkofm)



g.all.three("repent ")
g.all.three("Satan")
all.three.search("devil")
get.passages("Devil",theNT)
g.all.three("devil")
get.passages("devil",theOT)
g.NT("the devil ")
g.BofM("the devil ")
get.passages("Satan",theOT)
g.OT("Satan")
g.BofM("Satan")
thedevil.bm = get.passages("the devil ",bkofm); class(thedevil.bm)
g.all.three("house of Israel")
get.passages.2("house of Israel",theOT,todos = FALSE)
g.all.three("faith ")
g.NT("baptism") # 23
g.BofM("baptism") # 26
g.NT("bapti") # 100
g.BofM("bapti") # 145
get.passages.2("Bethabara",theNT)

g.all.three("Lamb of God")
get.passages.2("Lamb of God",bkofm,todos = FALSE)
get.passages.2("gospel",bkofm,todos = FALSE)
g.NT("Holy Ghost")
g.BofM("Holy Ghost")
get.passages.2("Holy Ghost",bkofm,todos = FALSE)
BofM.whclrs.9("Holy Ghost","the Spirit","Holy Spirit","the Spirit of the Lord")
NT.whclrs.9("Holy Ghost","the Spirit","Holy Spirit","the Spirit of the Lord")
g.OT("the Spirit of the")
get.passages.2("the Spirit of the",theOT,todos = FALSE)
g.OT("holy Spirit")
get.passages.2("holy Spirit",theOT)
get.passages.2("Holy Spirit",theNT)
g.NT("love")
g.OT("love")
g.NT("marriage")

get.passages.2("Gentiles",bkofm,todos = FALSE)

#########################################################################################################
BofMpsalms = data.frame(X = c(6855,31449,33925,35689,63613,139205,156776,1112987,1224702,1287475,154697,
                              154927,156019,156331,156482,158003,236605,541669,277619,281743,294125,297501,
                              299579,320212,381031,381622,428339,539268,539851,664338,538879,539008,539150,
                              539525,539731,558810,599378,599708,601627,598570,600876,607676,689310,687407,
                              688423,689211,694039,691618,695172,737452,740397,740397,761728,764245,1241588,
                              770390,770474,1126531,1138279,1162614,1190559,1357297,1380409),
                        Y = rep(1,63))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"), .3)) + 
  theme_classic() + xlab("") + ylab("") +
  geom_point(data = BofMpsalms, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("Where phrases from the Psalms are quoted in the Book of Mormon") + theme(legend.position = "none") + 
  geom_vline(xintercept = c(193650,231790))
# Psalms in the Book of Mormon:
BofM.search("will show unto you that the tender mercies of the Lord") # 6855
BofM.search("that they might leave me in the wilderness") # 31449
BofM.search("for there shall be none that doeth good")
#########################################################################################################

all.three.search("probation")
g.BofM("probation")
get.passages.2("probation",bkofm)

all.three.search("house of prayer")
g.NT("house of prayer")
g.OT("house of prayer")
get.passages.2("house of prayer",theOT)
get.passages.2("house of prayer",theNT)

g.NT("Lamb") # A John 1 word and a heavy book of Revelation word!!!!!!!!

g.BofM("the angel") # Nephi has the most lengthy interaction with "the angel" out of anyone in the BofM

all.three.search("rs of wars")
get.passages.2("rs of wars",theNT)
get.passages.2("rs of wars",bkofm)



OT.wfp("poor")
OT.wcp("poor")
gregexpr("poor", iPsalms)
get.passages.2("poor",iPsalms)
get.passages.2("poor",iProverbs)
OT.wcp("needy")
OT.wfp("needy")
get.passages.2("mourn",theOT)
OT.whclrs("mourn")
OT.whclrs("comfort")



####################################################
# Getting passages with two hit words within a certain distance from each other:
ot.comfort = gregexpr("comfort",theOT)
ot.mourn = gregexpr("mourn",theOT)
class(ot.comfort)
length(ot.comfort[[1]])
ifelse(abs(ot.comfort[[1]][2] - ot.mourn[[1]][1]) < 20,1,0)
# Okay, this is it:
comfort.mourn = data.frame(matrix(NA, nrow = 81, ncol = 121))
for (i in 1:81){
  for (j in 1:121){
    comfort.mourn[i,j] = ifelse(abs(ot.comfort[[1]][i] - ot.mourn[[1]][j]) < 20, 1, 0)
  }
}
for (i in 1:81){
  print(c(sum(comfort.mourn[i,]), i)) # You need to sum all the (121) numbers in a row, row by row; the sum will usually 1 at most, but more often zero
}
ot.comfort[[1]][c(29,57,62)]
str_sub(theOT,1892417-200,1892417+200)
str_sub(theOT,2432023-200,2432023+200)
str_sub(theOT,2507204-200,2507204+200)
####################################################

get.passages.2("the meek",iPsalms)
get.passages.2("hunger",iPsalms)
get.passages.2("hungry",iPsalms)
get.passages.2("thirst",iPsalms)
OT.wcp("righteousness")
get.passages.2("hunger",iIsaiah)

ot.hunger = gregexpr("hunger", theOT); length(ot.hunger[[1]])
ot.thirst = gregexpr("thirst", theOT); length(ot.thirst[[1]])
hunger.thirst = data.frame(matrix(NA, nrow = length(ot.hunger[[1]]), ncol = length(ot.thirst[[1]])))
for (i in 1:length(ot.hunger[[1]])){
  for (j in 1:length(ot.thirst[[1]])){
    hunger.thirst[i,j] = ifelse(abs(ot.hunger[[1]][i] - ot.thirst[[1]][j]) < 40, 1, 0)
  }
}
for (i in 1:length(ot.hunger[[1]])){
  print(c(sum(hunger.thirst[i,]), i)) 
}
ot.hunger[[1]][c(3,9)]
str_sub(theOT,763050-200,763050+200)
str_sub(theOT,2400825-200,2400825+200)
gregexpr("hunger and thirst",theOT)
gregexpr("hunger and thirst",theNT)
gregexpr("hunger and thirst",bkofm)
get.passages.2("hunger and thirst",theNT)
get.passages.2("hunger and thirst",bkofm)



#################################################################
################ ANALYSIS OF SEASONALITY OF WARS ################ 
1900 + (100 - (91-11)) # 1920
1900 + (100 - (91-19)) # 1928
1900 + (100 - (91-26)) # 1935
df.wars = data.frame(wars = c(1), dates = c("1920-02-05",
                                            "1928-11-10",
                                            "1935-01-01",
                                            ""))
df.mnth = data.frame(wars = rep(1,7), dates = c("2000/02/05",
                                                "2000/11/10",
                                                "2000/01/01",
                                                "2000/01/02",
                                                "2000/02/15",
                                                "2000/07/03",
                                                "2000/06/15"))
df.mnth$dates = as.Date(df.mnth$dates, '%Y/%m/%d')
rownames(df.mnth) = c("Ammonihah destroyed",
                      "Armies of Lamanites seen approaching",
                      "Lamanite army wakes up to dead Amalickiah",
                      "Moroni gets letter from Helaman",
                      "Jershonites send provisions to armies",
                      "Stripling warriors go against Lamanite army",
                      "Giddianhi's robbers go to war against Nephites")

ggplot(data = df.mnth, aes(dates,wars)) + 
  geom_point()

ggplot() +
  geom_point(data = df.mnth, mapping = aes(x = dates, y = wars)) +
  geom_text(data = df.mnth, mapping = aes(x = dates, y = wars, label = rownames(df.mnth)), 
            size = 2, angle = 45, vjust = -.1, hjust = -.1) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b", 
               limits = as.Date(c("2000/01/01","2000/12/31"))) +
  theme_classic()

library(dplyr)
thewars = sample(12,7)
length(which(between(thewars,5.1,9.1)))
thewars = vector()
for (i in 1:10000){
  roll = sample(12,7)
  thewars[i] = length(which(between(roll,5.1,9.1)))
}
# thewars
mean(thewars)


Ether = str_sub(bkofm,books[15,1],books[15,2])
strcmp(Ether,iEther)
gregexpr("anchor",bkofm)
g.BofM("anchor")
get.passages.2("anchor",bkofm)
get.passages.2("anchor",theNT)



#############################################################################

# Documentary Hypothesis - Anchor Bible Dictionary

# 3. Terminology
one = c("creation","P","J")
two = c("animals on ark","J","P")
three = c("lifespan","J","P")
four = c("Abraham's home","P"," ")
five = c("Benjamin's birthplace","E","P")
six = c("sale of Joseph","E","J")
seven = c("Jethro vs Reuel","E","J")
eight = c("God's name - Moses","P","J")
nine = c("construction of Tent","E","P")
ten = c("Decalog","P","J")
eleven = c("manna and birds","E","P")
twelve = c("faithful spies","J","P")
thirteen = c("Amalekites","J"," ")
fourteen = c("Korah's destruction","J","P")
fifteen = c("Israel at Peor","J","P")
terms = rbind(one,two,three,four,five,six,seven,eight,nine,ten,eleven,twelve,thirteen,
      fourteen,fifteen)
## TWO METHODS FOR FINDING HOW MANY J-P / P-J PAIRS THERE ARE:
# THE LONGER METHOD:
length(which(terms[,2] == "J" & terms[,3] == "P"))+
  length(which(terms[,2] == "P" & terms[,3] == "J"))
# THE SHORTER METHOD:
length(which(terms[,2] %in% c("J","P") & terms[,3] %in% c("J","P")))

length(which(terms[,2] %in% c("J","E") & terms[,3] %in% c("J","E")))

length(which(terms[,2] %in% c("E","P") & terms[,3] %in% c("E","P")))





# NEW RESEARCH METHODS ON THINGS IN THE OT, NT, AND BOFM WILL GO IN NEW NOTEPAD: SCRIPT.RESEARCH


