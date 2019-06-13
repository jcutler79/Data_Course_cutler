# The Doctrine and Covenants

# Source: http://www.sacred-texts.com/mor/dc/index.htm
## It was a zipped file. All I had to do was click on it in my downloads and it 
## turned into a txt file which I was able to import with readLines below:

# Libraries
library(tm)
library(stringr)

DC = readLines("/Users/jamescutler/Downloads/dc.txt")

?str_count
str_count(DC,"Christ")
str(DC)
DC2 = Corpus(VectorSource(DC))
str_count(DC2,"Christ")

str_count(DC[800:820],"repentance")
str_count(DC2,"come unto Christ")
?grep()

gregexpr("Christ",DC2)

grep("Christ",DC2)

nature = "Agricultural activities are also some of the largest contributors to human emissions of greenhouse gases. They account for roughly 25% of total emissions due to the use of fertilizers and the conversion of areas such as tropical forests to grow crops or raise livestock such as cattle. Agricultural threats to ecosystems will only increase as the world’s population continues to grow, according to the IPBES analysis."
grep("of",nature)
natvec = strsplit(nature, split = " ")
grep("of",natvec)
grepl("of",natvec)
grepl("bye",natvec)
grep("to",natvec)
gregexpr("to",natvec)
gregexpr("to",nature)
natvec

substr(nature, start = 1, stop =10)
substr(nature,1,91)
gregexpr("of",nature)
gregexpr(" to ",nature)

grepl(" to ",nature)
class(natvec)
str_match(DC2,"Jesus Christ")
str_count(DC2, "Jesus Christ")
str_length(DC2)
nchar(DC2[3])
str_length(DC2[[1]])

nchar(DC2)
str_length(DC2[[5]])
str_length(nature)
nchar(nature)


NT.whclrs("my rest")
OT.whclrs("my rest")
NT.whclrs.9("my rest","his rest")

OT.whclrs.9("his rest","my rest")
get.passages.2("his rest",theNT)
get.passages.2("my rest",theOT)

BofM.whclrs("come unto him")
get.passages.2("come unto him",bkofm)
get.passages.2("come unto him",theNT)
NT.whclrs("come unto him")
OT.whclrs("come unto thee")

NT.whclrs("come unto me")
get.passages.2("come unto me",theNT)


gregexpr("come unto Christ",DC2)

str_sub(DC2,79570-100,79570+100)


dcp = paste(DC2,sep = " ")


gregexpr("come unto Christ",dcp)
str_sub(dcp,79570-100,79570+100)
dcp = gsub("\n","",dcp)
str_sub(dcp,79570-100,79570+100)
gregexpr("come unto Christ",dcp)
str_sub(dcp,79437-200,79437+200)

nchar(dcp)[1]
nchar(dcp[1])
dcp2 = dcp[1]
str_sub(dcp2,79437-200,79437+200)

OT.whclrs("pray")
get.passages.2("pray",theOT,todos = FALSE)

BofM.whclrs.9("follow Christ","follow him","follow your","following your","follow me")
NT.whclrs.9("follow Christ","follow him","follow your","following your","follow me")

gregexpr("come unto",dcp2)
str_count(dcp2,"come unto")


s10 = gregexpr("because you delivered up those writings",dcp2)[[1]][1]
s20 = gregexpr("The rise of the Church of Christ",dcp2)[[1]][1]
s30 = gregexpr("you have feared man and have not relied on me",dcp2)[[1]][1]
s40 = gregexpr("the heart of my servant James",dcp2)[[1]][1]
s50 = gregexpr("according as ye have asked and are agreed as touching the church",dcp2)[[1]][1]
s60 = gregexpr("return speedily to the land from whence they came",dcp2)[[1]][1]
s70 = gregexpr("and also unto my servant Sidney Rigdon",dcp2)[[1]][1]
s80 = gregexpr("that cometh under the sound of your voice",dcp2)[[1]][1]
s90 = gregexpr("according to thy petition",dcp2)[[1]][1]
s100= gregexpr("my friends Sidney and Joseph",dcp2)[[1]][1]
s110= gregexpr("The veil was taken from our minds",dcp2)[[1]][1]
s120= gregexpr("it shall be disposed of by a council",dcp2)[[1]][1]
s130= gregexpr("shall appear we shall see him as he is",dcp2)[[1]][1]

mytext = paste0("s",seq(10,130,10))
mytext
sections = vector()
for (i in 1:length(mytext)){
  sections[i] = eval(parse(text = mytext[i]))
}
sections
dfsections = data.frame(sections = sections)

DC.whclrs = function(myword){
  gregop = gregexpr(pattern = myword, dcp2)
  gregopl = str_count(dcp2,pattern = myword)
  ggplot() + 
    theme_classic() + labs(title = sprintf("instances of '%s' (%s)",myword,gregopl),
                           x="",y="") +
    geom_point(data = as.data.frame(gregop[[1]][1:gregopl]),
               mapping = aes(x = gregop[[1]][1:gregopl],
                             y = rep(1,gregopl)),
               shape = 3) +
    geom_text(data = dfsections, 
              mapping = aes(x=sections,y=rep(1.25,nrow(dfsections)), 
                            label = as.character(seq(10,130,10))),
              size = 3, angle = 45) +
    coord_cartesian(ylim = c(0,2))
}
DC.whclrs("cometh unto me")

get.passages.2("cometh unto",dcp2)

BofM.whclrs("come unto Christ")
get.passages.2("come unto Christ",bkofm)

BofM.whclrs.9("come unto me","come unto him","come unto Christ")
get.passages.2("come unto me",bkofm)[[1]][1]
get.passages.2("come unto me",bkofm)

NT.whclrs.9("come unto me","Come unto me","cometh unto me","cometh to me")
get.passages.2("come unto me",theNT)

BofM.whclrs.9("come unto me","Come unto me","cometh unto me","cometh to me")
DC.whclrs("cometh unto me")
get.passages.2("cometh unto me",dcp2)



DC.whclrs.9 = function(myword1,myword2,myword3 = "no input",myword4 = "no input",
                       myword5 = "no input",myword6 = "no input",myword7 = "no input",
                       myword8 = "no input",myword9 = "no input"){
  gregop1 = gregexpr(pattern = myword1, dcp2)
  gregop2 = gregexpr(pattern = myword2, dcp2)
  if (myword3 == "no input"){
    gregop3 = gregexpr(pattern = "no input", dcp2)
  } else{gregop3 = gregexpr(pattern = myword3, dcp2)}
  if (myword4 == "no input"){
    gregop4 = gregexpr(pattern = "no input", dcp2)
  } else{gregop4 = gregexpr(pattern = myword4, dcp2)}
  if (myword5 == "no input"){
    gregop5 = gregexpr(pattern = "no input", dcp2)
  } else{gregop5 = gregexpr(pattern = myword5, dcp2)}
  if (myword6 == "no input"){
    gregop6 = gregexpr(pattern = "no input", dcp2)
  } else{gregop6 = gregexpr(pattern = myword6, dcp2)}
  if (myword7 == "no input"){
    gregop7 = gregexpr(pattern = "no input", dcp2)
  } else{gregop7 = gregexpr(pattern = myword7, dcp2)}
  if (myword8 == "no input"){
    gregop8 = gregexpr(pattern = "no input", dcp2)
  } else{gregop8 = gregexpr(pattern = myword8, dcp2)}
  if (myword9 == "no input"){
    gregop9 = gregexpr(pattern = "no input", dcp2)
  } else{gregop9 = gregexpr(pattern = myword9, dcp2)}
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
  ggplot() +
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
    ggtitle(sprintf("instances of '%s' (%s, circles),\n and of '%s' (%s, triangles),\n and of '%s' (%s, crosses),\n and of '%s' (%s, X's),\n and of '%s' (%s, diamonds),\n and of '%s' (%s, inverted triangle),\n and of '%s' (%s, crossed boxes),\n and of '%s' (%s, stars),\n and of '%s' (%s, crossed diamonds)", myword1, gregopl1, myword2, gregopl2,myword3, gregopl3,myword4, gregopl4,myword5, gregopl5,myword6, gregopl6,myword7, gregopl7,myword8, gregopl8,myword9, gregopl9))
}
  





######################################################

# D&C
DC.whclrs.9("come unto him","Come unto him","cometh unto him","cometh to him","come to him")
get.passages.2("come unto him",dcp2)

DC.whclrs.9("come unto me","Come unto me","come to me","cometh unto me","cometh to me")
get.passages.2("come unto me",dcp2)
get.passages.2("cometh unto me",dcp2)


# NT
NT.whclrs.9("come unto him","Come unto him","cometh unto him","cometh to him","come to him")
get.passages.2("come unto him",theNT)
get.passages.2("come to him",theNT)

NT.whclrs.9("come unto me","Come unto me","come to me","cometh unto me","cometh to me")
get.passages.2("come unto me",theNT) # suffer the children
get.passages.2("come to me",theNT)
get.passages.2("cometh to me",theNT)
get.passages.2("Come unto me",theNT)


# BofM
BofM.whclrs.9("come unto him","Come unto him","cometh unto him","cometh to him","come to him")
get.passages.2("come unto him",bkofm)
get.passages.2("come to him",bkofm)

BofM.whclrs.9("come unto me","Come unto me","come to me","cometh unto me","cometh to me")
get.passages.2("come unto me",bkofm)
get.passages.2("Come unto me",bkofm)
get.passages.2("cometh unto me",bkofm)



###### A FEW MORE VARIATIONS:

get.passages.2("come unto the Father",dcp2)
get.passages.2("come unto the Father",theNT)
get.passages.2("come unto the Father",bkofm)

get.passages.2("come unto thee",dcp2)
get.passages.2("come unto thee",theNT)
get.passages.2("come unto thee",bkofm)

get.passages.2("come unto the God",bkofm)
get.passages.2("come unto the Holy One",bkofm)
get.passages.2("come unto the God",theNT)
get.passages.2("come unto the God",dcp2)

get.passages.2("come unto God",bkofm)
get.passages.2("come unto God",theNT)
get.passages.2("come unto God",dcp2)

get.passages.2("come unto the Lord",bkofm)
get.passages.2("come unto the Lord",theNT)
get.passages.2("come unto the Lord",dcp2)




#####################












