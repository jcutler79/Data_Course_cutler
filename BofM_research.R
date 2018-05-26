### Book of Mormon research

library(ggplot2)
library(stringr)

load("/Users/jamescutler/Desktop/Data_Course_cutler/bofm_word_vector.RData")
load("/Users/jamescutler/Desktop/Data_Course_cutler/real_bofm.RData") # Load bkofm, the one with the multiple spaces problem fixed

gregexpr(pattern = "THE FIRST BOOK OF NEPHI", bkofm) # 1381
gregexpr(pattern = "THE SECOND BOOK OF NEPHI", bkofm) # 129693
gregexpr(pattern = "THE BOOK OF JACOB", bkofm) # 280201
gregexpr(pattern = "THE BOOK OF ENOS", bkofm) # 327756
gregexpr(pattern = "THE BOOK OF JAROM", bkofm) # 333663
gregexpr(pattern = "THE BOOK OF OMNI", bkofm) # 337564
gregexpr(pattern = "THE WORDS OF MORMON", bkofm) # 344877
gregexpr(pattern = "THE BOOK OF MOSIAH", bkofm) # 349458
gregexpr(pattern = "THE BOOK OF ALMA", bkofm) # 510923
gregexpr(pattern = "THE BOOK OF HELAMAN", bkofm) # 953229
gregexpr(pattern = "THIRD NEPHI", bkofm) # 1060145
gregexpr(pattern = "FOURTH NEPHI", bkofm) # 1207552
gregexpr(pattern = "THE BOOK OF MORMON", bkofm) # 1217958
gregexpr(pattern = "THE BOOK OF ETHER", bkofm) # 1266461
gregexpr(pattern = "THE BOOK OF MORONI", bkofm) # 1351169
nchar(bkofm)

# The 2 Nephi Isaiah chapters:
str_sub(bkofm, 230000, 234000)
Isaiah.start = gregexpr(pattern = "The word that Isaiah the son", bkofm) # 193650
Isaiah.end = gregexpr(pattern = "the poor of his people shall trust in it", bkofm) # 231790 (231750+40)

xa = c(1,1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169)
xb = c(1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169,1382696)

books = data.frame(xa = c(1,1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169),
                   xb = c(1381,129693,280201,327756,333663,337564,344877,349458,510923,953229,1060145,1207552,1217958,1266461,1351169,1382696),
                   ya = rep(0,length(xa)),
                   yb = rep(2,length(xa)),
                   book = c("Title","1Nephi","2Nephi","Jacob","Enos","Jarom","Omni","WofM","Mosiah","Alma","Helaman","3Nephi","4Nephi","Mormon","Ether","Moroni"))

#######################################################################

JC = gregexpr(pattern = "Jesus Christ", bkofm)
JC[[1]][1:66]
JesChr = data.frame(X = JC[[1]][1:66], Y = rep(.2,66))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = JesChr, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'Jesus Christ'")

CHR = gregexpr(pattern = "Christ", bkofm)
CHR[[1]][1:391]
Christ = data.frame(X = CHR[[1]][1:391], Y = rep(.4, 391))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = Christ, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'Christ'")

J = gregexpr(pattern = "Jesus", bkofm)
J[[1]][1:184]
Jesus = data.frame(X = J[[1]][1:184], Y = rep(.6, 184))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = Jesus, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'Jesus'")
J[[1]][1]
str_sub(bkofm, 237693-200,237693+200)

# ALL THREE ("Christ", "Jesus", and "Jesus Christ"):
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + ggtitle("instances of 'Messiah' (diamonds), Jesus' (circles), \n'Christ' (crosses), and 'Jesus Christ' (X's)") + 
  geom_point(data = Christ, mapping = aes(x = X, y = Y), shape = 3) + 
  geom_point(data = JesChr, mapping = aes(x = X, y = Y), shape = 4) + 
  geom_point(data = Jesus, mapping = aes(x = X, y = Y), shape = 1) + 
  geom_point(data = Messiah, mapping = aes(x = X, y = Y), shape = 5)

G = gregexpr(pattern = "God", bkofm)  
length(G[[1]])
G[[1]][1:1667]
God = data.frame(X = G[[1]][1:1667], Y = rep(.8, 1667))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = God, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'God'")

Lrd = gregexpr(pattern = "Lord", bkofm)
Lrd[[1]][1:1572]
Lord = data.frame(X = Lrd[[1]][1:1572], Y = rep(.9,1572))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = Lord, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'Lord'")

tLG = gregexpr(pattern = "the Lord God", bkofm)
tLG[[1]][1:109]
tLrdGd = data.frame(X = tLG[[1]][1:109], Y = rep(.5,109))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = tLrdGd, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'the Lord God'") + geom_vline(xintercept = c(193650,231790))

HOI = gregexpr(pattern = "the Holy One of Israel", bkofm)
HOI[[1]][1:40]
HolyOne.I = data.frame(X = HOI[[1]][1:40], Y = rep(1,40))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = HolyOne.I, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'the Holy One of Israel'") + geom_vline(xintercept = c(193650,231790))

xpr.state = gregexpr(pattern = "state", bkofm)
xpr.state[[1]][1:78]
state = data.frame(X = xpr.state[[1]][1:78], Y = rep(1.1,78))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = state, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'state'") + geom_vline(xintercept = c(193650,231790))

anxty = gregexpr(pattern = "anxiety", bkofm)
anxty[[1]][1:8]
anxiety = data.frame(X = anxty[[1]][1:8], Y = rep(1.3,8))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = anxiety, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'anxiety'") + geom_vline(xintercept = c(193650,231790))
str_sub(bkofm, 134049-100, 134049+100) # Lehi
str_sub(bkofm, 164259-500, 164259+500) # Jacob speaking in 2 Nephi 6
str_sub(bkofm, 281342-100, 281342+100) # Jacob
str_sub(bkofm, 284775-100, 284775+100) # Jacob
str_sub(bkofm, 299784-100, 299784+100) # Jacob
str_sub(bkofm, 499259-100, 499259+100) # Mormon relating in Mosiah 28 that the people of Limhi had great anxiety to know what was on the 24 plates
str_sub(bkofm, 605625-100, 605625+100) # Alma - Alma 13
str_sub(bkofm, 1072370-100, 1072370+100) # Giddianhi

astnshd = gregexpr(pattern = "astonished", bkofm)
astnshd[[1]][1:25]
astonished = data.frame(X = astnshd[[1]][1:25], Y = rep(.9,25))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = astonished, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'astonished'") + geom_vline(xintercept = c(193650,231790))
str_sub(bkofm, astnshd[[1]][1]-100, astnshd[[1]][1]+100)

mrmr = gregexpr(pattern = "murmur", bkofm)
length(mrmr[[1]])
mrmr[[1]][1:33]
murmur = data.frame(X = mrmr[[1]][1:33], Y = rep(1,33))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = murmur, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'murmur'") + geom_vline(xintercept = c(193650,231790))

Mssh = gregexpr(pattern = "Messiah", bkofm)
length(Mssh[[1]])
Mssh[[1]][1:32]
Messiah = data.frame(X = Mssh[[1]][1:32], Y = rep(1,32))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = Messiah, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'Messiah'") + geom_vline(xintercept = c(193650,231790))
str_sub(bkofm, Mssh[[1]][32]-400,Mssh[[1]][32]+400)

tRoftJ = gregexpr(pattern = "the reign of the judges", bkofm)
length(tRoftJ[[1]])
tRoftJ[[1]][1:100]
ReignJudges = data.frame(X = tRoftJ[[1]][1:100], Y = rep(1,100))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = ReignJudges, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'the reign of the judges'") + geom_vline(xintercept = c(193650,231790))

aictp = gregexpr(pattern = "nd it came to pass", bkofm)
length(aictp[[1]])
aictp[[1]][1:1123]
aictpass = data.frame(X = aictp[[1]][1:1123], Y = rep(1,1123))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = aictpass, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'and it came to pass'") + geom_vline(xintercept = c(193650,231790))

gregexpr(pattern = "Son of man", bkofm) # nowhere found in the Book of Mormon
str_sub(bkofm, 172450-200,172450+200)

cstlyapp = gregexpr(pattern = "costly apparel", bkofm); cstlyapp
length(cstlyapp[[1]])
costlyapp = data.frame(X = cstlyapp[[1]][1:8], Y = rep(1,8))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = costlyapp, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'costl apparel'") + geom_vline(xintercept = c(193650,231790)) +
  theme(legend.position = "none")

apprl = gregexpr(pattern = "apparel", bkofm); apprl
length(apprl[[1]])
apparel = data.frame(X = apprl[[1]][1:14], Y = rep(1,14))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = apparel, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'apparel'") + geom_vline(xintercept = c(193650,231790)) +
  theme(legend.position = "none")
str_sub(bkofm, apprl[[1]][3]-200,apprl[[1]][3]+200)

drss = gregexpr(pattern = "dress", bkofm); drss
length(drss[[1]])
dress = data.frame(X = drss[[1]][1:6], Y = rep(1,6))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = dress, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'dress'") + geom_vline(xintercept = c(193650,231790)) +
  theme(legend.position = "none")
for (i in 1:6){
  print(str_sub(bkofm, drss[[1]][i]-200,drss[[1]][i]+200))
}

rsrt = gregexpr(pattern = "resort", bkofm); rsrt
length(rsrt[[1]])
resort = data.frame(X = rsrt[[1]][1:5], Y = rep(1,5))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = resort, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'resort(ed)'") + geom_vline(xintercept = c(193650,231790)) +
  theme(legend.position = "none")
for (i in 1:5){
  print(str_sub(bkofm, rsrt[[1]][i]-200, rsrt[[1]][i]+200))
}

rbbr = gregexpr(pattern = "robber", bkofm); rbbr
length(rbbr[[1]])
robber = data.frame(X = rbbr[[1]][1:62], Y = rep(1,62))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + geom_point(data = robber, mapping = aes(x = X, y = Y), shape = 3) + 
  ggtitle("instances of 'robber(s)'") + geom_vline(xintercept = c(193650,231790)) +
  theme(legend.position = "none")

thvs = gregexpr(pattern = "thieves", bkofm); thvs
thf = gregexpr(pattern = "thief", bkofm); thf
thieves_thief = data.frame(X = c(thvs[[1]][1:4],thf[[1]][1]), Y = rep(1.1,5))
ggplot() + geom_rect(data = books, aes(xmin = xa, xmax = xb, ymin = ya, ymax = yb, fill = book)) + 
  scale_fill_manual(values = alpha(c("blue","green","red","yellow","limegreen","purple","khaki","cyan","brown1","violet","olivedrab","coral","orangered","gold","darksalmon","springgreen"),.3)) +
  theme_classic() + 
  geom_point(data = robber, mapping = aes(x = X, y = Y), shape = 3) + 
  geom_point(data = thieves_thief, mapping = aes(x = X, y = Y), shape = 4) + 
  ggtitle("instances of 'thief(ves)' (Xs) and 'robber(s)' (crosses)") + geom_vline(xintercept = c(193650,231790)) +
  theme(legend.position = "none")
str_sub(bkofm, thf[[1]][1]-500, thf[[1]][1]+500)



#########################################################################################

                                 ### WORD FREQUENCY ###

#########################################################################################

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

## 5. TARGET-WORD-COUNT BY BOOK:
# The word 'unto':
# i.unto = NULL
# i.unto.l = numeric(16)
# for (i in 1:16){
#   i.unto[i] = gregexpr(pattern = "unto", eval(parse(text = titulos[i])))
#   if (i.unto[[i]][1] < 0){
#     i.unto.l[i] = 0
#   } else{
#     i.unto.l[i] = length(i.unto[[i]])
#   }
# }
# i.unto.l
# 
# ## 6. DIVIDE TARGET WORD COUNT BY TOTAL WORD COUNT TO GET FREQUENCY:
# # The word 'unto':
# freq.unto = NULL
# for (i in 1:16){
#   freq.unto[i] = i.unto.l[i]/Mrmn.wcount[i]
# }
# freq.unto
# freq.unto.df = data.frame(freq = freq.unto, X = 1:16)
# 
# ## 7. PLOT:
# # The word 'unto':
# freq.unto.p = ggplot(freq.unto.df, aes(x = X, y = freq)) + geom_line() +
#   ggtitle("frequency of 'unto' by book") + 
#   scale_x_continuous(breaks = seq(1,16,1), labels = Mrmn.b[,1]) +
#   theme(axis.text.x = element_text(size = 7, angle = 45, vjust = .9, hjust = .9))
# freq.unto.p

###########################################################

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

mcw = "the be to of and a in that have I it for not on with he as you do at this but his by
from they we say her she or an will my one all would there their what so up out if about
who get which go me when make can like time no just him know take people into year your
good some could them see other than then now look only come its over think also back
after use how our work first even because any these day most us"
mcw = unlist(strsplit(mcw, " "))
mcw10 = head(mcw,10)
length(mcw10)
mcw10[10]
mcw11_20 = mcw[11:20]; mcw11_20

# For loop for applying the function to many different words in one step:
for (i in 1:length(mcw10)){
  print(BofM.wfp(mcw10[i]))
}

for (i in 1:length(mcw11_20)){
  print(BofM.wfp(mcw11_20[i]))
}

#########################################################
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

#########################################################
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


########################################################################
ac = "A B C D E F G H I J K Q R S T U V"
abc = "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
vac = unlist(strsplit(ac, " "))
vabc = unlist(strsplit(abc, " "))
vabc[! vabc %in% vac]

Mormon = c(LvWofM,LvMosiah,LvAlma,LvHelaman,Lv3Nephi,Lv4Nephi,LvMormon)
head(Mormon, 30)
notMrmn = c(LvTitle,Lv1Nephi,Lv2Nephi,LvJacob,LvEnos,LvJarom,LvOmni,LvEther,LvMoroni)
head(notMrmn, 30)

Mrmnswrds = unique(Mormon[! Mormon %in% notMrmn])
grep("resort", Mrmnswrds)
Mrmnswrds[c(261,362)]

grep("watchmen", Lv3Nephi)
########################################################################

# Grid Extra
library(gridExtra)
commandment.bm.f = BofM.wfp("commandment")
commandment.bm.n = BofM.wcp("commandment")
commandment.bm.clrs = BofM.whclrs("commandment")
grid.arrange(commandment.bm.f,commandment.bm.n,commandment.bm.clrs)

# Satan
BofM.whclrs("Satan")
Satan = gregexpr(pattern = "Satan", bkofm); length(Satan[[1]])
for (i in 1:26){
  print(str_sub(bkofm, Satan[[1]][i]-300,Satan[[1]][i]+300))
}

# covenant(s)
covenant.bm.f = BofM.wfp("covenant")
covenant.bm.n = BofM.wcp("covenant")
covenant.bm.clrs = BofM.whclrs("covenant")
grid.arrange(covenant.bm.f,covenant.bm.n,covenant.bm.clrs)

# conditions (as in, "conditions of repentance")
conditions.bm.f = BofM.wfp("conditions")
conditions.bm.n = BofM.wcp("conditions")
conditions.bm.clrs = BofM.whclrs("conditions")
grid.arrange(conditions.bm.f,conditions.bm.n,conditions.bm.clrs)
# full phrase:
condofrep.bm.f = BofM.wfp("conditions of repentance")
condofrep.bm.n = BofM.wcp("conditions of repentance")
condofrep.bm.clrs = BofM.whclrs("conditions of repentance")
grid.arrange(condofrep.bm.f,condofrep.bm.n,condofrep.bm.clrs)
conditions = gregexpr(pattern = "conditions", bkofm); length(conditions[[1]])
condofrep = gregexpr(pattern = "conditions of repentance", bkofm)
for (i in 1:4){
  print(str_sub(bkofm, condofrep[[1]][i]-500,condofrep[[1]][i]+500))
}
str_sub(bkofm, condofrep[[1]][4]-600,condofrep[[1]][4]+600)

## So, "conditions of repentance" is a uniquely BofM phrase, and, it first gets used by Mormon
# in the BofM, even though people earlier than him, like Alma the younger, used it too. Why does
# it get worded that way, though, when the subject possessing the conditions should be an end to
# which repentance is the means--like salvation, which is what king Benjamin says in Mosiah 4:8.






