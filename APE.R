######################################## 
               # APE #
########################################

### QUESTIONS:
## 1. What is "ultrametric"? e.g., the drop.tip function drops tips in a way that
# keeps "the tree ultrametric in the case that it was beforehand."
## 2. How do you make a tree longer (horizontally stretched out) to fill the full
# width of the plot space, so there's enough space for the edge, tip, and node
# labels?
## 3. scan() and 1: ... 14: - see line 471 and in the book 1053/4091



### Chapter 2
n = 5
str(n)
ls()
5:-5
x = .Last.value; x
x[5]
x[6]
x[7]
x[length(x)-1]
tail(x,2)
head(x,3)
x[length(x)/2]
length(x)/2
z = c('order','family','genus','species')
z
z[c(TRUE,FALSE,TRUE,TRUE)]
f = c("male","male","male"); f
f = factor(f)
f
ff = factor(f, levels = c("male","female")); ff
table(f)
table(ff)

## read.table and as.is - see location 294/4091
## A matrix is a vector arranged in a tabular way. It is a vector with an
# additional attribute called dim (dimensions) which is itself a numeric
# vector with length 2, and defines the numbers of rows and columns of 
# the matrix.
nine = matrix(1:9, 3, 3, byrow = TRUE); nine
x = 1:9; x
dim(x) = c(9,1); x
x[3,2]
x[3,2, drop = FALSE]
x[,2, drop = FALSE]
x[,2] # WHOA. FASCINATING ... Paradis says that extracting a column or a
# row from a matrix returns a vector and not a single row or column matrix.
# This is because [] returns an object of the lowest possible dimension.
# So, drop = FALSE makes it return a row or column matrix.
rownames(x) = c('A','B','C')
colnames(x) = c('v1','v2','v3')
x

## The only difference between a matrix and a data frame is that a df
# can have vectors and/or factors of different modes or classes.

df = data.frame(z, y = 0:3, 4:1); df
L = list(z = z, 1:2, df)
L
length(L)
names(L)
L[[1]]
L[[2]]
L[[3]]
L$z
str(L[[1]])
str(L[1]) # NOTE THE SUBTLE BUT CRUCIAL DIFFERENCE BETWEEN [[]] AND []???

# 2.4 - Graphics
# There's a dev.print() function?

# 2.5 - Saving and restoring R data
# There's a command called history() to get all your commands

# 2.6 - R functions
# summary is a generic function; summary.phylo and summary.lm are methods

# 2.7 - Repeating commands
# source(mytreeplot.R) example !!!!!!

# 2.7.2 - apply-like functions
## apply
## lapply - for lists
## sapply is like lapply but returns its results in a more friendly way as 
# a vector or matrix with rownames and colnames
## rapply is a recursive version of lapply for lists with lists of objects
# that applys FUN to the non-list elements. Results returned as a vector 
# unless how = "replace" is used.
## tapply acts on a vector and applies a function on subsets defined by 
# INDEX.
## WHOA: by is an insanely cool apply function!!!!
## aggregate is the same as by!!!!!!
## replicate is cool!!!!!!


Archaea = c("Crenarchaea","Euryarchaea","Actinobacteria")
Bacteria = c("Cyanobacteria","Spirochaetes","Acidobacteria")
TofL = list(Archaea = Archaea,Bacteria = Bacteria, Eukaryotes)
TofL$Archaea
TofL$Bacteria
Eukaryotes = c("Alveolates","Cercozoa","Plants","Opisthokonts")
TofL$Eukaryotes = Eukaryotes
TofL

### 1.3.1 - Installations

install.packages("ape") # Error
library(ape)
install.packages("ade4")
library(ade4)
library(seqinr)
# For Emacs: http://www.gnu.org/software/emacs/emacs.html 
# For ESS: http://ess.r-project.org/



### Chapter 3 - Phylogenetic Data in R

## An object of class "phylo" is a list with the following components:
# edge - a two-column matrix where each row

## APE uses a class called phylo to store phylogenetic trees. The principle 
# of its design is to store in different elements a description of its 
# hierarchical structure, the names of the taxa, the branch lengths, and 
# other information that may be necessary.

## APE has another class called "matching".

## The package stats has two classes relevant here: hclust and dendrogram.
# These are designed to code hierarchical clusters. They thus contain less
# info than the two classes mentioned above.

## The package clue extends a wide range of clustering methods.

## Know the components of the list of an object of class "phylo":
# edge
# edge.length
# tip.label
# Nnode
# node.label
# root.edge

## phylo4 and S4 vs S3

## matching: The idea is to assign to each tip and node a positive number,
# and then to represent the topology as a series of pairs of these numbers
# that are siblings (the matchings). Interestingly(?), if some conventions
# are given, this results in a unique representation between a given tree
# and a given matching.
## Components of an object of class "matching":
# matching - a three-column matrix where the first two columns represent the 
# sibling pairs, and the third one the corresponding ancestor.
# tip.label
# node.label
# etc.

## The class "treeshape" (apTreeshape)

## The class "haploNet"

## Networks are less common than trees in evolutionary studies, perhaps 
# because of the difficulty to draw them graphically.

## Splits (self-explanatory)
# The optional attribute "number" gives the number of times each partition 
# was observed (e.g., in a bootstrap analysis; p. 171)
# For the four taxa A,B,C,D, the split AD|BC will be numbered 6 (0110?)

## Molecular sequences
## The class "DNAbin" (ape)

## The class "haplotype" (pegas)

## The class "alignment" (seqinr)
# nb - the number of sequences
# seq - sequences as a vector of mode character where each sequence is a 
# single string, and all strings have the smae number of characters.
# nam - vector of mode character giving the individual labels

## The class "phyDat" (phangorn)
# The sequences must be aligned, and only the patterns of variation among 
# sequences are stored.


### 3.1.5 - Allelic Data

## The class "loci" (pegas)

## The class "genind" (adegenet)

## The classes "SNPbin" and "genlight" (adegenet)

## Phenotypic data


### 3.2 - Reading Phylogenetic Data

## The agreed upon format for trees: the nested parantheses format, known as 
# the Newick or New Hampshire format.
## ape has two functions to read trees in Newick and NEXUS formats: read.tree
# and read.nexus:
# tr = read.tree("treefile.tre")
# trx = read.nexus("treefile.nex")
## Both read.tree and read.nexus create an object of class phylo.


### 3.2.2 - Molecular Sequences

## DNA can be read with the ape function read.dna, which reads files in 
# FASTA, Clustal, interleaved, or sequential format. It returns the data as
# an object of class "DNAbin".

## read.GenBank reads GenBank sequences; accession numbers are used as names
# for the individual sequences. species.names = TRUE for species names instead.

# ...


### 3.2.4 - Reading data over the internet

## e.g., the data file compiled by Ernest containing values of life history traits
# of non-flying placental mammals. For clarity, we first store the URL in a string:
a = "http://www.esapubs.org/archive/ecol/E084/093/" 
b = "Mammal_lifehistories_v2.txt"
ref = paste(a, b, sep = "")
X = read.delim(ref)
names(X)
X$order
paste(X$Genus, X$species, sep = "_")[1:10]

############## LARGE DATABASES OF PROTEINS ############## 
## Know what the RCSB is (Research Collaboratory for Structural Bioinformatics)
## Know that it's cool that you can create a handy function for reading any of the
# PDB files on the RCSB website

## Sanger Institute example of reading in data:
a = "http://pfam.sanger.ac.uk/family/tree/" 
b = "download?alnType=meta$acc=PF01607"
ref = paste(a, b, sep = ""); ref
sanger.tr = read.tree(ref) # Pfam is no longer supported at the Wellcome Sanger Institute
# MAYBE TRY:
a = "https://pfam.xfam.org/family/"
# b = "PF01607" # I NEED TO FIND OUT WHERE THE ACTUAL FILE IS. THIS IS STILL JUST A PLACE TO CLICK ON FURTHER LINKS ABOUT THE PROTEIN
ref = paste(a, b, sep = "")
sanger.tr = read.tree()
######################################################## 


### 3.4 Manipulating Data

## Results can be visualized with plot instead of write.tree (write.tree???)
## ape has several functions to manipulate 'phylo' objects:
# drop.tip - removes tips from a tree
# extract.clade
# drop.fossil
# bind.tree
# rotate

tr.prac = read.tree(text = "((a:1,b:1):1,(c:1,d:1):1);")
write.tree(tr.prac)
plot(tr.prac)
plot(drop.tip(tr.prac, c("a","b")))
write.tree(drop.tip(tr.prac, c("a","b")))
plot(drop.tip(tr.prac, 3:4, trim.internal = FALSE))
write.tree(extract.clade(tr.prac, node = 6)) # SO THAT'S HOW THEY'RE NUMBERED!!!



### 3.8.1 - Sylvia warblers

## 26 species of warblers of the genus Sylvia. They sequenced the gene of the
# cytochrome b for these species; the sequences were deposited in GenBank and
# have accession numbers AJ534526-AJ534549 and Z73494:
library(ape)
x = paste("AJ5345", 26:49, sep = "")
x = c("Z73494",x)
x
sylvia.seq = read.GenBank(x); sylvia.seq # 25 1100-bp sequences
sylvia.clus = clustal(sylvia.seq) # Error: cannot find executable 'clustalw2'
# on your computer. Place the executable of Clustal in a directory on the PATH
# of your computer: /bin, /sbin, usr/bin, usr/local/bin, etc.
image(sylvia.clus) # HOLY
# library(devtools)
# install_github("fmichonneau/phyloch")
# library(phyloch)
# library(ips)
# sylvia.maff = mafft(sylvia.seq) # ERROR!!! sh: /usr/local/bin/mafft: No such file or directory

taxa.sylvia = attr(sylvia.seq, "species")
names(taxa.sylvia) = names(sylvia.seq)
taxa.sylvia[c(1,24)]
taxa.sylvia[1] = "Sylvia_atricapilla"
class(taxa.sylvia)
sylvia.eco = read.table("sylvia_data.txt")
taxa.sylvia["AJ534526"]
?word
word(taxa.sylvia, end = 2, sep = "_")
sylvia.eco = read.table("/Users/jamescutler/Desktop/Data_Course_cutler/sylvia_data.txt")
sylvia.eco
str(sylvia.eco)
rownames(sylvia.eco)
taxa.sylvia
# save(sylvia.clus, taxa.sylvia, sylvia.eco, file = "sylvia.RData")
getwd()
setwd("/Users/jamescutler/Desktop/")
getwd()
save(sylvia.clus, taxa.sylvia, sylvia.eco, file = "sylvia.RData") # ACTUALLY, BETTER TO JUST SAVE EACH THING SEPARATELY
load("sylvia.RData")

### Mammalian mito genomes:
## Link to Andrew Gibson's 2005 paper, "A Comprehensive Analysis of Mammalian Mitochondrial Genome Base Composition and Improved Phylogenetic Methods"
# https://academic.oup.com/mbe/article/22/2/251/963859

# library(Biostrings)
# setwd("/Users/jamescutler/Desktop/APER2_Online_Material/")
# m.mt.fastas = dir(getwd(), full.names = TRUE, pattern = ".fasta")
# readDNAStringSet(m.mt.fastas) # DOESN'T WORK!!!
# m.mt.fastas
mamm.mt.taxa = read.table("/Users/jamescutler/Desktop/APER2_Online_Material/mammal_mtGenome.fasta", 
                          skip = 4, nrows = 233, sep = "_", comment.char = "(", as.is = TRUE) # HOW TO MAKE SENSE OF ALL THIS GIBBERISH??

mamm.mt.taxa[1:5,]
mamm.mt.taxa$V3 = NULL
mamm.mt.taxa$V1 = gsub("#", "", mamm.mt.taxa$V1)
mamm.mt.taxa$V1 = gsub(" : ", "", mamm.mt.taxa$V1)
colnames(mamm.mt.taxa) = c("code","species")
library(ape)
mt.mamm = read.dna("/Users/jamescutler/Desktop/APER2_Online_Material/mammal_mtGenome.fasta",
                   format = "fasta", skip = 239) # WHY THE SKIP = 239????

## Okay, so, one thing is mamm.mt.taxa, and another is mt.mamm. mt.mamm is the DNAbin of the 
# actual sequences of the genes. mamm.mt.taxa is just a bunch of species names with their 
# corresponding abbreviated form (code).

###############################################################################
a.mistake = c("I love asdjf food a lot??")
a.simple = c("I love food a lot.")
a.string = c("The 234 man 567 in8910the asdlfkj yellow hat")
thestrng = c("jcut79 gmail.net(ATP6)","yomama89 gmail.com(ATP6)","45yomama yahoo.com(ATP6)")
mystrng = c("@jcut79@gmeil.net(ATP6)@","@yomama89@gmail.com(ATP6)@","@45yomama@yahoo.com(ATP6)@")
mys.2 = gsub("\\d","",mystrng); mys.2
mys.2 = gsub("\\D","",mystrng); mys.2
my2 = gsub("\\s","@",thestrng); my2 # '\\s' ONLY WORKS IF YOU'RE LOCATING A SPACE, NOT TRYING TO REPLACE WITH A SPACE, DUH--JUST DO A " " TO ADD A SPACE
a.2 = gsub("\\w","foobar",a.string); a.2 # WEIRD ...
a.2 = gsub("\\w", "!",a.simple); a.2 # ... OKAY, so it replaces each letter in every word with whatever you tell it, not replacing the punctuation.
a.m2 = gsub("\\w", "1",a.mistake); a.m2 # doesn't matter if it's not technically a real word. Anything with letters has the letters replaced
a.m3 = gsub("\\W","1",a.mistake); a.m3 # even spaces are replaced
mys.2 = gsub("@$","!",mystrng); mys.2 # IT ONLY WORKS IF YOU PUT THE $ LAST!
mys.3 = gsub("\\@$","!",mystrng); mys.3 # the double backslashes are superfluous:
identical(mys.2,mys.3) # superfluous!
mys.4 = gsub("^@","!",mystrng); mys.4 # IT ONLY WORKS IF YOU PUT THE CARROT FIRST!
mys.5 = gsub("/(7|8)9/","49",mystrng); mys.5 # DOESN'T WORK! DOESN'T WORK ON NUMBERS APPARENTLY
mys.5 = gsub("/(e|a)i/","u",mystrng); mys.5 # TO SEE WHY THIS DOESN'T WORK, SEE BELOW FOR THE OBVIOUS CORRECT WAY: ...
str_locate(mystrng,"gmail")
mys.5 = gsub("meil|mail","whale",mystrng); mys.5 # ... THE CORRECT WAY TO USE THE ALTERNATION SYMBOL
mys.5b = gsub("mail","whale",mystrng); mys.5b
new.strng = c("??hi my name is","what's your name?","9alpha","10what?")

foo = paste("AJ79er",1:10, sep = ""); foo
bar = c(paste(foo[1:5],"(COX3)",sep = ""),paste(foo[6:10],"(ATP6)",sep = "")); bar
new.bar = gsub("^[[:alnum:]]{1,}\\(", "", bar); new.bar # ONLY WORKS WHEN THERE ARE DOUBLE BRACKETS
num.bar = gsub("^[[:alpha:]]{1,}[[:digit:]]", "", bar); num.bar # TOOK OUT THE 7
num.bar.good = gsub("^[[:alpha:]]{1,}", "", bar); num.bar.good # JUST TAKES OUT THE ALPHABETICAL
trimthat = gsub(")$", "", bar); trimthat # WORKED WITHOUT THE DOUBLE BACKSLASH
AJer = gsub("79", "", bar); AJer # DUH!!!!!!
rm1_10 = gsub("(?<=r)[[:digit:]]", "", bar, perl = TRUE); rm1_10 # MUST HAVE perl = TRUE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
no.o.par = gsub("\\(", "", bar); no.o.par # NEEDS THE DOUBLE BACKSLASH
no.pars = gsub(")", "", no.o.par); no.pars # NO $ NEEDED
###############################################################################

length(mt.mamm)
class(mt.mamm)
names(mt.mamm)[1:10]
# THE GENE NAMES (gene.names) ARE GOING TO BE THE MT.MAMM NAMES WITHOUT THE ABBREVIATED SPECIES NAMES COMPONENT
gene.names = gsub("^[[:alnum:]]{1,}\\(","",names(mt.mamm)); gene.names[1:10] 
gene.names = gsub("\\)$","",gene.names); gene.names # IT WORKED
length(names(mt.mamm))

## OKAY, SO, there are 9087 unique names(mt.mamm) because the original names remain unchanged,
# and they still have the abbreviated species names next to the gene names, and there are no
# two species-gene combos that are the same. But each gene has up to 233 homologues. 
unique(gene.names)
length(unique(gene.names)) # only 60 different genes (only 39 of which have 233 homologues)

length(unique((names(mt.mamm)))) # 9087 duh
i = grep("Sequence does not exist", names(mt.mamm)); str(i)
mt.mamm = mt.mamm[-i]; length(mt.mamm) # REALLY IMPORTANT CODE--THIS IS HOW YOU TRIM A DATAFRAME (OR DNABIN)
9087-8568 # 519 rows were removed
## Okay, now there are fewer mt.mamm rows--fewer names. If I do the gene.names code over again,
# I'll get a different (smaller) set of gene names:
gene.names = gsub("^[[:alnum:]]{1,}\\(","",names(mt.mamm)) 
gene.names = gsub("\\)$","",gene.names); length(unique(gene.names)) # NOW THERE ARE ONLY 39 UNIQUE GENE NAMES

table(gene.names) # TABLE IS AN INTERESTING FUNCTION ... 
sum(table(gene.names)) # should be 8568, and it is!

# ANYWAY, WHAT I WOULD LIKE TO DO WITH THIS IS COMPARE 233 HOMOLOGUES OF ONE GENE FROM SPECIES
# IN THE MAMMALIAN CLADE, OR PROBABLY LESS THAN 233.

## We are now ready to do all manner of analyses on this data set. We can analyze base
# frequencies at three levels of variation:
# Between species (all genes pooled)
# Between genes (all species pooled)
# Between sites for a single protein-coding gene (all species pooled)

## To calculate base frequencies for each species, we first create a matrix:
nrow(mamm.mt.taxa) # there are 233 rows for 233 species I presume.

# BF.sp = matrix(NA, nrow = 233, ncol = 4)
# rownames(BF.sp) = mamm.mt.taxa$species
# colnames(BF.sp) = c("A","C","G","T")
# for (i in 1:233){
#   BF.sp[i,] = base.freq(mt.mamm[i])
# }
# matplot(BF.sp, type = "l", col = 1:4, xlab = "Species",
#         ylab = "Base frequency")

# Now do it the right way:
BF.sp.right = matrix(NA, nrow = 233, ncol = 4)
rownames(BF.sp.right) = mamm.mt.taxa$species
colnames(BF.sp.right) = c("A","C","G","T")
for (i in 1:233){
  x = grep(mamm.mt.taxa$code[i], names(mt.mamm))
  BF.sp.right[i,] = base.freq(mt.mamm[x])
}
matplot(BF.sp.right, type = "l", col = 1:4, xlab = "Species",
        ylab = "Base frequency")

# Now do genes with all species pooled:
BF.gene = matrix(NA, nrow = 39, ncol = 4)
rownames(BF.gene) = unique(gene.names)
colnames(BF.gene) = c("A","C","G","T")
for (i in 1:nrow(BF.gene)){
  x = grep(rownames(BF.gene)[i], names(mt.mamm), fixed = TRUE)
  BF.gene[i,] = base.freq(mt.mamm[x])
}
par(mar = c(8,3,3,2))
barplot(t(BF.gene), las = 2, legend = TRUE, 
        args.legend = list(horiz = TRUE, bg = "white"))
dev.off()

################## figuring stuff out ################## 
length(mt.mamm)
mt.mamm[1:50]
names(mt.mamm)[1:10]
length(unique(names(mt.mamm)))
mt.ma.sp = strtrim(names(mt.mamm), 6); mt.ma.sp[1:30]
length(unique(mt.ma.sp))
length(mt.ma.sp)
class(mt.ma.sp)
table(mt.ma.sp)
length(mamm.mt.taxa$code)
unique(mt.ma.sp)
mt.ma.sp = paste(mt.ma.sp,"MIT",sep = ""); unique(mt.ma.sp)
names(mt.mamm)[234]
mt.mamm[mamm.mt.taxa$code[1]]
doo = grep(mamm.mt.taxa$code[1], names(mt.mamm)); doo # THIS IS WHERE I FIGURED OUT WHY IT NEEDS TO BE DONE THE BOOK'S WAY!!!
base.freq(mt.mamm[doo]) # THIS IS AN AVERAGE OF ALL THOSE 37 BASE FREQUENCIES!!!
#################################################################################

# Now on to the third analysis--between sites for a single gene. We'll choose COX1 as our gene:
cox1 = mt.mamm[grep("COX1", names(mt.mamm))]; cox1 # cox1 is 233 1557-bp sequences
class(cox1)
# IF I ONLY DID A TENTH OF THESE, IT WOULDN'T TAKE ANY LONGER THAN THE SYLVIA WARBLERS!
table(sapply(cox1, length)) # tells you the frequency of each length!
str(cox1)
# cox1.clustal = clustal(cox1); cox1.clustal # DON'T HIT ENTER ON THIS COMMAND UNTIL YOU'RE READY TO LET IT RUN FOR HOURS

names(cox1)
y = c(6,17,9,10,15,8,27,174,142,1,4,193,192,187,180,117,110,115,120,118,97,119,109,175,140,
      166,128,124,172,28,12,31,16,90,89,29,32,26,11,36,7,13,14,176,72,68,42,121,60,233,232,228,205)
mamm.mt.taxa$code[y]
mt.mamm[grep(mamm.mt.taxa$code[c(233,232,228)], names(cox1))] # GREP DOESN'T DO MULTIPLE PATTERNS. THAT MEANS A FOR LOOP IS NECESSARY.
# cox1.sbst53 = mt.mamm[grep(mamm.mt.taxa$code[y], names(cox1))]
y[3]
length(y)

# getsbst = function(){
#   for (i in 1:length(y)){
#     cox1.53[i] = mt.mamm[grep(mamm.mt.taxa$code[y[i]], names(cox1))]
#   }
#   return(cox1.53)
# }
# cox1.my53 = capture.output(getsbst()); cox1.my53

# cox.ex = rbind(cox1, mt.mamm[grep(mamm.mt.taxa$code[c(233)], names(cox1))]) Error: "R is stupid"
cox1.53 = cox1[y] # FINALLY FIGURED IT OUT--I'M LUCKY CUZ THE COX1 SUBSET HAS THE EXACT SAME ORDER OF SPECIES AS MAMM.MT.TAXA
cox1.53.clus = clustal(cox1.53); cox1.53.clus # IT WORKED!! IN LESS THAN 5 MINUTES!!!
image(cox1.53.clus)


### CHAPTER 4 - Phylogenies
# Random forest model; broken stick model; .5 and on is reported, .7 is strong, .9 is probably real
library(ape)
wm.tre = read.tree("/Users/jamescutler/Desktop/APER2_Online_Material/rodent.tre")
plot(wm.tre)
wm.tre$edge.length
mean(wm.tre$edge.length)
summary(wm.tre$edge.length)
hist(wm.tre$edge.length)
wm.tre$edge # WHAT'S THIS???
plot(wm.tre, type = "c", use.edge.length = FALSE, direction = "l")
plot(wm.tre); nodelabels(cex = .3); tiplabels(cex = .3); edgelabels(cex = .3)
# BUT HOW DO YOU MAKE THE TREE HORIZONTALLY LONG ENOUGH TO FIT ALL THESE LABELS???

tr.ape = read.tree(text = "((Homo,Pan),Gorilla);")
plot(tr.ape, x.lim = c(-.1, 2.2))
nodelabels("6.4 Ma", 4, frame = "c", bg = "white")
nodelabels("5.4 Ma", 5, frame = "c", bg = "white")

bs.pars = scan()
plot(wm.tre, no.margin = TRUE)
nodelabels(bs.pars, adj = c(1.2, -.5), frame = "n",
           cex = .8, font = 2)
# or ...
plot(wm.tre, no.margin = TRUE)
nodelabels(thermo = bs.pars/100, piecol = "grey")
c.code = c("black","grey","white")
p = character(length(bs.pars))
p[bs.pars >= 90] = c.code[1]
p[bs.pars < 90 & bs.pars >= 70] = c.code[2]
p[bs.pars < 70] = c.code[3]
p
# OR ... (!!!):
plot(wm.tre, no.margin = TRUE)
nodelabels(node = 16:27, pch = 21, bg = p[-1], cex = 2)
points(rep(.005,3), 1:3, pch = 21, cex = 2, bg = c.code)
text(rep(.01,3), 1:3, adj = 0, c("BP >= 90","70 <= BP < 90","BP < 70"))

data("bird.orders")
plot(bird.orders, font = 1, x.lim = 40, no.margin = TRUE)
segments(37,1,37,6,lwd = 2)
text(39,3, "Proaves", srt = 270)
segments(38,6,38,18,lwd = 2)
text(39,14.5,"Neoaves", srt = 270)



### Chapter 5 - Phylogeny Estimation
X = matrix(c(0,1,5),3,3); rownames(X) = LETTERS[1:3]; X
dist(X)
dist(X, method = "maximum")
dist(X, method = "manhattan")
dist(X, method = "binary")
dist(X, method = "euclidean") # As you can see, this is the default
d = dist(X); class(d)
as.matrix(d)


data("woodmouse")
dw = dist.dna(woodmouse)
x = replicate(5, rnorm(15)) # cool! replicate is really straightforward
x
class(x)
delta.plot(dw)
dx = dist(x)
dx
delta.plot(dx)

tr.wm = nj(dw)
which(tr.wm$edge.length < 0)
tr.wm$edge.length[5]
mean(tr.wm$edge.length) # essentially zero. Why???? Is that significant???

# library(phangorn) # apparently not needed for modelTest
wm.tr.ML = phymltest(woodmouse) # Error: command not found
modelTest(x, G = FALSE, I = FALSE) # Error

########################################################################################
# 1. Convert fastq to fasta and write them to files in same directory
writeFasta(fq.1,"fq1.fasta")
# 2. perform multiple sequence alignment with default settings #####
align1 = msa(fa.1)
# 3. convert to seqinr format...
seq_align1 = msaConvert(align1, type="phangorn::phyDat")
# 4. explore the fasta alignment objects
summary(seq_align1)
# 5. a) Build a distance-matrix (comparing "differences between each pair of sequences)
# several ways of doing this .. we will use "maximum likelihood"
dm1 = dist.ml(seq_align1) # BUT dist.ml IS FOR ALIGNMENT OF AMINO ACIDS!!!
# 5. b) check it out
head(dm1,20)
# each number is the "distance" between the sequences indicated in "rows" and "columns"
# 6. We can use the values to compute a phylogenetic tree
# first, we will try a "Neighbor-Joining" tree method
NJ.1 = NJ(dm1); NJ.1
# 7. A quick way to plot a tree. YAY!!!!!!!!!
plot(NJ.1)
# 8. compute liklihood of a given tree
fit <- pml(NJ.1, seq_align1)
# 9. find optimum...using Jukes-Cantor nucleotide model (way too many options to cover them all)
fitJC = optim.pml(fit, model = "JC", rearrangement = "stochastic")
# 10. Use that tree as a starting point, and randomly resample 100 times to compute bootstrap values
bs <- bootstrap.pml(fitJC, bs=100, optNni=TRUE, multicore=TRUE, control = pml.control(trace=0))
plotBS(midpoint(fitJC$tree), bs, p = 50, type="p")
# FINAL STEP FOR NOW: Best way to make a tree look pretty is with a specialized tree-editing program like "FigTree"
# Export this tree
write.tree(bs, file="seq_align1.tre")

## So, ...
# 1. fastq to fasta
# 2. MSA
# 3. [convert to seqinr format]
# 4. [explore summary]
# 5. build distance matrix and check it out
# 6. make a tree (e.g. neighbor joining)
# 7. plot the tree
# 8. compute likelihood of the tree
# 9. Jukes-Cantor to find optimum
# 10. Use that tree as a starting point, and randomsly resample 100 times to compute bootstrap values
####################################################################################################################################

# dist.dna()
# head(distobject, 20)
# tr.nj.wm = nj(distobject); nj.wm
# plot(nj.wm)
data("woodmouse")
dw = dist.dna(woodmouse)
tr.nj.wm = nj(dw)
tr.nj.wm
plot(tr.nj.wm) # works!
class(woodmouse)

wm.clus = clustal(woodmouse)
image(wm.clus)


### Mammalian mt genomes and tree drawing/manipulating review:

# Now on to the third analysis--between sites for a single gene. We'll choose COX1 as our gene:
cox1 = mt.mamm[grep("COX1", names(mt.mamm))]; cox1 # cox1 is 233 1557-bp sequences
class(cox1)
# IF I ONLY DID A TENTH OF THESE, IT WOULDN'T TAKE ANY LONGER THAN THE SYLVIA WARBLERS!
table(sapply(cox1, length)) # tells you the frequency of each length!
str(cox1)
# cox1.clustal = clustal(cox1); cox1.clustal # DON'T HIT ENTER ON THIS COMMAND UNTIL YOU'RE READY TO LET IT RUN FOR HOURS

names(cox1)
y = c(6,17,9,10,15,8,27,174,142,1,4,193,192,187,180,117,110,115,120,118,97,119,109,175,140,
      166,128,124,172,28,12,31,16,90,89,29,32,26,11,36,7,13,14,176,72,68,42,121,60,233,232,228,205)
mamm.mt.taxa$code[y]
# mt.mamm[grep(mamm.mt.taxa$code[c(233,232,228)], names(cox1))] # GREP DOESN'T DO MULTIPLE PATTERNS. THAT MEANS A FOR LOOP IS NECESSARY.
# cox1.sbst53 = mt.mamm[grep(mamm.mt.taxa$code[y], names(cox1))]
y[3]
length(y)

# getsbst = function(){
#   for (i in 1:length(y)){
#     cox1.53[i] = mt.mamm[grep(mamm.mt.taxa$code[y[i]], names(cox1))]
#   }
#   return(cox1.53)
# }
# cox1.my53 = capture.output(getsbst()); cox1.my53

# cox.ex = rbind(cox1, mt.mamm[grep(mamm.mt.taxa$code[c(233)], names(cox1))]) Error: "R is stupid"
cox1.53 = cox1[y] # FINALLY FIGURED IT OUT--I'M LUCKY CUZ THE COX1 SUBSET HAS THE EXACT SAME ORDER OF SPECIES AS MAMM.MT.TAXA
cox1.53.clus = clustal(cox1.53); cox1.53.clus # IT WORKED!! IN LESS THAN 5 MINUTES!!!
image(cox1.53.clus)

# d.cox1.53 = dist.dna(cox1.53) # NOT OF THE SAME LENGTH
# d.cox1.53 = dist.dna(cox1.53.clus) # REMEMBER, THIS IS THE ONE WITH UNINTELLIGIBLE NAMES! SEE BELOW FOR COMMON NAMES
tr.nj.cox1.53 = nj(d.cox1.53)
par(mar = c(1,1,1,1))
plot(tr.nj.cox1.53, cex = .5)
names(cox1.53)
dim(y) = c(1,53); y
class(y)
mamm.mt.taxa$species[y]
common.names = data.frame(scientific = mamm.mt.taxa$species[y], 
                          common = c("La Plata dolphin","white-beaked dolphin","Omura's whale","pygmy right whale",
                                     "Bryde's whale","narwhal","pygmy sperm whale","grey seal","spotted seal",
                                     "European rabbit","European hare","guinea pig","house mouse","brown rat",
                                     "lesser Egyptian jerboa","ring-tailed lemur","Horsfield's tarsier","northern plains gray langur",
                                     "rhesus macaque","Bornean orangutan","gorilla","bonobo","us","brown bear","gray wolf","wolverine",
                                     "raccoon","leopard","domestic cat","cow","water buffalo","dromedary","alpaca","donkey","horse",
                                     "tufted deer","Tibetan antelope","Yezo Sika deer","sheep","Barbary sheep","goat","wild boar",
                                     "common warthog","round-eared elephant shrew","African elephant","dugong","hippo","long-tailed pangolin",
                                     "little red flying fox","western long-beaked echidna","platypus","koala","banded-hare wallaby"))
cox1                                     
cox1.53
names(cox1)
names(cox1.53) = common.names$common
cox1.53.clus = clustal(cox1.53); cox1.53.clus

setwd("/Users/jamescutler/Desktop/Data_Course_cutler/")
save(cox1.53.clus, file = "Cox1_53_clus_common_names.RData")

d.cox1.53 = dist.dna(cox1.53.clus)
tr.nj.cox1.53 = nj(d.cox1.53)
plot(tr.nj.cox1.53, cex = .5)
unique(gene.names)
table(gene.names)





