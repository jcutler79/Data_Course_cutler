# Bioinformatics



# Libraries
library(stringdist) # For calculating Hamming distance between two strings (also does other distance methods)
library(mgsub) # For substituting multiple patterns simultaneously (great for changing As to Ts, Ts to As, Gs to Cs, and Cs to Gs all at the same time so you don't undo what you did in previous steps by trying to do it one letter at a time)



# A GREAT WEBSITE FOR HANDS-ON BEGINNING BIOINFORMATICS PRACTICE:
## http://rosalind.info/problems

# THE PRACTICE PROBLEMS I'VE DONE ON ROSALIND SO FAR:

dna = readLines("/Users/jamescutler/Downloads/rosalind_dna.txt")
dna = "TGTGAAGCACAAACGAGAAGCTCGATACGAAAATACTAGACGACCTCCCTACTGAGGATAGCGGAGCCGTGATTATAGTGCGCACGGTAGCCCCTGGTTTATGTCCCGTTAACCCCGACGCATGCCATGGGGTCTGACATTATGTAATGTCAAGTAATTCATGGATCACGTCGCGCCCTGCTAAACTCTATCTAACCGCGAGGTCAGACAACTTAGCGAAAGCACAAGACTAGTTAATACCTCGCCTGAAAAGTGTGAGTTCATATGAGTTGAACAGTTTTCAGGACGATACAAATCGATGACTACAGCCGCGTTCAATATTTTTAACAATCCTAGTAACACTAAACACAAACACTTAACAGGATTATGTCTTAGTGTAACCGTAACATCAGTGGAATGTCTTACTCAGGAGGGACCCCGTAGCGAGTTCCACATAGAGTTAGACAAGGCCCTAAACGTGTGATTCCCAGCGCCTGGTGACGGGTGGTCACTCTTAGGAGTGTGCTAGGCGTCTCTTGGGCTAGGGCGCCGTGATGTACAGAAGGCGCGCTACTTGCAGGGAGAGAGAACGTACTTCTTCAACTCGAGGTCACACGATGTATGGGTTTTTTCGTCGGCGACTTCCATCGTCTCCACCTTGCACCAACGACGTATTAGTTTAATGCGCCCACACGCTGGTTTACCTAACGGTTTGACGCTTACCAGGGTCTAGGTCAAGACGGAGCCTGCTACCACACTTCCGATCCGGCTAGTCGGTCCCAGAAATGCTGTCGAGATTTAAATCCCAACGGATCAATGGTATCAGTGAAATGTCCCTTTGATC"

a = str_count(dna,"A")
c = str_count(dna,"C")
g = str_count(dna,"G")
t = str_count(dna,"T")
cat(a,c,g,t, sep = " ")


# Problem 2

dna = "CTAGATGTGACGCCCCGGATGTGGGTTAGGTGCGCTGCATGGAGTTACTGCTAAAGCATGAGGGAGATCTCCAAATTCTAAACGCACCTGTGTTCTGATTTTAAAGACTTCGTAGATATACTAAATACTACGAATGCGCACCCTACTCTCGAGACTCTAATCCTCCAAATAGTACGAGTGCAGAGCTGTCAGCTATCGCTGATGAACACTCGGTGATTGATAGTAAGCAACTACTGCAAAGTAGACCGGATAAGAGAACGAAGTGAAGTCGTGTTATCCACTCAAAGTGGTTATCATCTCAGATCCCGCGAATTCAGCGCGCTATGTGACAAACGAGACAGCTTGACCACCTAGCGACGAGCCTGCTATAGCTACTGGGGCCCCTCTTCGAATCTTGGTTGATCTTGCTTAGGTCCCCACGCCCAGCTGTCTCTTCGCGATCGGCGCACCGGCCTCTCCCATATCAGAGCAGATGCTCATTATGTTGAGGAATCAACCCCATGTATTTCTAGGATCTCTTGTAAGGCCCCTCGAAAGTTACGTCCCTTTTCCCGGTGCTCCCGGGAGGGTCGGACTGAGGATTAAGGACTCGATACTCTACTTATTTCTATCGACAGTACATCCGAGCTATTGCAGCGACTACAAAGCAGCTGCCGTCTTATGAGCTCCAAGCGCACGCCGGTAGTGGAATTCCCCGAACCATTATTAACTATAAGCAGGCTGCCACGCCCTCACCCTGAGTCTGCCGAAATCCTGAAGTTCGACGCACCCCATCAGTCCTGGAAGTCACAACGGGTTATAGCAGAGCGTACCTCGGGACAGTGAACTGCGGATCCCCTATACCCTAGCGCCGGACATGCGCGAGCAGGGTTCGTCACATCAGAATGGTAAGCATGCGAGTCGGATAACAGCATGGTGGCGGG"
rna = gsub("T","U",dna)
rna


# Problem 3

dna = "AAAACCCGGT"
lapply(strsplit(dna,NULL), rev)

data("iris")

sapply(iris,mean) # wow. Great function.
sapply(iris,plot)
summary(iris)

barplot(iris$Petal.Length, col = iris$Species)

paste(lapply(strsplit(dna,NULL), rev), collapse = "")

# It's strange that this doesn't work:
rev(strsplit(dna,NULL))
# But this does:
lapply(strsplit(dna,NULL), rev)



myrev = function(x){
  sapply(lapply(strsplit(x,NULL), rev), paste, collapse = "")
}
dna2 = myrev(dna)


# mgsub: MONEY FUNCTION (GSUB MULTIPLE CHARACTERS SIMULTANEOUSLY)
library(mgsub)

dnaComp = function(x){
  compliment = mgsub(x, pattern = c("A","T","C","G"), 
                     replacement = c("T","A","G","C"))
  return(compliment)
}
dnaComp(dna2)

# Problem 3 real data

dna = readLines("/Users/jamescutler/Desktop/Data_Course_cutler/rosalind_revc.txt")
dna

dnarev = myrev(dna)
dnarev

dnarevComp = dnaComp(dnarev)
dnarevComp



# Problem 4: GC content
R6404 = "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCCTCCCACTAATAATTCTGAGG"
R5959 = "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCTATATCCATTTGTCAGCAGACACGC"
R0808 = "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGACTGGGAACCTGCGGGCAGTAGGTGGAAT"

( (str_count(R6404,"G") + str_count(R6404,"C")) / nchar(R6404) )*100

myGCcontent = function(x){
  gc.content = ( (str_count(x,"G") + str_count(x,"C")) / nchar(x) )*100
  return(gc.content)
}
myGCcontent(R5959)
myGCcontent(R0808)
myGCcontent(c(R0808,R5959)) # It's amazing that this works for vectors and I have no idea why

myGCcontent(c(R6404,R5959,R0808))




# Problem 4 real data:

## Step 1 - read in data:
dna = readLines("/Users/jamescutler/Desktop/rosalind_gc.txt")
dna

## Step 2 - get rid of Rosalind:
dna2 = gsub(">Rosalind_","",dna)

## Step 3 - collapse it:
dna3 = paste0(dna2, collapse = "")
dna3

## Step 4 - get your index of where each 4-digit ID# is, 
### and use it to store the 4-digit IDs:
myindex = gregexpr("\\d",dna3)[[1]][seq(1,36,4)] # IT'S ONLY 36 IF THAT'S HOW MANY THERE END UP BEING!!!
Rnums = str_sub(dna3,myindex,myindex+3)

## Step 5 - Divide the DNA sequences into separate strings:
dna4 = unlist(strsplit(dna3,"\\d"))

## Step 6 - Find out which members in the string vector have DNA sequence info:
dnaindex = which(nchar(dna4[]) > 0)

## Step 7 - Use the index in step 6 to run the GCcontent function on those members:
gc.contents = vector()
for (i in 1:length(dnaindex)){
  print(i)
  gc.contents[i] = myGCcontent(dna4[dnaindex[i]])
}
gc.contents
howmanyRs = str_count(dna,"Rosalind")
gc.contents = gc.contents[1:howmanyRs] # IT'S ONLY 9 IF THAT'S HOW MANY THERE END UP BEING!!!
gc.contents

## Step 8 - Make a data frame by combining the GC-content values with 
### their corresponding ID#s:
df.gc = data.frame(Ros_nums = Rnums,
                   gc.contents = gc.contents)
df.gc

## Step 9 - Find out which ID has the highest GC content:
df.gc = df.gc[order(df.gc$gc.contents, decreasing = TRUE),]
df.gc[1,]

## Step 10 - Make a custom answer out of it:
cat(paste0("Rosalind_",df.gc[1,1]),"\n",df.gc[1,2])



# Problem 5 - Hamming distance

s = "GAGCCTACTAACGGGAT"
t = "CATCGTAATGACGGCCT"
# library(e1071)
# hamming.distance(s,t)


# library(stringdist)
stringdist(s,t) # wow that was freaking easy

dna = readLines("/Users/jamescutler/Desktop/Data_Course_cutler/rosalind_hamm.txt")
dna[1]
dna[2]
stringdist(dna[1],dna[2], method = "hamming")
dna1 = dna[1]
dna2 = dna[2]
dna1sub = str_sub(dna1,1,20)
dna2sub = str_sub(dna2,1,20)
dna1sub
dna2sub
stringdist(dna1sub,dna2sub)
stringdist(dna1sub,dna2sub, method = "hamming")
stringdist(dna1,dna2, method = "hamming")





























