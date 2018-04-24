### FINAL PROJECT BIOL 490

pan = "AAAGAAGACCACGGAGGCCCTGCTGGAGCTGAAGGCCATGCTGGAGGCCCACCCCGAGGTGGTGTCCCACTACCTGGTGGGGCTACGCTTCACCTGGAGGATGACATCCTACTGAGCCCCTGCTTCCAGCGGGACAGCCGCTACCTGAACATCAACCTGTACAG"
nchar(pan)
macaca2 = "AAAGAAGACCACAGGGGCCCTGCTGGAGATGAAGGCCATGCTGGAGGCCCACCCTGAGGTGGTGTCCCACTAACCGGTGGGGGTGCGCTTCACCCAAGGGATGACATCATACTGAGCCCCTGCTTCCAGCAGGACAGCTGCTACCTGGACATCAACCTGTACAG"
identical(macaca,macaca2) # FALSE!!!!
nchar(macaca2)
identical(pan, macaca2)
pongo = "AAAGAAGACCACGGAGGCCCTGCTGGAGCTGAAGGCCATGCTGGAGGCCCACCCTGAGGTGGTGTCCCACTACCCGGTGGGGGTGCGCTTCACCCAGAGGATGACGTCCTACTGAGCCCCTGCTTCCAGCAGGACAGCCGCTATCTGAACATCAACCTGTACAG"
nchar(pongo)
identical(pan,pongo)
identical(pongo,macaca)

homo = "aaagaagaccacggaggccctgctggagctgaaggccgtgctggaggcccaccctgaggtggtgtcccactacctggtgggggta cgcttcacct ggaggatgac atcctactga gcccctgctt ccagtgggacagccgctacc tgaacatcaa cctgtacagg"
homo = gsub(" ","",homo)
homo
homo = toupper(homo)
homo
nchar(homo)

spalax = "CAGGTCCACGGCCACAAAGGGGTCCAGTTCCAAAACTGGGCAAAGACCTATGGCTGCTGTCCAGAGATGT
ACTACCAGCCCACATCTGTGGAGGAGGTCAAAGAGGTGCTGACCCTAGCTCGACAGCAGAACAAGCGGGT
GAAGGTGGTGGGTGGTGGCCACTCACCCTCGGACATCGCCTGCACGGATGGCTTCATGATTCACATGGGC
AAGATGAACCGGGTTCTCCAGGTTGACAAGGAGAAGAAGCAGGTGACAGTAGAGGCTGGCATCTTCCTGT
CTGACCTGCACCCACAGTTGGACAAGCATGGCCTGGCCCTGTCCAATCTGGGAGCAGTGTCCGATGTGAC
AGTCGCTGGTGTCATTGGGTCTGGGACACACAACACGGGAATCAAGCATGGCATCCTGGCCACTCAGGTG
GTGGCGCTGACCCTGCTGACAGCTGATGGGATCGTTCTGGAATGCTCAGAGTCCAGCAACGCAGATGTGT
TCCAAGCGGCACGAGTGCACCTAGGCTGCTTGGGCATCATCCTCACCGTCACCCTGCAGTGCGTGCCACA
GTTCCACCTGCAGGAAACATCCTTCCCCTCTACCCTCAAGGAGGTCCTCGACAACCTGGACAGCCACCTG
AAGAAGTCAGAATACTTCCGCTTCCTCTGGTTCCCACACAGCGAGAACGTCAGCGTCATCTACCAAGATC
ACACCAGCAAGGCTCCCTCCTCTTCAACTAGCTGGTTTTGGGACTACGCCATTGGATTCTACTTACTGGA
GCTTCTGCTCTGGACCAGCACCTTCCTGCCCTGCCTCGTGGGCTGGATCAACCGCTTCTTCTTCTGGCTA
CTGTTCAACTGTACGAAGGAGAACAGCAACATCAGCCACAAGATCTTCACTTATGAGTGTCGCTTCAAGC
AGCACGTGCAGGACTGGGCCATCCCTAGGGAGAAGACCAAGGAGGCCCTGCTGGAGCTGAAGGCCATGCT
GGAGGCCCACCCCAAGGTGGTGGCCCACTACCCTGTGGAGGTGCGCTTCACCAGGGGTGATGACATCCTA
CTGAGCCCCTGTTTCCAGAGAGACAGCTGCTACATGAACATCATCATGTACAGGCCCTATGGCAAAGATG
TGCCCCGGCTGGACTACTGGCTAGCCTATGAGACCATCATGAAGAAGTTCGGGGGCAGGCCCCACTGGGC
CAAGGCCCACAACTGTACCAGGAAGGACTTCGAGAAAATGTACCCCTCTTTTTCAAAGTTCTGTGCCATC
CGGGAGAGACTGGACCCCACTGGCATGTTCCTGAATTCTTACCTGGAGAAGGTGTTCTACTGA"
spalax = gsub("\\\n","",spalax); spalax
library(stringr)
str_locate(spalax,"AAAGAAGAC")
foo = "ACCTGGAGAAGGTGTTCTACTGA"
str_locate_all(spalax, "AAAGA")
substr(spalax,1114,1114+164)
pongo
library(e1071)
hamming.distance(pan,macaca2)
pongo2 = gsub("CCC","BBB",pongo);pongo2
str_locate_all(pongo2,"BBB")
hamming.distance(pongo,pongo2)
fake = sample(LETTERS[c(1,3,7,20)],164, replace = TRUE)
fake = paste(fake, collapse = ""); fake
hamming.distance(pongo,fake)
class(pongo)

# GLOfastas = readFasta("/Users/jamescutler/Downloads/seqdump (1).txt") # could not find function readFasta
# clustal(GLOfastas)
?DNAbin

GLOfastas = read.dna("/Users/jamescutler/Downloads/seqdump (1).txt", format = "fasta")
class(GLOfastas)
GLOclus = clustal(GLOfastas)
image(GLOclus) # SUCKS THAT IT INCLUDES SUCH A LONG SEQUENCE

# ROUND 2:
GLOprimates = read.dna("/Users/jamescutler/Downloads/seqdump (2).txt", format = "fasta")
GLOprim.clus = clustal(GLOprimates)
image(GLOprim.clus) # STILL CRAP - THE HUMAN SEQUENCE IS TOO LONG

# ROUND 3:
GLO3 = read.dna("/Users/jamescutler/Downloads/seqdump (3).txt", format = "fasta")
class(GLO3)
GLO3.clus = clustal(GLO3) 
par(mar = c(3,6,3,1))
image(GLO3.clus) # can't change the actual y labels??? 

# library(phangorn)
# source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("msa")
# biocLite("S4Vectors")
# biocLite("SummarizedExperiment")
# biocLite("ShortRead")             # NONE OF THESE PACKAGES CONTAIN THE 'msa' FUNCTION!! WTF???
align.GLO = msa(GLO3)

class(GLO3)
dGLO = dist.dna(GLO3)
tr.nj.GLO = nj(dGLO)
tr.nj.GLO
plot(tr.nj.GLO)

### Now to retry the original seqdump (1):
GLOfastas.trmd = as.list(GLOfastas)
GLO.trmd.clus = clustal(GLOfastas.trmd)
par(mar = c(2,15,4,0))
image(GLO.trmd.clus)
identical(GLOfastas, GLOfastas.trmd)
dGLO9 = dist.dna(GLOfastas) # Error in as.matrix.DNAbin(x) : DNA sequences in list not of the same length.
library(ape)
glo4 = read.dna("/Users/jamescutler/Downloads/seqdump (4).txt", format = "fasta")
class(glo4)
glo4.clus = clustal(glo4)
image(glo4.clus)
dglo4 = dist.dna(glo4)
del.gaps(glo4.clus)
class(glo4.clus)
wtf = del.colgapsonly(as.matrix(glo4.clus), threshold = 1, freq.only = FALSE); wtf
wtf = as.matrix(glo4.clus)
t.wtf = t(wtf)
ncol(t.wtf)
nrow(t.wtf)
t.wtf
df.wtf = as.data.frame(t.wtf)
library(magrittr)
class(glo4)
class(glo4.clus)
library(Biostrings)
glo4.clus







