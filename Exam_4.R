### Exam 4 Biol 490

# Redo of Exam 3

library(dada2)
library(vegan)
library(tidyr)

# Part I

# This is my bash code for converting fastq to fasta and counting lines:
STUC02MMPMLFH00:Exam_3 jamescutler$ ls -ahl
total 21536
drwxr-xr-x  8 jamescutler  staff   256B Apr 26 21:24 .
drwxr-xr-x  5 jamescutler  staff   160B Apr 24 10:15 ..
-rw-r--r--  1 jamescutler  staff   3.4K Apr 24 10:15 Exam_3.Rmd
-rw-r--r--  1 jamescutler  staff   723K Apr 24 10:15 Exam_3.html
-rw-r--r--  1 jamescutler  staff   4.1M Apr 24 10:15 Sample1.fastq
-rw-r--r--  1 jamescutler  staff   4.9M Apr 24 10:15 Sample2.fastq
-rw-r--r--  1 jamescutler  staff   3.0K Apr 24 10:15 exam3_metadata.csv
-rw-r--r--  1 jamescutler  staff   813K Apr 24 10:15 exam3_otu_table.csv
STUC02MMPMLFH00:Exam_3 jamescutler$ fastq_to_fasta -i Sample1.fastq -o Sample1.fasta
STUC02MMPMLFH00:Exam_3 jamescutler$ fastq_to_fasta -i Sample2.fastq -o Sample2.fasta
STUC02MMPMLFH00:Exam_3 jamescutler$ ls -ahl
total 31360
drwxr-xr-x  10 jamescutler  staff   320B Apr 26 21:25 .
drwxr-xr-x   5 jamescutler  staff   160B Apr 24 10:15 ..
-rw-r--r--   1 jamescutler  staff   3.4K Apr 24 10:15 Exam_3.Rmd
-rw-r--r--   1 jamescutler  staff   723K Apr 24 10:15 Exam_3.html
-rw-r--r--   1 jamescutler  staff   2.2M Apr 26 21:25 Sample1.fasta
-rw-r--r--   1 jamescutler  staff   4.1M Apr 24 10:15 Sample1.fastq
-rw-r--r--   1 jamescutler  staff   2.6M Apr 26 21:25 Sample2.fasta
-rw-r--r--   1 jamescutler  staff   4.9M Apr 24 10:15 Sample2.fastq
-rw-r--r--   1 jamescutler  staff   3.0K Apr 24 10:15 exam3_metadata.csv
-rw-r--r--   1 jamescutler  staff   813K Apr 24 10:15 exam3_otu_table.csv
STUC02MMPMLFH00:Exam_3 jamescutler$ grep -c "^>" *.fasta
Sample1.fasta:6575
Sample2.fasta:7507
STUC02MMPMLFH00:Exam_3 jamescutler$
  
  
# Now I get the files ready for trimming:
setwd("/Users/jamescutler/Desktop/Course_Materials/Data_Course/Exams/Exam_3/")
filt.files = dir(path = getwd(), full.names = FALSE, pattern = ".fastq")
filt.files

for (i in filt.files){
  fastqFilter(fn = i, fout = paste0("/Users/jamescutler/Desktop/Course_Materials/Data_Course/Exams/Exam_3/", i, ".trim"), truncLen = 100)
}
# Evidence that this for loop worked:
STUC02MMPMLFH00:Exam_3 jamescutler$ ls -ahl
total 32264
drwxr-xr-x  12 jamescutler  staff   384B Apr 26 21:33 .
drwxr-xr-x   7 jamescutler  staff   224B Apr 26 21:30 ..
-rw-r--r--   1 jamescutler  staff   3.4K Apr 24 10:15 Exam_3.Rmd
-rw-r--r--   1 jamescutler  staff   723K Apr 24 10:15 Exam_3.html
-rw-r--r--   1 jamescutler  staff   2.2M Apr 26 21:25 Sample1.fasta
-rw-r--r--   1 jamescutler  staff   4.1M Apr 24 10:15 Sample1.fastq
-rw-r--r--   1 jamescutler  staff   181K Apr 26 21:33 Sample1.fastq.trim
-rw-r--r--   1 jamescutler  staff   2.6M Apr 26 21:25 Sample2.fasta
-rw-r--r--   1 jamescutler  staff   4.9M Apr 24 10:15 Sample2.fastq
-rw-r--r--   1 jamescutler  staff   265K Apr 26 21:33 Sample2.fastq.trim
-rw-r--r--   1 jamescutler  staff   3.0K Apr 24 10:15 exam3_metadata.csv
-rw-r--r--   1 jamescutler  staff   813K Apr 24 10:15 exam3_otu_table.csv
STUC02MMPMLFH00:Exam_3 jamescutler$ 
  
# Part II

otu = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/Exams/Exam_3/exam3_otu_table.csv")
meta = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/Exams/Exam_3/exam3_metadata.csv")

# In order to subset them:
meta[which(meta$SampleType == "Soil"),1, drop = FALSE] # A nice view of just the (8) soil samples
meta[which(meta$SampleType == "rhizosphere"),1, drop = FALSE] # A nice view of just the (8) rhizo samples

# To save them in a vector:
soil.vec = meta[which(meta$SampleType == "Soil"),1]; length(soil.vec)
rhizo.vec = meta[which(meta$SampleType == "rhizosphere"),1]; length(rhizo.vec)

# To convert the sample names into a usable format for the purposes of what's coming up next:
soil.str = as.character(soil.vec); soil.str
rhizo.str = as.character(rhizo.vec); rhizo.str

# Now to manipulate the otu data frame:
# Extracting just the soil and rhizo columns from the otu data frame (".s" suffix is for "subset"):
otu.s = otu[,c(soil.str,rhizo.str)]

# Transposing the otu subset above so as to have the column names become row names (".t" suffix is for "transpose"):
otu.s.t = as.data.frame(t(otu.s))

# Now to subset the meta data drame:
meta.s = meta[c(which(meta$SampleType == "Soil"),which(meta$SampleType == "rhizosphere")),]

# To verify that both subsetted dataframes have the same row names / SampleID names:
identical(rownames(otu.s.t), as.character(meta.s$SampleID))

# Convert the sample IDs in the meta df to row names:
meta.s$SampleID = as.character(meta.s$SampleID)
identical(rownames(otu.s.t), meta.s$SampleID) 
rownames(meta.s) = meta.s$SampleID
identical(rownames(otu.s.t), rownames(meta.s)) # Double check the row names are indeed the same

# PermANOVA:
pnova1 = adonis(otu.s.t ~ meta.s$SampleType)

# I'll throw in a REAL table for ya:
adonis.table = data.frame(matrix(NA, nrow = 3, ncol = 6))
for (i in 1:nrow(adonis.table)){
  for (j in 1:ncol(adonis.table)){
    adonis.table[i,j] = pnova1[[1]][i,j]
  }
}  
colnames(adonis.table) = c("Df","SumsOfSqs","MeanSqs","F.Model","R2","Pr(>F)")
rownames(adonis.table) = c("meta.s$SampleType","Residuals","Total")
View(adonis.table)

# Part III

# Which soil sample is most abundant, and which rhizo sample is most abundant:
# First, soil:
otu.soil = otu[,c(soil.str, "Consensus.lineage")]
soil.long = gather(otu.soil, key = "sampleID", value = "OTU_abundance", soil.str)
soil.sums = tapply(soil.long$OTU_abundance, soil.long$sampleID, sum)
soil.sums # Shows 8 abundances. Or.Ma.D.s has the highest abundance.
soil.sums["Or.Ma.D.s"] # Abundance of 34,839
identical(as.numeric(soil.sums["Or.Ma.D.s"]), as.numeric(max(soil.sums))) # confirmed - Or.Ma.D.s is the highest


# Next, rhizosphere:
otu.rhizo = otu[,c(rhizo.str, "Consensus.lineage")]
rhizo.long = gather(otu.rhizo, key = "sampleID", value = "OTU_abundance", rhizo.str)
rhizo.sums = tapply(rhizo.long$OTU_abundance, rhizo.long$sampleID, sum)
rhizo.sums # Of the 8 abundances shown, the highest is Mg.Sf.D.rz, at 72,337.
rhizo.sums["Mg.Sf.D.rz"]
identical(as.numeric(rhizo.sums["Mg.Sf.D.rz"]), as.numeric(max(rhizo.sums))) # confirmed - Mg.Sf.D.rz is the higest

  