### Exam 3


## Part 1

# 1. Converted to fasta
# 2. There are 6575 in Sample1, and 7507 in Sample2
# 3. Trim the fastq files:

filt.files = dir(path = getwd(), full.names = FALSE, pattern = ".fastq")
filt.files

library(dada2)
getwd()
setwd("/Users/jamescutler/Desktop/Course_Materials/Data_Course/Exam3/")
dir.create("./filtered/")
for (i in filt.files){
  fastqFilter(fn = i, fout = paste0(getwd(), "/filtered/", i, ".trim"), truncLen = 100)
}
# THAT WAS FREAKING EASY! THANKS DR. ZAHN! I LOVE FEELING THIS WAY DURING A TEST :)


## Part 2

# 1. Import:

otu = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/Exam3/exam3_otu_table.csv")
meta = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/Exam3/exam3_metadata.csv")

# 2. Subset them:
# Just soil or rhizosphere (16 of them):
meta[which(meta$SampleType == "Soil"),1, drop = FALSE]
meta[which(meta$SampleType == "rhizosphere"),1, drop = FALSE]
soil.vec = meta[which(meta$SampleType == "Soil"),1]; soil.vec
rhizo.vec = meta[which(meta$SampleType == "rhizosphere"),1]; length(rhizo.vec)
soil.str = as.character(soil.vec); soil.str
rhizo.str = as.character(rhizo.vec); rhizo.str
otu.t = as.data.frame(t(otu), row.names = TRUE) # WOW THIS MADE R REALLY SLOW AT SHOWING ME THE TRANSPOSED TABLE
# That didn't work. And row.names = FALSE doesn't work either.
otu.s = otu[,c(soil.str,rhizo.str)]
otu.s.t = as.data.frame(t(otu.s)) # HOPEFULLY THIS IS THE RIGHT WAY! STILL REALLY REALLY SLOW :(

meta.s = meta[c(which(meta$SampleType == "Soil"),which(meta$SampleType == "rhizosphere")),]
rownames(otu.s.t)
meta.s$SampleID
identical(rownames(otu.s.t), as.character(meta.s$SampleID)) # YAAAYYYYYYYY!!!!!!!!
meta.s$SampleID = as.character(meta.s$SampleID)
identical(rownames(otu.s.t), meta.s$SampleID) # YAAAYYYYYYYY!!!!!!!!
rownames(meta.s) = meta.s$SampleID
identical(rownames(otu.s.t), rownames(meta.s)) # YAAAYYYYYYYY!!!!!!!!

# 3. permANOVA:
library(vegan)
adonis(otu.s.t ~ meta.s$SampleType)
# p value is .002 for SampleType making a difference in bacterial community composition.
# There is sufficient evidence, at the alpha = .05 level of significance, to conclude that
# there is a difference in bacterial community composition between soil and rhizosphere
# sample types.

# 4. Which genus is most abundant:
head(otu$Consensus.lineage,200)
length(unique(otu$Consensus.lineage)) 

otu.g = otu[,c(soil.str,rhizo.str,"Consensus.lineage")]
length(unique(otu.g$Consensus.lineage))
g.sums = tapply(otu.g$Mg.Sf.R.rz, otu.g$Consensus.lineage, sum)
max(g.sums)
class(g.sums)
g.sums[which(g.sums == max(g.sums))]

smpl.sums = numeric(16)
for (i in 1:16){
  smpl.sums[i] = tapply(otu.g[,i], otu.g$Consensus.lineage, sum) # Nah, this doesn't work. ...
}
smpl.sums

# ... time for some really redundant code!

OMDs = tapply(otu.g$Or.Ma.D.s, otu.g$Consensus.lineage, sum)
OMDs[which(OMDs == max(OMDs))]

OMRs = tapply(otu.g$Or.Ma.R.s, otu.g$Consensus.lineage, sum)
OMRs[which(OMRs == max(OMRs))]

OSDs = tapply(otu.g$Or.Sf.D.s, otu.g$Consensus.lineage, sum)
OSDs[which(OSDs == max(OSDs))]

OSRs = tapply(otu.g$Or.Sf.R.s, otu.g$Consensus.lineage, sum)
OSRs[which(OSRs == max(OSRs))]

MMDs = tapply(otu.g$Mg.Ma.D.s, otu.g$Consensus.lineage, sum)
MMDs[which(MMDs == max(MMDs))]

MMRs = tapply(otu.g$Mg.Ma.R.s, otu.g$Consensus.lineage, sum)
MMRs[which(MMRs == max(MMRs))]

MSDs = tapply(otu.g$Mg.Sf.D.s, otu.g$Consensus.lineage, sum)
MSDs[which(MSDs == max(OMSDs))]

MSRs = tapply(otu.g$Mg.Sf.R.s, otu.g$Consensus.lineage, sum)
MSRs[which(MSRs == max(MSRs))]

OMDr = tapply(otu.g$Or.Ma.D.rz, otu.g$Consensus.lineage, sum)
OMDr[which(OMDr == max(OMDr))]

OMRr = tapply(otu.g$Or.Ma.R.rz, otu.g$Consensus.lineage, sum)
OMRr[which(OMRr == max(OMRr))]

OSDr = tapply(otu.g$Or.Sf.D.rz, otu.g$Consensus.lineage, sum)
OSDr[which(OSDr == max(OSDr))]

OSRr = tapply(otu.g$Or.Sf.R.rz, otu.g$Consensus.lineage, sum)
OSRr[which(OSRr == max(OSRr))]

MMDr = tapply(otu.g$Mg.Ma.D.rz, otu.g$Consensus.lineage, sum)
MMDr[which(MMDr == max(MMDr))]

MMRr = tapply(otu.g$Mg.Ma.R.rz, otu.g$Consensus.lineage, sum)
MMRr[which(MMRr == max(MMRr))]

MSDr = tapply(otu.g$Mg.Sf.D.rz, otu.g$Consensus.lineage, sum); length(MSDr)
MSDr[which(MSDr == max(MSDr))]

# MSRr - done already




################################################################

# How I would have done the above code in a for loop:

# i.sums = NULL
# genera.maxima = NULL
for (i in 1:16){
  i.sums[i] = tapply(otu.g[,i], otu.g$Consensus.lineage, sum)
  genera.maxima = i.sums[i][which(i.sums[i] == max(i.sums[i]))]
  print(genera.maxima) # DOESN'T WORK! THIS STUFF IS WEIRD
}


for (i in 1:16){ # FINALLY!!!!!! IT WORKS!!!!!!!!!!!!!!!!!
  i.sums = tapply(otu.g[,i], otu.g$Consensus.lineage, sum); length(i.sums)
  genera.max = i.sums[which(i.sums == max(i.sums))]; genera.max
  print(genera.max) # YAAAAAAAAYYYYYYYYYYY!!!!!!!!
}












