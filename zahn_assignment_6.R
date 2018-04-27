### Assignment 6 and 7 Biol 490


### PAY ATTENTION TO THE ORDER!!! IT'S REALLY IMPORTANT TO UNDERSTAND WHY CODE IS USED IN THE ORDER
# IT SHOWS UP BELOW FOR CREATING THE TRIMMED FILES IN THE PLACE I'M CREATING THEM. OTHERWISE, IF I 
# TRIED TO USE THE ORDER IN WHICH THE CODE IS USED ON EXAM 3, THIS WOULDN'T WORK.
getwd()
setwd("/Users/jamescutler/Desktop/Data_Course_cutler/")
dir.create("./filtered_six/")


setwd("/Users/jamescutler/Desktop/Course_Materials/Data_Course/assignments/Assignment_6/")
filt.files = dir(path = getwd(), full.names = FALSE, pattern = ".fastq$")

library(dada2)

for (i in filt.files){
  fastqFilter(fn = i, fout = paste0("/Users/jamescutler/Desktop/Data_Course_cutler/filtered_six/", i, ".trim"), 
              truncLen = 150)
} # fastqFilter automatically removes Ns and it also automatically filters at the default quality score of 2!!!
getwd()
setwd("/Users/jamescutler/Desktop/Data_Course_cutler/filtered_six/")
filtered = dir(path = getwd(), pattern = ".trim")
trim.1 = readFastq(filtered[1])

library(ShortRead)
quality(trim.1)
trim.1

derep = derepFastq(filtered)
errs = learnErrors(derep)
plotErrors(errs)


clean = dada(derep, errs)
seqtable = makeSequenceTable(clean)
seqtable_df=as.data.frame(seqtable)


taxonomy = assignTaxonomy(seqtable, refFasta = "/Users/jamescutler/Desktop/Course_Materials/Data_Course/assignments/Assignment_6/sh_general_release_dynamic_10.10.2017.fasta")
tax_df = as.data.frame(taxonomy)
which(colnames(seqtable_df) %in% rownames(tax_df))
colnames(seqtable_df) = paste0(tax_df$Family,"_",tax_df$Genus)

heatmap(as.matrix(seqtable_df), col = gray.colors(100))

sp.abndnce.tbl = write.csv(seqtable_df, "/Users/jamescutler/Desktop/Data_Course_cutler/seq_table.csv")

