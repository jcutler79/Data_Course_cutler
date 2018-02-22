# Exam 1 Biol-490R


getwd()
setwd("Course_Materials/Data_Course/Exam_1/")
DNA_Conc_blah = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/Exam_1/DNA_Conc_by_Extraction_Date.csv.gz")
DNA_Conc_blah2 = DNA_Conc_blah
colnames(DNA_Conc_blah2) = c("number","year","code","date","Katy","Ben","lab")
?hist
hist(DNA_Conc_blah2$Katy, Main= "Katy's DNA concentrations", xlab = "DNA concentrations", ylab = "frequency")

# Part I
hist(DNA_Conc_blah2$Katy, main = "Katy's DNA concentrations", 
     xlab = "DNA concentrations", ylab = "frequency")
hist(DNA_Conc_blah2$Ben, xlab = "DNA concentrations", ylab = "frequency", main = "Ben's DNA concentrations")

# Part II
DNA_Conc_blah2$year.fac = as.factor(DNA_Conc_blah2$year)
plot(DNA_Conc_blah2$year.fac, DNA_Conc_blah2$Katy, xlab = "YEAR", 
     ylab = "DNA concentration", main = "Katy's Extractions")
plot(DNA_Conc_blah2$year.fac, DNA_Conc_blah2$Ben, xlab = "YEAR",
     ylab = "DNA concentration", main = "Ben's Extractions")

# Part III
jpeg(filename = "/Users/jamescutler/Desktop/Data_Course_cutler/CUTLER_Plot1.jpeg")
plot(DNA_Conc_blah2$year.fac, DNA_Conc_blah2$Katy, xlab = "YEAR", 
     ylab = "DNA concentration", main = "Katy's Extractions")
dev.off()

jpeg(filename = "/Users/jamescutler/Desktop/Data_Course_cutler/CUTLER_Plot2.jpeg")
plot(DNA_Conc_blah2$year.fac, DNA_Conc_blah2$Ben, xlab = "YEAR",
     ylab = "DNA concentration", main = "Ben's Extractions")
dev.off()

# Part IV
sort(DNA_Conc_blah2[,6]-DNA_Conc_blah2[,5]) %>% head(1)

which(DNA_Conc_blah2$Differences == 
        sort(DNA_Conc_blah2[,6]-DNA_Conc_blah2[,5]) %>% head(1))

# This is how to get the year:
DNA_Conc_blah2[which(DNA_Conc_blah2$Differences == 
                       +                          sort(DNA_Conc_blah2[,6]-DNA_Conc_blah2[,5]) %>% head(1)),2]
[1] 2000

# Part V
DNA3 = DNA_Conc_blah2 %>% group_by(year) %>% summarise(Yearly_means = mean(Ben))
DNA3[which.max(DNA3$Yearly_means),1:2]

# Part IV
write.csv(DNA3, file = "/Users/jamescutler/Desktop/Data_Course_cutler/Ben_average_by_year.csv")




