# ctrl alt b gets you your whole environment from the notepad back
getwd()
setwd("/Users/jamescutler/Desktop/Course_Materials/Data_Course/data/")
getwd()
list.files()[1:5]
df = read.csv("Fake_grade_data.csv")
View(df)

names(df)

df$Total_Points = rowSums(df[,3:17])
view(df)
View(df)
rowSums(df[c(1,3)])


row.names(df) = df$Student
row.names(df)
names(df)

dftwo = df[,-c(1,2)]
dftwo


A = df[df$Total_Points >= 700,]

df$Total_Points >= 700

?arrange
?order
?sort
# arrange is the least used/useful because you don't hardly ever need to arrange columns
# order is good cuz it orders your rows. It gives you the inherent order in your data.
# sort is self-explanatory.

order(A$Total_Points)
A[order(A$Total_Points),]
A[order(A$Total_Points, decreasing = TRUE),]

sort(A$Total_Points)

