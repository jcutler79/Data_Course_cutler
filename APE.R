######################################## 
               # APE #
########################################


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
dim(x) = c(3,3); x
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


















