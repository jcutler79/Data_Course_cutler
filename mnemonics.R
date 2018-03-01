# mnemonics

length(seq(1,35,2) + seq(9,43,2))

length(seq(9,43,2)) + length(seq(1,35,2)) + length(seq(1,27,2)) + length(seq(3,33,2)) +
  length(seq(3,19,2)) + length(seq(3,35,2)) + length(seq(1,35,2)) + length(seq(1,29,2)) +
  length(seq(3,19,2)) + length(seq(3,29,2)) + length(seq(1,7,2))

x = sample(1:100,50, replace = TRUE); x
df = data.frame(one = 1:50, two = x)
length(which(duplicated(df$two) == TRUE))
df[which(duplicated(df$two) == TRUE),2]
length(unique(df$two))
which(df$two == 61)

dudes = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/first_38_mnemonics.csv")
which(duplicated(dudes$number) == TRUE)
dudes[c(14,33),]
which(dudes$number == 646) 
which(dudes$number == 102) # I did it in two lines cuz using c() to put two numbers in one line of code breaks R
dudes
dudes[14,] = NA
dudes[28,] = NA
dudes = na.omit(dudes)
length(dudes)
length(dudes$number)
dudes[32,] = NA
dudes = na.omit(dudes)
length(dudes$number)
which(dudes$name == "Nephi?")
dudes[30,]
dudes[30,2] = "Winfrey"
class(dudes$name)
dudes$name = as.character(dudes$name)
class(dudes$name)
dudes
poop = data.frame(one = 1:5,two = c("one","two","three","four","five"))
class(poop$two)
poop$two = as.character(poop$two)
poop = rbind(poop,c(6,"six"),c(7,"seven"))

