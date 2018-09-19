# Probability Theory

numerator = .967*.0038; numerator
denominator = (1-.0038)*.015 + .0038*.967; denominator
numerator/denominator



right = vector()
for (i in 1:100000){
  guess = sample(1:4,1)
  right[i] = ifelse(guess == 1,1,0)
}
sum(right)/length(right)

right = vector()
for (i in 1:100000){
  oneortwo = sample(1:2,1) # Offers two possibilities, which are described in the if else set up below:
  if (oneortwo == 2) { # In this possibility, there are two right answers
    correct = sample(1:4,2,replace = FALSE)
    guess = sample(1:4,1)
    right[i] = ifelse(guess %in% correct,1,0)
  }else{ # In this possibility, there is only one right answer
    correct = sample(1:4,1)
    guess = sample(1:4,1)
    right[i] = ifelse(guess == correct,1,0)
  }
} # In both cases, the question is answered at random (there is no strategy here to always choose one of the "duplicated" answers)
sum(right)/length(right)
# I think this code is sound because in this scenario the right answer could be one of the duplicates, 
# or it could be one of the other two answers. 50% of the time it will be the case that one of the
# duplicates is the correct answer (so there will be two choices that will get you a point). The
# other 50% of the time there will be only one of the four choices that is the correct one. This
# is because A, B, C, and D each have a 25% chance of being right. 
## I'm guessing this shows that if you choose answers randomly, you'll be right ~37% of the time. But
## what if you always guess one of the duplicated answers? Would you get ~63% of the answers right?

win = vector()
for (i in 1:10000){
  roll = sample(1:6,4,replace = TRUE)
  win[i] = ifelse(sum(ifelse(roll == 6,1,0)) > 0,1,0)
}
prob.win = sum(win)/10000



roll = sample(1:6,4,replace = TRUE)

win[i] = ifelse(sum(ifelse(roll == 6,1,0)) > 0,1,0)


########################

outcomes = vector()
X = 12
for (i in 1:X-1){
  outcomes[i] = ncol(combn(X,i))
  print(ncol(combn(X,i)))
}
outcomes[X-(X-3)]
plot(outcomes)



for (i in 10:20){
  print(ncol(combn(i,2))) # add the number to its combination and you get the combination of the next number   
}

for (i in 10:20){
  print(ncol(combn(i,3))) # So you're always adding the previous number to the one you get here, and that tells you what the next higher number's combination will be  
}
165+55
220+66

func = function(x) x^3
integrate(func, lower = 1, upper = 5)
.25*5^4 - .25


library(combinat)
ncol(combn(11,6))
ncol(combn(7,4))
6^6
####################################################################################################
best.jobs = data.frame(jobs = c("PhD statistics",
                                "Masters biostatistics",
                                "PhD computer science",
                                "Masters human computer interaction",
                                "PhD physics",
                                "Juris Doctor",
                                "Masters telecom engineering",
                                "Masters applied math",
                                "Masters statistics",
                                "Masters engineering",
                                "Masters computer science",
                                "Masters software engineering",
                                "PhD economics",
                                "MBA",
                                "Masters information science"),
                       salaries = c(131700,
                                    113400,
                                    144800,
                                    115200,
                                    132400,
                                    138200,
                                    119100,
                                    121900,
                                    109700,
                                    117200,
                                    122100,
                                    121300,
                                    122500,
                                    113000,
                                    101800))
worst.jobs = data.frame(jobs = c("Masters interior design",
                                 "Masters educational administration",
                                 "Masters early childhood education",
                                 "Masters criminal justice",
                                 "Masters reading and literacy",
                                 "PhD educational leadership",
                                 "Masters health administration",
                                 "Masters studio art",
                                 "Masters construction management",
                                 "Masters fine arts",
                                 "Masters divinity",
                                 "Masters educational leadership",
                                 "Masters social work",
                                 "Masters leadership",
                                 "Masters curriculum and instruction"),
                        salaries = c(69400,
                                     77100,
                                     48100,
                                     60500,
                                     52300,
                                     88500,
                                     73300,
                                     51300,
                                     99600,
                                     55900,
                                     52100,
                                     72600,
                                     59400,
                                     81600,
                                     58200))
library(ggplot2)
ggplot(best.jobs, aes(1:length(jobs),salaries)) +
  geom_point(col = "green") +
  geom_point(data = worst.jobs, mapping = aes(1:length(jobs),salaries), col = "red") +
  geom_text(data = best.jobs, mapping = aes(1:length(jobs),salaries, label = jobs), size = 3, angle = 20, vjust = -.3, hjust = -.1) +
  geom_text(data = worst.jobs, mapping = aes(1:length(jobs),salaries, label = jobs), size = 3, angle = 20, vjust = -.3, hjust = -.1) +
  scale_y_continuous(breaks = seq(0,150000,10000)) + 
  coord_cartesian(xlim = c(0,18), ylim = c(40000,160000))
#########################################################################################################

permn(6) # Obviously you can't actually do permutations with this.

# MY PERMUTATION FUNCTION:
mypermutationfunc = function(n,r){
  theanswer = factorial(n)/factorial(n-r)
  print(prettyNum(theanswer, big.mark = ",", scientific = FALSE))
}

# MY FUNCTION FOR GETTING THE PERMUTATION AND THE COMBINATION AT THE SAME TIME:
getpermn.and.combn = function(n,r){
  thepermn = prettyNum(factorial(n)/factorial(n-r), big.mark = ",", scientific = FALSE)
  thecombn = prettyNum(factorial(n)/(factorial(r)*factorial(n-r)), big.mark = ",", scientific = FALSE)
  print(sprintf("The permutation is: %s",thepermn))
  print(sprintf("The combination is: %s",thecombn))
}
getpermn.and.combn(6,2)
getpermn.and.combn(50,2)

ncol(combn(5,3))
factorial(3)
mypermutationfunc(3,3)

smallvector = c(4,3,5)
factorial(smallvector) # IT WORKS!

factorial(10)/36

# MY FUNCTION FOR GETTING THE NUMBER OF POSSIBLE ORDERED SETS OF AN UNORDERED SET:
numofOfU = function(k,vector.of.Ms){
  thenumerator = factorial(k)
  facted.vec = factorial(vector.of.Ms)
  thedenominator = prod(facted.vec)
  theanswer = prettyNum(thenumerator/thedenominator, big.mark = ",", scientific = FALSE)
  print(theanswer)
}
numofOfU(10,c(3,3))

over100 = 500000
decade.starting = 2010
for (i in 1:10){
  print(c(prettyNum(decade.starting, big.mark = "", scientific = FALSE), prettyNum(over100, big.mark = ",", scientific = FALSE)))
  over100 = over100*2
  decade.starting = decade.starting + 10
}

at.least.10 = vector()
for (i in 10:20){
  at.least.10[i] = (factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i)
}
sum(at.least.10)

thesum = 0
for (i in 10:20){
  print((factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i))
  thesum = thesum + (factorial(20)/(factorial(i)*factorial(20-i)))*((1/4)^i)*(3/4)^(20-i)
  print(c(i,thesum))
}


.009922275 + .00300675

ncol(combn(20,10))
ncol(combn(20,11))
for (i in 10:20){
  at.least.10[i] = ncol(combn(20,i))
}
for (i in 10:20){
  print(c(i,ncol(combn(20,i))))
}
ncol(combn(20,20))
factorial(20)/(factorial(20)*factorial(0))

# EXAMPLE OF "NOT IN" CODE:
ac = "A B C D E F G H I J K Q R S T U V"
abc = "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
vac = unlist(strsplit(ac, " "))
vabc = unlist(strsplit(abc, " "))
vabc[! vabc %in% vac] # YOU WANT THE PART OF VABC THAT'S JUST THE VABC THAT'S NOT ALSO IN VAC ==> SO, YOU WANT THE VABC PART THAT'S EXCLUSIVE TO VABC

# Problem 1.30:
ncol(combn(11,6))
?expand.grid
trial1 = expand.grid(first = c(1,2,3), second = c(1,2,3), third = c(1,2,3))
class(trial1)
trial1[1,]
all.samps = expand.grid(first = c(1,2,7,8,14,20),
                        second = c(1,2,7,8,14,20),
                        third = c(1,2,7,8,14,20),
                        fourth = c(1,2,7,8,14,20),
                        fifth = c(1,2,7,8,14,20),
                        sixth = c(1,2,7,8,14,20))
all.samps
themeans = vector()
for (i in 1:nrow(all.samps)){
  themeans[i] = mean(all.samps[i,])
} # DOESN'T WORK
hist(themeans) # (DOESN'T WORK)

all.samps$means = mean(all.samps[,1:6]) # DOESN'T WORK

themeans = apply(all.samps,1,mean)
hist(themeans)
summary(themeans)
mean(all.samps[1,1:6])
mean(all.samps[1,])


# A PDF:
x = seq(-10,10,length.out = 10000); x
pdf.x = exp(-x)/((1 + exp(-x))^2)
plot(x,pdf.x, xlim = c(-20,20), type = "l", col = "blue")

cdf.x = 1/(1 + exp(-x))
plot(x,cdf.x, xlim = c(-20,20), type = "l", col = "red")
points(x,pdf.x, type = "l", col = "blue")



x = seq(0,1,length.out = 1000)
plot(-log(x))
abline(h = 0, v = 0, col = "grey")
y = -log(x)
plot(x,y, type = "l", col = "red")

y = function(x) exp(x)
plot(y, xlim = c(-5,5), ylim = c(-5,50))
abline(h = 0, v = 0, col = "grey")
