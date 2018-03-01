ds = as.numeric(506)
predictions2 = function(x){
  d = function(u,v) (sqrt((u[1] - v[1])^2 + (u[2] - v[2])^2 + (u[3] - v[3])^2 + (u[4] - v[4])^2 + 
                            (u[5] - v[5])^2 + (u[6] - v[6])^2 + (u[7] - v[7])^2 + (u[8] - v[8])^2 + 
                            (u[9] - v[9])^2 + (u[10] - v[10])^2 + (u[11] - v[11])^2 + (u[12] - v[12])^2 + 
                            (u[13] - v[13])^2))
  for (i in 1:506){
    ds[i] = d(mybos[i,1:13],x)
  }
  ds = (ds - mean(ds))/sd(ds)
  w = exp(-ds^2)
  w = w/sum(w)
  y.pred = t(mybos[,14])%*%w
}


#############

bata = data.frame(first.column = x, second.column = y)
x = c(1,2,3,4,5,6,7,8,9,10,11,12)
y = c(1,4,3,4,6,4,1,2,5,4,5,1)
ggplot(bata, aes(x = first.column,y = second.column)) + geom_point() + geom_line()

curve(x^5, from = 1, to = 20)
curve(exp(x), add = TRUE, col = "red")


#############

my.vector = NULL
beg = 0
for (i in 1:40){
  beg = (beg + 12000)*1.11
  print(beg)
  my.vector = rbind(my.vector, data.frame(beg))
}
my.vector = as.vector(my.vector)
stupid = data.frame(numeros = seq(1,40,1), mystuff = my.vector); stupid
length(stupid$numeros)
length(stupid$beg)
stupid$no.intrst = stupid$numeros*12000

ggplot(stupid, aes(x = numeros)) + 
  geom_line(aes(y = stupid$beg), col = "red") + 
  geom_line(aes(y = stupid$no.intrst), col = "green")

Carnap.vect = NULL
inicio = 0
for (i in 1:240){
  inicio = (inicio + 100)*1.03
  print(inicio)
  Carnap.vect = rbind(Carnap.vect, data.frame(inicio))
}
LPositiv = data.frame(nums = 1:240, stuff = Carnap.vect); LPositiv
LPositiv$no.intrst = LPositiv$nums*100

thr.prcnt = ggplot(LPositiv, aes(x = nums)) + 
  geom_line(aes(y = LPositiv$inicio), col = "red") +
  geom_line(aes(y = LPositiv$no.intrst), col = "green")

thr.prcnt + scale_x_continuous(name = "months", 
                               breaks = c(12,24,36,48,60,72,84,96,108,120,132,144,156,168,180,192,204,216,228,240))


# add a $1000 a month for 20 years, grow at 6% interest annually 
Husserl.vec = NULL
lento = 0
for (i in 1:20){
  lento = (lento+12000)*1.06
  print(lento)
  Husserl.vec = rbind(Husserl.vec, data.frame(lento))
}
Phnmnlgy = data.frame(nums = 1:20, stuff = Husserl.vec); Phnmnlgy
Phnmnlgy$no.intrst = Phnmnlgy$nums*12000

lento.six = ggplot(Phnmnlgy, aes(x = nums)) + 
  geom_line(aes(y = Phnmnlgy$lento), col = "red") + 
  geom_line(aes(y = Phnmnlgy$no.intrst), col = "green")
lento.six + geom_point(aes(x = 20, y = 480000), col = "blue", size = .5)



