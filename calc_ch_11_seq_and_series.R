### calculus chapter 11 - sequences and series ###


eleven.two.nine = 0
lvn.t.nn.sum = 0
for (i in 1:10){
  eleven.two.nine[i] = 12/(-5)^i
  lvn.t.nn.sum[i] = sum(eleven.two.nine)
}
eleven.two.nine
lvn.t.nn.sum
nines.nums = as.vector(1:10)
plot(nines.nums, eleven.two.nine)
points(nines.nums, lvn.t.nn.sum)
datfr = data.frame(nums = nines.nums, sqnce = eleven.two.nine, sries = lvn.t.nn.sum)
ggplot(datfr, aes(x = nums, y = sqnce)) + geom_point(col = "red") + 
  geom_point(aes(x = nums, y = sries), col = "green") + geom_point(alpha = .2)

two.lvn = 0
t.lvn.sum = 0
for (i in 1:10){
  two.lvn[i] = i/sqrt(i^2 + 4)
  t.lvn.sum[i] = sum(two.lvn)
}
two.lvn
t.lvn.sum
lvn.nums = as.vector(1:10)
datfr11 = data.frame(nums = lvn.nums, sqnce = two.lvn, sries = t.lvn.sum)
ggplot(datfr11, aes(x = lvn.nums, y = sqnce)) + geom_point(col = "red") + 
  geom_point(aes(x = nums, y = sries), col = "green") + geom_point(alpha = .2)

two.thrtn = 0
thrtn.sum = 0
for (i in 1:10){
  two.thrtn[i] = 1/(i^2 + 1)
  thrtn.sum[i] = sum(two.thrtn)
}
two.thrtn
thrtn.sum
thrtn.nums = as.vector(1:10)
datfr13 = data.frame(nums = thrtn.nums, sqnce = two.thrtn, sries = thrtn.sum)
ggplot(datfr13, aes(x = thrtn.nums, y = sqnce)) + geom_point(col = "red") + 
  geom_point(aes(x = nums, y = sries), col = "green") + geom_point(alpha = .2)

two.fftn = 0
t.ftn.sum = 0
for (i in 1:30){
  two.fftn[i] = 2*i/(3*i +1)
  t.ftn.sum[i] = sum(two.fftn)
}
two.fftn
t.ftn.sum
fftn.nums = as.vector(1:30)
datfr15 = data.frame(nums = fftn.nums, sqnce = two.fftn, sries = t.ftn.sum)
ggplot(datfr15, aes(x = nums, y = sqnce)) + geom_point(col = "red") + 
  geom_point(aes(x = nums, y = sries), col = "green") + geom_point(alpha = .2)

foo = 0
for (i in 1:100){
  foo[i] = ((-1)^(i-1))*2/(5^(i-2))
  bar = sum(foo)
}
bar


poo = 0
par = 0
for (i in 1:200){
  poo[i] = 1/i^2
  par[i] = sum(poo)
}
poo
par
poar.nums = as.vector(1:200)
datpoar = data.frame(nums = poar.nums, sqnce = poo, sries = par)
ggplot(datpoar, aes(x = nums, y = sqnce)) + geom_point(col = "red") + 
  geom_point(aes(x = nums, y = sries), col = "green") + geom_point(alpha = .2)


fucpi = function(x) (1/x^2)
curve(fucpi, from = -4, to = 4, ylim = c(-3,20))
abline(v = 0, h = 0)
length(seq(1,27,2))







