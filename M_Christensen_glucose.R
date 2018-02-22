#### M. Christensen glucose test

sqrt(100/4*.28)

install.packages(pwr)
library(pwr)
v = 2
sig2 = .025
alpha = .05
pwr = .99

pwr.anova.test(k = v, sig.level = alpha, power = pwr, f = 1.06)
?pwr.anova.test
?uniroot
del = .9
pwr.anova.test(k = v, n = NULL, sig.level = alpha, power = pwr, 
               f = sqrt(del^2/(2*v*sig2)))

sqrt(.25^2/(2*4*.007))


##### Meat cooking experiment (Ch. 3) - sample size trial calculation
meat.table = data.frame(Cook.mthd = rep(c("Fried","Grilled"), each = 5), 
                        ten = c(81,88,85,84,84,84,84,82,81,86), 
                        fiftn = c(85,80,82,80,82,83,88,85,86,88),
                        twenty = c(71,77,72,80,80,78,75,78,79,82))
var(meat.table$ten)
var(meat.table$fiftn)
var(meat.table$twenty)
new.vector = c(meat.table$ten,meat.table$fiftn,meat.table$twenty)
new.vector
var(new.vector)


library(magicfor)
magic_for(print,silent = FALSE)
for (i in 1:2){
  result = rep(c(10,15,20), each = 5)
  print(result)
} 
magic_result_as_dataframe() # IT WORKED!!!!!! I CAPTURED THE OUTPUT OF A FOR LOOP!


meat2 = data.frame(Cook = rep(c("Fried","Grilled"), each = 15), 
                   fat.cntnt = magic_result_as_dataframe(result),
                   weight.post = c(81,88,85,84,84,85,80,82,80,82,71,77,72,80,80,
                                   84,84,82,81,86,83,88,85,86,88,78,75,78,79,82))
# IT WORKED!!!! I USED THE CAPTURED OUTPUT OF A FOR LOOP IN THE CREATION OF A DATA FRAME!
# BUT NOW I NEED TO GET RID OF THE i COLUMN THAT COMES ALONG WITH IT:
meat2$fat.cntnt.i = NULL

library(ggplot2)

class(meat2$weight.post)


ggplot(meat2, aes(x = factor(fat.cntnt.result), y = weight.post, col = "red")) + 
  geom_point(alpha = .6) + ggtitle("Post-cooking weight by fat content") + 
  facet_grid(facets = ~ Cook) 

col.means.fry = tapply(meat2[which(meat2$Cook == "Fried"),"weight.post"], 
                       meat2[which(meat2$Cook == "Fried"),"fat.cntnt.result"], mean)
col.means.fry
col.means.grill = tapply(meat2[which(meat2$Cook == "Grilled"),"weight.post"], 
                         meat2[which(meat2$Cook == "Grilled"),"fat.cntnt.result"], mean)
col.means.grill
x.fry = meat2[1:15,"fat.cntnt.result"]
y.fry = meat2[1:15,"weight.post"]
plot(factor(x.fry),y.fry,col = "red",
     xlab = "fat content (10, 15, or 20 %)",
     ylab = "post-frying weight (grams)") 
points(col.means.fry, col = "blue", pch = 18)
# How can I add (color-highlighted) points for the means of each fat content weight to the 
# facet ggplot above?

## On to calculating sample size:
var(meat2$weight.post)
## Calculate the sample sizes needed for an anova test with alpha = .05 to have power .9 if:
# delta = 5
# delta = 10
v = 6
delta = 5
sig2 = 18.44
alpha = .05
pwr = .9
pwr.anova.test(k = v, sig.level = alpha, power = pwr, f = sqrt(delta^2/(2*v*sig2)))

v = 3 # Should be two, but that would make it not an anova
delta = 10 # don't know for sure what this is either
sig2 = 25 # don't know for sure what this is
alpha = .05
pwr = .9
pwr.anova.test(k = v, sig.level = alpha, power = pwr, f = sqrt(delta^2/(2*v*sig2))) 
# This power test gives a weird result anyway--only >7 in each group. 

# To estimate population mean with a margin of error E at a (1-alpha)*100% level of confidence:
qnorm(.025, lower.tail = FALSE)
nsize = (1.645*30/4)^2; nsize # basically if the SD and E are similar than it's not going to be
# a very big number for sample size. At least, not at a 95% confidence level.






