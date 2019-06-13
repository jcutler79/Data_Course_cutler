# James Cutler - HW 6 - Genetic Epi - Dr. Xu

# Part I (see scanned images for the rest of Part I)

tdt = ((13-4)^2)/(13+4); tdt
pchisq(4.76,df = 1, lower.tail = FALSE)



# Part II

cc = data.frame(type = rep(c("case","control"),c(62+24,52+56)),
                allele = c(rep(c("A1","A2"),c(62,24)),
                           rep(c("A1","A2"),c(52,56)) ) )
t.cc = table(cc); t.cc
chisq.test(t.cc, correct = FALSE)
chisq.test(t.cc)
# Both with and without the continuity correction we have a very significant p-value. We 
## can reject the null. There is sufficent evidence to conclude that A1 is associated with 
## the disease. 


# Odds Ratio:
OR = (62/24)/(52/56); OR
# The odds of being a case for those with A1 is 2.78 times greater compared to those with A2.


