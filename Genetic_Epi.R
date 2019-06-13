### Genetic Epi

#### Homework 1

# 1. b

# 2. a

# 3. d

# 4. d

# 5. c

# 6. c

# 7. a
## A tribe has sick cell SS with a frequency of 36%. What is the frequency of those
# with normal (N = normal allele) blood cells? 

# Well, it should be 64%, duh. We can calculate it the long way:
SSfreq = .36 # = q^2
Sfreq = sqrt(SSfreq) # = q
Nfreq = 1-Sfreq; Nfreq
NNfreq = Nfreq^2; NNfreq
NSfreq = 2*Sfreq*Nfreq; NSfreq
NSfreq + NNfreq # = 64% - this is what proportion of them have normal RBCs


# 8. c

# 9. c

# 10. a

# 11. d

# 12. c

# 13. a

# 14. 

# 15. (see pictures)


### 16. Estimate allele frquencies and test whether each locus is under HWE
# (state null and statistical rule)

## For K blood group:
AA = 1
AB = 20
BB = 967
n = sum(AA,AB,BB); n

# Null: the genetic locus is in HWE for A and B loci
# Ha: the locus is not in HWE

# allele frequencies:
Ac = AA*2 + AB; Ac 
Afreq = Ac/(2*n); Afreq # p
Bc = BB*2 + AB; Bc
Bfreq = Bc/(2*n); Bfreq # q

# expected counts:
# AAfreq = p^2 and BBfreq = q^2 and ABfreq = 2pq
AAfreq = Afreq^2; AAe = AAfreq*n; AAe
BBfreq = Bfreq^2; BBe = BBfreq*n; BBe
ABfreq = 2*Afreq*Bfreq; ABe = ABfreq*n; ABe

z = c(AA,AB,BB,AAe,ABe,BBe)
mysum = 0
for (i in 1:3){
  mysum = mysum + ( (z[i]-(z[i+3]))^2 )/(z[i+3])
  print(mysum)
}
mysum
pchisq(mysum,df = 1, lower.tail = FALSE)
# p-value is below .05, we reject the null; there is evidence that they are not in HWE


## For adenosine deaminase group:
AA = 926
AB = 64
BB = 1
n = sum(AA,AB,BB); n

# Null: the genetic locus is in HWE for A and B loci
# Ha: the locus is not in HWE

# allele frequencies:
Ac = AA*2 + AB; Ac      # A count
Afreq = Ac/(2*n); Afreq # p
Bc = BB*2 + AB; Bc      # B count
Bfreq = Bc/(2*n); Bfreq # q

# expected counts:
# AAfreq = p^2 and BBfreq = q^2 and ABfreq = 2pq
AAfreq = Afreq^2; AAe = AAfreq*n; AAe       # AA expected count
BBfreq = Bfreq^2; BBe = BBfreq*n; BBe       # BB expected count
ABfreq = 2*Afreq*Bfreq; ABe = ABfreq*n; ABe # AB expected count

z = c(AA,AB,BB,AAe,ABe,BBe)
mysum = 0
for (i in 1:3){
  mysum = mysum + ( (z[i]-(z[i+3]))^2 )/(z[i+3])
  print(mysum)
}
mysum
pchisq(mysum,df = 1, lower.tail = FALSE)
## The p value is way high; we fail to reject the null and there is insufficient
# evidence to conclude that they are not in HWE




#############################################################################
#############################################################################

# HW 3

x = c(2.5,5,10)
plot(x)
lines(x)
x = c(10,5,2.5,2.5/2,(2.5/2)/2)
plot(x, type = "l")
x = rexp(10); x
plot(x)

func = function(X) -log(X) + 10
X = seq(0,10,.1)
plot(func(X))
abline(h = 0)
points()



#############################################################################
#############################################################################

# HW 5

# Part I:

N = 489
x11 = 264 + 152; x11 = x11/N
x12 = 13 + 7; x12 = x12/N
x21 = 29 + 15; x21 = x21/N
x22 = 8 + 1; x22 = x22/N


# All B's and b's:
p1 = x11 + x12; p1
p2 = x21 + x22; p2

# All C's and c's:
q1 = x11 + x21; q1
q2 = x12 + x22; q2

p1 + p2; q1 + q2

# Are all the D's equal?
x11 - p1*q1 # .0119772
p1*q2 - x12 # yep
p2*q1 - x21 # yep
x22 - p2*q2 # yep! 
# So, D = .0119772
D = x11 - p1*q1; D
x11*x22 - x12*x21 # THIS ONE IS THE SAME TOO! (.0119772) - THIS IS AB*ab - Ab*aB

# D':
Dmax = min(p1*q2,p2*q1); Dmax
Dprime = D/Dmax; Dprime

# r^2:
r_sq = (D^2)/(p1*p2*q1*q2); r_sq

# Chi-square statistic:
mychi = r_sq*N; mychi # way above the critical value of 3.84:
pchisq(3.84,df = 1, lower.tail = FALSE)
pchisq(mychi,df = 1, lower.tail = FALSE) # p-value = .00031

# The p-value is significant. There is sufficient statistical evidence to reject the null hypothesis of linkage equilibrium.

# Now for the function:
Dprime_rSq_Chisq_from.counts_Enter.sums.for.each.allele.pair = function(ABsum,Absum,aBsum,absum,N){
  x11 = ABsum/N
  x12 = Absum/N
  x21 = aBsum/N
  x22 = absum/N
  p1 = sum(x11,x12)
  p2 = sum(x21,x22)
  q1 = sum(x11,x21)
  q2 = sum(x12,x22)
  D = x11*x22 - x12*x21
  if (D < 0){
    Dmin = max(-p1*q1,p2*q2)
    Dprime = D/Dmin
  } else{Dmax = min(p1*q2,p2*q1); Dprime = D/Dmax}
  r_sq = (D^2)/(p1*p2*q1*q2)
  mychi = r_sq*N
  pvalue = pchisq(mychi,df = 1, lower.tail = FALSE)
  print(sprintf("D' = %s ... and r^2 = %s; ... The Chi-square statistic = %s ... and the p-value is %s",round(Dprime,6),round(r_sq,6),round(mychi,3),round(pvalue,6)))
  return(c(Dprime,r_sq))
}
Dprime_rSq_Chisq_from.counts_Enter.sums.for.each.allele.pair(264+152,13+7,29+15,8+1,N=489)


# Part II:

Dprime_rSq_from.gametic.frequencies.for.alleles.of.two.genes = function(AB,Ab,aB,ab){
  D = AB*ab - Ab*aB
  PA = sum(AB,Ab)
  Pa = sum(aB,ab)
  PB = sum(AB,aB)
  Pb = sum(Ab,ab)
  if (D < 0){
    Dmin = max(-PA*PB,-Pa*Pb)
    Dprime = D/Dmin
  } else{Dmax = min(PA*Pb,Pa*PB); Dprime = D/Dmax}
  r_sq = (D^2)/(PA*Pa*PB*Pb)
  print(sprintf("D' = %s ... and r^2 = %s",Dprime,r_sq))
  return(c(Dprime,r_sq))
}

# Population 1:
pop1 = Dprime_rSq(.2598,.5362,.0792,.1248)
round(pop1,7)

# Population 2:
pop2 = Dprime_rSq(.0008,.0196,.0694,.9102)
round(pop2,7)

# Population 3:
pop3 = Dprime_rSq(.7332,.0082,.0230,.2356)
round(pop3,7)

# Population 3 clearly shows the greatest amounts of LD.



#############################################################################
#############################################################################

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



#############################################################################
#############################################################################

# HWE test function from genotype counts:
## (e.g. AA = 117, Aa = 187, aa = 81, total = 385)

HWEtest.from.geno.counts = function(AA,Aa,aa){
  n = sum(AA,Aa,aa)
  fAA = AA/n
  fAa = Aa/n
  faa = aa/n
  fA = (2*AA + Aa)/(2*n)
  fa = (2*aa + Aa)/(2*n)
  EAA = fA*fA*n
  EAa = 2*fA*fa*n
  Eaa = fa*fa*n
  ovec = c(AA,Aa,aa)
  evec = c(EAA,EAa,Eaa)
  chis.stat = sum(((ovec-evec)^2)/evec)
  pvalue = pchisq(chis.stat,df=1,lower.tail = FALSE)
  print(sprintf("EAA = %s",round(EAA,4)))
  print(sprintf("EAa = %s",round(EAa,4)))
  print(sprintf("Eaa = %s",round(Eaa,4)))
  print(sprintf("Your chi-squared statistic is: %s",round(chis.stat,4)))
  print(sprintf("Your p-value is: %s",round(pvalue,4)))
}
HWEtest.from.geno.counts(117,187,81)

# Proof that the ovec - evec method works:
AA = 117
Aa = 187
aa = 81

n = sum(AA,Aa,aa)
fAA = AA/n; fAA
fAa = Aa/n; fAa
faa = aa/n; faa
fA = (2*AA + Aa)/(2*n); fA2 = fAA*2 + fAa; fA; fA2 # fA2 is incorrect
fa = (2*aa + Aa)/(2*n); fa2 = faa*2 + fAa; fa; fa2 # fa2 is incorrect

fA3 = fAA + .5*fAa; fA3 # correct!

sum(fAA,fAa,faa)

EAA = fA*fA*n; EAA
EAa = 2*fA*fa*n
Eaa = fa*fa*n

((AA - EAA)^2)/EAA + ((Aa - EAa)^2)/EAa + ((aa - Eaa)^2)/Eaa # same as ...
ovec = c(AA,Aa,aa)
evec = c(EAA,EAa,Eaa)
sum(((ovec-evec)^2)/evec) # ... this.



#############################################################################
#############################################################################

# Question that I missed a part of on Exam 1:

A1 = data.frame(type = rep(c("case","contro"),c(56+24,48+52)),
                allele = c(rep(c("A1","A2"),c(56,24)),
                           rep(c("A1","A2"),c(48,52)) ) )
tA1 = table(A1); tA1
chisq.test(tA1)


# Slow way:

ovec = c(56,48,24,52)
evec = c(52,52,38,38)
sum(((ovec-evec)^2)/evec)




#############################################################################
#############################################################################

# HW 7 - read an article (we picked "Genetic variants in novel pathways influence blood pressure and cardiovascular disease risk")












