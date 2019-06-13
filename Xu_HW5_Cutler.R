# James Cutler
# Dr. Xu - Genetic Epi - HW 5


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
pop1 = Dprime_rSq_from.gametic.frequencies.for.alleles.of.two.genes(.2598,.5362,.0792,.1248)
round(pop1,7)

# Population 2:
pop2 = Dprime_rSq_from.gametic.frequencies.for.alleles.of.two.genes(.0008,.0196,.0694,.9102)
round(pop2,7)

# Population 3:
pop3 = Dprime_rSq_from.gametic.frequencies.for.alleles.of.two.genes(.7332,.0082,.0230,.2356)
round(pop3,7)

# Population 3 clearly shows the greatest amounts of LD.



