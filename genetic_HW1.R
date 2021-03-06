### Genetic Epi

#### Homework 1

### 1. b

### 2. a

### 3. d

# 4. d

# 5. c

# 6. c

# 7. a

# 8. c

# 9. c

# 10. a

# 11. d

# 12. c

# 13. a

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







