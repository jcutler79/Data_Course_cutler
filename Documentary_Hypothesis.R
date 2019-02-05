### The Documentary Hypothesis

library(dplyr)


andsign = c(23.76,44.14,28.56,28.67); mean(andsign)
per_15 = c(41.69,16.65,15.25,46.03); mean(per_15)
C = c(27.88,36.9,21.17,31.58); mean(C)
Y = c(38.29,29,21.92,21.86); mean(Y)
X = c(41.59,31.6,29.93,25.68); mean(X)
A = c(37,29.1,33,37); mean(A)
B = c(48,28.72,31.93,37.03); mean(B)
f = c(29.46,30.2,30.63,25.78,21.89,32.66,32.5); mean(f)
H = c(26.29,30.15,33.78,24.12,26.59,36.97,24.15); mean(H)

competitors = list(andsign = andsign, 
                   per_15 = per_15, 
                   C = C, 
                   Y = Y, 
                   X = X,
                   A = A,
                   B = B,
                   f = f,
                   H = H)
inorder = lapply(competitors, mean); inorder
inorder = t(as.data.frame(inorder)); inorder = as.data.frame(inorder); inorder
inorder$nombres = rownames(inorder); inorder
colnames(inorder)[1] = "means"
inorder = inorder %>% arrange(desc(inorder$means))
inorder

barplot(inorder$means, col = inorder$means, names.arg = inorder$nombres,
        las = 2)
inorder

# one = vector()
# two = vector()
# three = vector()
# four = vector()
# for (i in 1:nrow(inorder)){
#   one[i] = eval(parse(text = inorder[i,2]))[1]
#   two[i] = eval(parse(text = inorder[i,2]))[2]
#   three[i] = eval(parse(text = inorder[i,2]))[3]
#   four[i] = eval(parse(text = inorder[i,2]))[4]
# }
# one; two; three; four
# inorder$one = one
# inorder$two = two
# inorder$three = three
# inorder$four = four
# inorder


plot(f, xlim = c(1,14), ylim = c(0,60), pch = 19, col = "red", 
     xlab = "", ylab = "", main = "comparing f and H")
points(8:14,H, pch = 19, col = "blue")
abline(h = 30, lty = "dotted", col = "green")






