### Calc 3

#### CHAPTER 14 - Partial Derivatives

### 14.1 - Functions of severable variables

b = 5
curve(-x/sqrt(4-x^2), from = -b, to = b, ylim = c(-b,b)); abline(h = 0, v = 0, col = "gray", lty = 2)
t.vals = seq(-2,2,length.out = 100); t.vals
y = -t.vals/sqrt(4-t.vals^2)
plot(t.vals,y, cex = .5)
abline(h = 0, v = 0, col = "gray", lty = 2)
plot(y,t.vals, cex = .5)
abline(h = 0, v = 0, col = "gray", lty = 2)

asin(-1)
asin(1)
asin(2)

library(plot3D)
X = seq(0, pi, length.out = 50)
Y = seq(0, 2*pi, length.out = 50)
M = mesh(X, Y)
phi = M$x
theta = M$y

r = sin(4*phi)^3 + cos(2*phi)^3 + sin(6*theta)^2 + cos(6*theta)^4
x = r*sin(phi)*cos(theta)
y = r*cos(phi)
z = r*sin(phi)*sin(theta)
surf3D(x,y,z, colvar = y, colkey = FALSE, shade = .5, box = FALSE, theta = 60, axes = TRUE) # why no axes???
surf3D(x,y,z, colvar = y, colkey = FALSE, box = FALSE, theta = 60, facets = FALSE)

par(mar = c(1,1,1,1))
example(persp3D)
dev.off()
#######################
# 14.1 #32.
b = 5
l = 100 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = log(X[i]^2 + Y[j]^2)
  }
}
persp3D(X,Y,Z, theta = 40, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5,
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "ln(x^2 + y^2)")
# 32. cont.
b = 5
l = 100 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = 1/(1 + X[i]^2 + Y[j]^2)
  }
}
persp3D(X,Y,Z, theta = 40, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5,
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "1/(1 + x^2 + y^2)")
# 32. cont.
b = 5
l = 100 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = abs(X[i]*Y[j])
  }
}
persp3D(X,Y,Z, theta = 40, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5,
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "abs(xy)")
# 32. cont.
b = 5
l = 100 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = 1/(1 + (X[i]^2)*(Y[j]^2))
  }
}
persp3D(X,Y,Z, theta = 40, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5,
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "1/(1 + (x^2)(y^2))")

# 14.1 #25.
b = 3
l = 50 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = 10 - 4*X[i] - 5*Y[j]
  }
}
persp3D(X,Y,Z, theta = 90, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5,
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = 10 - 4x - 5y \nor 10 = 4x + 5y + z")

# 14.1 #s 61. thru 66.
# 64.
b = 10
l = 100 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = sin(X[i]) - sin(Y[j])
  }
}
persp3D(X,Y,Z, theta = 30, phi = 50, axes = TRUE, scale = 2, box = TRUE, nticks = 5, zlim = c(-2, 4),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = sin(x) - sin(y)")
# 63.
b = 3
l = 50 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = sin(X[i] - Y[j])
  }
}
persp3D(X,Y,Z, theta = 1, phi = 60, axes = TRUE, scale = 2, box = TRUE, nticks = 5,
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = sin(x-y)")
# 65.
b = 3
l = 50 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = (1 - X[i]^2)*(1 - Y[j]^2)
  }
}
persp3D(X,Y,Z, theta = 20, phi = 40, axes = TRUE, scale = 2, box = TRUE, nticks = 5,
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = (1-x^2)(1-y^2)")
# 62.
b = 3
l = 50 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = exp(X[i])*cos(Y[j])
  }
}
persp3D(X,Y,Z, theta = 40, phi = 40, axes = TRUE, scale = 2, box = TRUE, nticks = 5,
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = (e^x)(cos(y))")
# 61. 
b = 4
l = 100 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = sin(X[i]*Y[j])
  }
}
persp3D(X,Y,Z, theta = 30, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5, zlim = c(-1,4),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = sin(xy)")
# 66.
b = 3
l = 50 # anything over 200 is probably too much
X = seq(-b, b, length.out = l)
Y = seq(-b,b, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = (X[i] - Y[j])/(1 + X[i]^2 + Y[j]^2)
  }
}
persp3D(X,Y,Z, theta = 80, phi = 40, axes = TRUE, scale = 2, box = TRUE, nticks = 5, zlim = c(-1,1),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = (x-y)/(1 + x^2 + y^2)")

# 14.1 #29.
bx = 4
by = 2
l = 50 # anything over 200 is probably too much
X = seq(-bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = X[i]^2 + 4*Y[j]^2 + 1
  }
}
persp3D(X,Y,Z, theta = 30, phi = -15, axes = TRUE, scale = 2, box = TRUE, nticks = 5, zlim = c(0,20),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = x^2 + 4y^2 + 1")

# 14.1 #45. (level curves)
ffx = matrix(NA, nrow = 11, ncol = 3)
ffx[,1] = seq(-5,5,1); ffx[,1]
for (i in 1:nrow(ffx)){
  ffx[i,2] = sqrt(1 + ffx[i,1]^2)
  ffx[i,3] = -sqrt(1 + ffx[i,1]^2)
}
ffx
#####
ffy = matrix(NA, nrow = 11, ncol = 3)
ffy[,1] = seq(-5,5,1)
for (i in 1:nrow(ffy)){
  ffy[i,2] = sqrt(ffy[i,1]^2 - 1)
  ffy[i,3] = -sqrt(ffy[i,1]^2 - 1)
}
ffy
#####
bx = 5
by = 5
l = 50 # anything over 200 is probably too much
X = seq(-bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = X[i]^2 - Y[j]^2
  }
}
persp3D(X,Y,Z, theta = 30, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(0,20),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = x^2 - y^2")

# 14.1 #51. (level curves)
b = 3
curve(sqrt(2 - x^2), from = -b, to = b, ylim = c(-b,b)); abline(h = 0, v = 0, lty = 2, lwd = 2); abline(h = seq(-3,3,1), v = seq(-3,3,1), col = "gray")
curve(-sqrt(2 - x^2), from = -b, to = b, ylim = c(-b,b), add = TRUE)
curve(sqrt(3 - x^2), from = -b, to = b, ylim = c(-b,b), add = TRUE)
curve(sqrt(4 - x^2), from = -b, to = b, ylim = c(-b,b), add = TRUE)
curve(sqrt(5 - x^2), from = -b, to = b, ylim = c(-b,b), add = TRUE)
bx = 15
by = 15
l = 100 # anything over 200 is probably too much
X = seq(-bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = (X[i]^2 + Y[j]^2)^(1/3)
  }
}
persp3D(X,Y,Z, theta = 30, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(0,20),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = cube-root(x^2 + y^2)")


##########
# hyperbolic paraboloid:
bx = 5
by = 5
l = 50 # anything over 200 is probably too much
X = seq(-bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = 4 - X[i]^2 - 2*Y[j]^2
  }
}
persp3D(X,Y,Z, theta = 30, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(0,20),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = 4 - x^2 - 2y^2 \n(hyperbolic paraboloid)")

###############################

### 14.3 - Partial Derivatives

# wind chill table:
Twc = matrix(NA, nrow = 10, ncol = 11)
temps.C = c(5,0,-5,-10,-15,-20,-25,-30,-35,-40)
windSpeed.kph = c(5,10,15,20,25,30,40,50,60,70,80)
for (i in 1:nrow(Twc)){
  for (j in 1:ncol(Twc)){
    Twc[i,j] = 13.12 + .6215*temps.C[i] - 11.37*windSpeed.kph[j]^.16 + .3965*temps.C[i]*windSpeed.kph[j]^.16
  }
}
Twc = as.data.frame(Twc)
colnames(Twc) = c(5,10,15,20,25,30,40,50,60,70,80)
rownames(Twc) = c(5,0,-5,-10,-15,-20,-25,-30,-35,-40)

library(tidyr)
Twc$temp = rownames(Twc) # BINGO. THIS PRELIMINARY STEP IS KEY. WITHOUT IT, YOU WOULDN'T HAVE THE TEMP COLUMN THAT TELLS YOU WHAT TEMP CORRESPONDS TO WHAT WC.INDEX
Twc.long = gather(Twc, key = "WindSpeed", value = "W.Chill.Temp", c("5","10","15","20","25","30","40","50","60","70","80"))

class(Twc.long$temp)
Twc.long$temp = as.numeric(Twc.long$temp)
class(Twc.long$WindSpeed)
Twc.long$WindSpeed = as.numeric(Twc.long$WindSpeed)
#####################################################################################
# THIS IS THE ONE THAT ACTUALLY WORKS!!!
ggplot(Twc.long, aes(x = temp, y = W.Chill.Temp, col = WindSpeed)) + geom_point() + 
  ggtitle("Wind Chill Index") +
  xlab("actual temperature (Celsius)") + 
  ylab("wind chill index (perceived temperature in Celsius)") + 
  scale_x_continuous(breaks = seq(-40,5,5)) +
  scale_y_continuous(breaks = seq(-65,5,10)) +
  scale_color_gradient(low = "lightblue", high = "darkblue",
                       breaks = c(5,10,15,20,25,30,40,50,60,70,80))
#####################################################################################

# This one has the wrong color for 5 kph wind:
ggplot(Twc.long, aes(x = temp, y = W.Chill.Temp, col = WindSpeed)) + geom_point() +
  ggtitle("Wind Chill Index") +
  xlab("actual temperature (Celsius)") + 
  ylab("wind chill index (perceived temperature in Celsius)") + 
  scale_x_continuous(breaks = seq(-40,5,5)) +
  scale_color_brewer(breaks = as.character(as.factor(windSpeed.kph)), palette = "RdBu")


T.l1 = Twc.long[1:10,]
T.l2 = Twc.long[11:20,]
T.l3 = Twc.long[21:30,]
T.l4 = Twc.long[31:40,]
T.l5 = Twc.long[41:50,]
T.l6 = Twc.long[51:60,]
T.l7 = Twc.long[61:70,]
T.l8 = Twc.long[71:80,]
T.l9 = Twc.long[81:90,]
T.l10 = Twc.long[91:100,]
T.l11 = Twc.long[101:110,]
# I can't get this one to show a legend:
ggplot() +
  geom_point(data = T.l1, mapping = aes(x = temp, y = W.Chill.Temp), col = "#99FFCC") +
  geom_point(data = T.l2, mapping = aes(x = temp, y = W.Chill.Temp), col = "#99FFFF") +
  geom_point(data = T.l3, mapping = aes(x = temp, y = W.Chill.Temp), col = "#66FFFF") +
  geom_point(data = T.l4, mapping = aes(x = temp, y = W.Chill.Temp), col = "#00FFFF") +
  geom_point(data = T.l5, mapping = aes(x = temp, y = W.Chill.Temp), col = "#00CCFF") +
  geom_point(data = T.l6, mapping = aes(x = temp, y = W.Chill.Temp), col = "#0099FF") +
  geom_point(data = T.l7, mapping = aes(x = temp, y = W.Chill.Temp), col = "#0066FF") +
  geom_point(data = T.l8, mapping = aes(x = temp, y = W.Chill.Temp), col = "#0033FF") +
  geom_point(data = T.l9, mapping = aes(x = temp, y = W.Chill.Temp), col = "#0000CC") +
  geom_point(data = T.l10, mapping = aes(x = temp, y = W.Chill.Temp), col = "#000099") +
  geom_point(data = T.l11, mapping = aes(x = temp, y = W.Chill.Temp), col = "#000066") +
  ggtitle("Wind Chill Index") +
  xlab("actual temperature (Celsius)") + 
  ylab("wind chill index (perceived temperature in Celsius)") + 
  scale_x_continuous(breaks = seq(-40,5,5))









