### Calc 3

#############################################################

       #### CHAPTER 14 - Partial Derivatives ####

#############################################################

# Libraries:
library(plot3D)

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

# library(plot3D)
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

#####
# 14.2 #37.
bx = 8
by = 8
l = 50 # anything over 200 is probably too much
X = seq(-bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = (X[i]^2)*(Y[j]^3)/(2*X[i]^2 + Y[j]^2)
  }
}
persp3D(X,Y,Z, theta = 15, phi = 50, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(0,20),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "x^2y^3/(2x^2 + y^2)")



############################################################################################
############################################################################################

### 14.3 - Partial Derivatives

############################################################################################
############################################################################################

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

ggplot(Twc.long, aes(x = WindSpeed, y = W.Chill.Temp, col = temp)) + geom_point() +
  ggtitle("Wind Chill Index") + 
  xlab("wind speed (km per hr)") + 
  ylab("perceived temperature (Celsius)") +
  scale_x_continuous(breaks = c(5,10,15,20,25,30,40,50,60,70,80)) +
  scale_y_continuous(breaks = seq(-65,5,10)) +
  scale_color_gradient(breaks = seq(-40,5,5))

plot(Twc.long$WindSpeed, Twc.long$W.Chill.Temp)
neg.40 = which(Twc.long$temp == -40)
T.neg40 = Twc.long[neg.40,]

plot(T.neg40$WindSpeed, T.neg40$W.Chill.Temp, main = "Wind Chill temp as a function of wind speed")
## Now to add tangent lines:
c.line = smooth.spline(T.neg40$W.Chill.Temp ~ T.neg40$WindSpeed)
lines(c.line, col = "red")
nuevo.x = 10
pred.a = predict(c.line, x = nuevo.x, deriv = 0); pred.a # pred.a$y seems to simply be the y component of the point at x = 10
pred.b = predict(c.line, x = nuevo.x, deriv = 1); pred.b # pred.b$y = -.683 ... THAT'S THE SLOPE!
y.int = pred.a$y - (pred.b$y*nuevo.x); y.int # -44.27
x.int = -y.int/pred.b$y; x.int # -64.84
points(pred.a, col = "red", pch = 19)
lines(T.neg40$WindSpeed, y.int + pred.b$y*T.neg40$WindSpeed, col = "green")
t1.slope = -y.int/x.int; t1.slope # -.683 ... SAME AS pred.b$y!!!!
abline(h = -51.09932)
## Second tangent line - among the higher wind speeds:
# DONE: c.line = smooth.spline()
# DONE: lines(c.line, col = 'red')
novo.x = 60
pred.alpha = predict(c.line, x = novo.x, deriv = 0); pred.alpha # pred.alpha$y = -64.16662
pred.beta = predict(c.line, x = novo.x, deriv = 1); pred.beta # pred.beta$y = -.14
y.intcept = pred.alpha$y - (pred.beta$y*novo.x); y.intcept # -55.765
x.intcept = -y.intcept/pred.beta$y; x.intcept # -398.26
points(pred.alpha, col = "purple", pch = 19)
lines(T.neg40$WindSpeed, y.intcept + pred.beta$y*T.neg40$WindSpeed, col = "blue")
t2.slope = -y.intcept/x.intcept; t2.slope # indeed, -.14

# Internet example of using smooth.spline and predict to make tangent lines:
x = seq(0,40)
y = dnorm(seq(0,40), mean = 20, sd = 5) # COOL! ADDED BONUS OF LEARNING ABOUT WHAT dnorm DOES!
plot(x,y)
t.line = smooth.spline(y ~ x)
lines(t.line, col = "red")

new.x = 15
pred0 = predict(t.line, x = new.x, deriv = 0); pred0
pred1 = predict(t.line, x = new.x, deriv = 1); pred1

yint = pred0$y - (pred1$y*new.x); yint
xint = -yint/pred1$y; xint
points(pred0, col = "red", pch = 19)
lines(x, yint + pred1$y*x, col = "green")
points(xint, 0, col = "green", pch = 19)

plot(x, yint + pred1$y*x, ylim = c(0,.08))
################
# modeling the wind chill curve using the nls function:
plot(T.neg40$WindSpeed, T.neg40$W.Chill.Temp, main = "Wind Chill temp as a function of wind speed")
nlmodel = nls(W.Chill.Temp ~ a*WindSpeed^b, data = T.neg40, start = list(a = 1, b = 1)); nlmodel
p = coef(nlmodel)
curve(p["a"]*x^p["b"], lwd = 2, col = "red", add = TRUE) # WOW. BEAUTIFUL.
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

##########################################################################
##########################################################################
# Practice with smooth.spline() for tangent lines on the Wind Chill Index:
##########################################################################
##########################################################################
# First tangent line, at wind speed 10 kph:
plot(T.neg40$WindSpeed, T.neg40$W.Chill.Temp, main = "Wind Chill Index at -40 degrees Celsius")
p = smooth.spline(T.neg40$W.Chill.Temp ~ T.neg40$WindSpeed)
lines(p, col = "blue")
newx = 10
pred0 = predict(p, x = newx, deriv = 0) # our y1
pred1 = predict(p, x = newx, deriv = 1) # our m --> takeaway from this exercise: the temperature changes -.68 degrees with every __kph increase in wind speed
yint = pred0$y - pred1$y*newx; yint 
xint = -yint/pred1$y
points(pred0, col = "red", pch = 19)
lines(T.neg40$WindSpeed, pred1$y*T.neg40$WindSpeed + yint, col = "red")
# Second tangent line, at wind speed 25 kph:
newx25 = 25
pred025 = predict(p, x = newx25, deriv = 0)
pred125 = predict(p, x = newx25, deriv = 1) # temp changes -.29 degrees with every __kph increase in wind speed
yint25 = pred025$y - pred125$y*newx25
xint25 = -yint25/pred125$y
points(pred025, col = "green", pch = 19)
lines(T.neg40$WindSpeed, pred125$y*T.neg40$WindSpeed + yint25, col = "green")
# Third tangent line, at wind speed 70 kph:
newx70 = 70
pred070 = predict(p, x = newx70, deriv = 0)
pred170 = predict(p, x = newx70, deriv = 1); pred170 # temp changes -.12 degres with every __kph increase in wind speed
yint70 = pred070$y - pred170$y*newx70
xint70 = -yint70/pred170$y
points(pred070, col = "purple", pch = 19)
lines(T.neg40$WindSpeed, pred170$y*T.neg40$WindSpeed + yint70, col = "purple")
## Takeaway from all three tangent lines: The temp change is most dramatic (-.68 deg per __kph wind speed increase) at the lower speeds, less dramatic (-.12) at higher speeds, decreasing in steepness more than 5-fold
## Now add the non-linear model:
nlmodel = nls(W.Chill.Temp ~ a*WindSpeed^b, data = T.neg40, start = list(a = 1, b = 1)); nlmodel
p = coef(nlmodel)
curve(p["a"]*x^p["b"], lwd = 2, col = "orange", add = TRUE)

## Now on a higher temperature (0 Celsius):
# First tangent line, at 10 kph:
zero = which(Twc.long$temp == 0); zero
T.zero = Twc.long[zero,]
plot(T.zero$WindSpeed, T.zero$W.Chill.Temp)
pz = smooth.spline(T.zero$W.Chill.Temp ~ T.zero$WindSpeed)
lines(pz, col = "orange")
new.x = 10
pred0z = predict(pz, x = new.x, deriv = 0)
pred1z = predict(pz, x = new.x, deriv = 1); pred1z$y # -.285 ....the same as -40 Celsius at the 25 kph area
yintz = pred0z$y - pred1z$y*new.x
xintz = -yintz/pred1z$y
points(pred0z, col = "red", pch = 19)
lines(T.zero$WindSpeed, pred1z$y*T.zero$WindSpeed + yintz, col = "red")
# Second tangent line, at 25 kph:
new.x25 = 25
pred0z25 = predict(pz, x = new.x25, deriv = 0)
pred1z25 = predict(pz, x = new.x25, deriv = 1); pred1z25 # -.12 ... same as -40 C at the 70 kph area
yintz25 = pred0z25$y - pred1z25$y*new.x25
xintz25 = -yintz25/pred1z25$y
points(pred0z25, col = "green", pch = 19)
lines(T.zero$WindSpeed, pred1z25$y*T.zero$WindSpeed + yintz25, col = "green")
# And now, a custom function for drawing the third tangent line, at 70 kph:



##################### FUNCTION FOR CREATING TANGENT LINES!!! ##################### 
tangent.line = function(xdata,ydata,plottitle,xlabel,ylabel,smoothcolor,anewx,pointlinecolor){
  plot(xdata,ydata, main = plottitle, xlab = xlabel, ylab = ylabel)
  p = smooth.spline(ydata ~ xdata)
  lines(p, col = smoothcolor)
  pred0 = predict(p, x = anewx, deriv = 0)
  pred1 = predict(p, x = anewx, deriv = 1)
  yint = pred0$y - pred1$y*anewx
  xint = -yint/pred1$y
  points(pred0, col = pointlinecolor, pch = 19)
  lines(xdata, pred1$y*xdata + yint, col = pointlinecolor)
  print(sprintf("Slope of %s tangent line is %s",pointlinecolor,pred1$y))
  more.tangents = readline(prompt = "Would you like to add another tangent line? Hit 'y' if yes, anything else if no. ")
  if (more.tangents != 'y'){
    print("Cool.")
  }else{
    while (more.tangents == 'y'){
      anewx = as.numeric(readline(prompt = "Enter new x: "))
      pointlinecolor = readline(prompt = "Enter new point and line color (w/out quotes): ")
      pred0 = predict(p, x = anewx, deriv = 0)
      pred1 = predict(p, x = anewx, deriv = 1)
      yint = pred0$y - pred1$y*anewx
      xint = -yint/pred1$y
      points(pred0, col = pointlinecolor, pch = 19)
      lines(xdata, pred1$y*xdata + yint, col = pointlinecolor)
      print(sprintf("Slope of %s tangent line is %s",pointlinecolor,pred1$y))
      more.tangents = readline(prompt = "Would you like to add yet another tangent line? Hit 'y' if yes, anything else if no. ")
    }
  }
}

tangent.line(T.zero$WindSpeed,T.zero$W.Chill.Temp,"Wind Chill Index at 0 Celsius",
             "wind speed","wind chill temp","orange",10,"red")

# Now for a look at tangent line slopes on the temp by WC temp curves:
t.5kph = which(Twc.long$WindSpeed == 5); t.5kph
t5kph = Twc.long[t.5kph,]
tangent.line(t5kph$temp,t5kph$W.Chill.Temp,"Wind Chill Index at 5 kph winds",
             "temperature","wind chill temp","red",-35,"orange")
t.30k = Twc.long[51:60,]
tangent.line(t.30k$temp,t.30k$W.Chill.Temp,"Wind Chill Index at 30 kph winds",
             "temperature","wind chill temperature","red",-15,"orange")
t.80k = Twc.long[101:110,]
tangent.line(t.80k$temp,t.80k$W.Chill.Temp,"Wind Chill Index at 80 kph winds",
             "temperature","wind chill temperature","red",-15,"orange")

### IT TOOK ME UNTIL THE THIRD DAY AFTER MAKING THESE WIND CHILL PLOTS TO "SEE" THE NON-LINEARNESS OF THE WIND SPEED CURVE IN THE X=TEMPERATURE PLOT


### Now for the heat index:
HI = matrix(NA, nrow = 8, ncol = 9)
temp.F = seq(80,96,2)
rel.hum = seq(40,75,5)
c1 = -42.379
c2 = 2.04901523
c3 = 10.14333127
c4 = -.22475541
c5 = -6.83783e-3
c6 = -5.481717e-2
c7 = 1.22874e-3
c8 = 8.5282e-4
c9 = -1.99e-6
for (i in 1:nrow(HI)){
  for (j in 1:ncol(HI)){
    HI[i,j] = c1 + c2*temp.F[j] + c3*rel.hum[i] + c4*temp.F[j]*rel.hum[i] + c5*temp.F[j]^2 + c6*rel.hum[i]^2 + c7*(temp.F[j]^2)*rel.hum[i] + c8*temp.F[j]*rel.hum[i]^2 + c9*(temp.F[j]^2)*rel.hum[i]^2
  }
}
HI = as.data.frame(HI)
colnames(HI) = temp.F
rownames(HI) = rel.hum

HI$humid = rel.hum
HI.long = gather(HI, key = "RealTemp", value = "HeatIndex", as.character(temp.F))
HI.long$RealTemp = as.numeric(HI.long$RealTemp)

ggplot(HI.long, aes(x = RealTemp, y = HeatIndex, col = humid)) + geom_point() +
  ggtitle("Heat Index") +
  xlab("actual temperature (F)") +
  ylab("heat index (perceived temp, F)") +
  scale_y_continuous(breaks = seq(80,135,5)) +
  scale_color_gradient(low = "yellow", high = "red",
                       breaks = seq(40,75,5))

ggplot(HI.long, aes(x = humid, y = HeatIndex, col = RealTemp)) + geom_point() +
  ggtitle("Heat Index") +
  xlab("percent humidity") +
  ylab("heat index (perceived temp, F)") +
  scale_y_continuous(breaks = seq(80,135,5)) +
  scale_x_continuous(breaks = seq(40,75,5)) +
  scale_color_gradient(low = "yellow", high = "red",
                       breaks = seq(80,96,2))

## Tangent lines I'd like to see:
# temps 80 and 96 - t80 and t96
# humidities 40 and 75 - h40 and h75

t80 = which(HI.long$RealTemp == 80); t80
t80 = HI.long[t80,]
t96 = HI.long[65:72,]
h40 = which(HI.long$humid == 40); h40
h40 = HI.long[h40,]
h75 = which(HI.long$humid == 75); h75
h75 = HI.long[h75,]

# for temp 80 F:
tangent.line(t80$humid,t80$HeatIndex,"Heat Index at 80 F","percent humidity","HI",
             "red",45,"green")
# for temp 96 F:
tangent.line(t96$humid,t96$HeatIndex,"Heat Index at 96 F","percent humidity","HI",
             "red",45,"green")
# for humidity 40%:
tangent.line(h40$RealTemp,h40$HeatIndex,"Heat Index at 40% humidity",
             "real temperature","HI","red",82,"green")
# for humidity 75%:
tangent.line(h75$RealTemp,h75$HeatIndex,"Heat Index at 75% humidity",
             "real temperature","HI","red",82,"green")


############################################################################################
############################################################################################

### 14.6 - Directional Derivatives & Gradient Vectors (and tangent planes and normal lines)

############################################################################################
############################################################################################

#9. I just learned (by reading the harvard.edu link below) that you can't graph a function
# f(x,y,z) of three variables. INSANE!!! "But we can visualize it differently by drawing
# surfaces f(x,y,z) = c, where c is a constant.
# http://www.math.harvard.edu/archive/21a_summer_06/handouts/functions.pdf
# Apparently temperature distribution in space is an example of a 3-variable function,
# sin(xyz). CRAZY!!!!!!

## So, the 3D surface of this 3-variable function, f(x,y,z) = yzx^2 - xyz^3, is VERY BORING:
# (That's because if you set f(x,y,z) equal to zero, then z just equals the square root of something.)
bx = 8
by = 8
l = 50 # anything over 200 is probably too much
X = seq(0, bx, length.out = l)
Y = seq(0,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = sqrt((X[i]^2)*Y[j]/(X[i]*Y[j]))
  }
}
persp3D(X,Y,Z, theta = 10, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(0,20),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", 
        main = "z = sqrt(yx^2/xy) \nA surface of a 3-variable function: \nf(x,y,z) = yzx^2 - xyz^3")



############################################################################################
############################################################################################

### 14.7 - Max and Min

############################################################################################
############################################################################################





#4.
bx = 2.5
by = 2
l = 50 # anything over 200 is probably too much
X = seq(-bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = 3*X[i] - X[i]^3 - 2*Y[j]^2 + Y[j]^4
  }
}
persp3D(X,Y,Z, theta = 20, phi = 45, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(0,20),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "3x - x^3 - 2y^2 + y^4")

#7. 
bx = 3
by = 3
bz = 50
l = 50 # anything over 200 is probably too much
X = seq(-bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = X[i] - Y[j]*X[i]^2 - Y[j] + X[i]*Y[j]^2
  }
}
persp3D(X,Y,Z, theta = -5, phi = 70, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = (x-y)(1-xy)")

#11. 
bx = 3
by = 3
bz = 50
l = 50 # anything over 200 is probably too much
X = seq(-bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = X[i]^3 - 3*X[i] + 3*X[i]*Y[j]^2
  }
}
persp3D(X,Y,Z, theta = 5, phi = 50, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = x^3 - 3x + 3xy^2")

#15.
bx = 6
by = 4
bz = 50
l = 50 # anything over 200 is probably too much
X = seq(-.5*bx, bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = exp(X[i])*cos(Y[j])
  }
}
persp3D(X,Y,Z, theta = 20, phi = 30, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = (e^x)*cos(y)")

#18. YOU HAVE TO BE VERY CAREFUL TO MINIMIZE THE Z DIMENSION OR YOU WILL NEVER ACTUALLY SEE THE MINIMUM RIGHT THERE AT (0,0)
bx = 1.5
by = 1.5
bz = 50
l = 50 # anything over 200 is probably too much
X = seq((-2/3)*bx, 1.5*bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = (X[i]^2 + Y[j]^2)*exp(-X[i])
  }
}
persp3D(X,Y,Z, theta = 40, phi = 20, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = (x^2 + y^2)e^(-x)")

#33.
bx = 2
by = 2
bz = 50
l = 50 # anything over 200 is probably too much
X = seq(-bx,bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = X[i]^2 + Y[j]^2 + Y[j]*X[i]^2 + 4
  }
}
persp3D(X,Y,Z, theta = 20, phi = 20, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = x^2 + y^2 + yx^2 + 4")







###################################################################################################
# Randomly got interested in analyzing two independent proportions (from hypospadias paper):
library(MASS)
data("quine")
table(quine$Eth,quine$Sex)
prop.test(table(quine$Eth,quine$Sex), correct = FALSE)

prop.test(8,41,correct = FALSE)
prop.test(17,30,correct = FALSE)
prop.test(c(8,17),c(41,30),correct = FALSE)
hypospad = data.frame(type = rep(c("D","P"),c(41,30)), ej.problems = c(rep(c("Y","N"),c(8,41-8)),rep(c("Y","N"),c(17,30-17))))
table(hypospad$type,hypospad$ej.problems)
prop.test(table(hypospad$type,hypospad$ej.problems), correct = FALSE)
prop.test(table(hypospad$type,hypospad$ej.problems), conf.level = .3, correct = FALSE)
prop.test(c(800,1700),c(4100,3000),correct = FALSE)
prop.test(c(80,170),c(410,300),correct = FALSE)

# See how the margin of error at a 95% CI shrinks with 1 order of magnitude incremental increases in sample size:
plot(c(4,8,12),c(22,7,2), xlim = c(0,15),ylim = c(0,25), main = "3 margins of error for 3 different\nsample sizes each 1 order of magn. apart") 
marg.err = data.frame(X = c(4,8,12), Y = c(22,7,2))
me.nlmodel = nls(Y ~ a*X^b, data = marg.err, start = list(a = 1, b = 1)); me.nlmodel
p = coef(me.nlmodel)
curve(p["a"]*x^p["b"], lwd = 2, col = "red", add = TRUE) # Not so hot of a fit.
###################################################################################################



############################################################################################
############################################################################################

### 15.2 - Double integrals over general regions

############################################################################################
############################################################################################

#17.
bx = 2
by = 2
bz = 50
l = 50 # anything over 200 is probably too much
X = seq(-bx,bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = X[i]*cos(Y[j])
  }
}
persp3D(X,Y,Z, theta = -40, phi = 20, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = xcos(y)")


###################################################################################################

# QUESTIONS FOR EXAM 2 (CHAPTER 15):

## 1. How do you know what to do with z = two different things like in #24 of 15.3??? z = 1 + 2r^2 and z = 7??? 
# Why combine the two (now equal to zero) instead of just choosing one of them?




############################################################################################
############################################################################################

### 15.5 - SURFACE AREA

############################################################################################
############################################################################################

#9. 
bx = 1
by = 1
bz = 50
l = 50 # anything over 200 is probably too much
X = seq(-bx,bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = X[i]*Y[j]
  }
}
persp3D(X,Y,Z, theta = 10, phi = 20, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = xy")


#14.
bx = 1
by = 1
bz = 50
l = 50 # anything over 200 is probably too much
X = seq(-bx,bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = cos(X[i]^2 + Y[j]^2)
  }
}
persp3D(X,Y,Z, theta = 20, phi = 20, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = cos(x^2 + y^2)")
# We actually get to integrate by computer on this one!!!

library(SphericalCubature) # WARNING: rgl_init failed, running with rgl.useNULL = TRUE
## What I need for this integral calculator:
# 1) the equation, z = _______ (can be in terms of r!!! see below example)
# 2) the dimensions of space (n = 3)
# 3) the radius, R = c(0,1)
# 4) upper and lower limits - the "polar angular coordinates of" the limits:
 # lower: (0,0)
 # upper: (1, 0 to 2*pi???) - there is no polar "coordinate" for the upper limit, just an equation: r = 1
f1 = function(r) {r*sqrt(4*(r^2)*(sin(r^2))^2 + 1)}
n = 3 
int1 = adaptIntegrateBallPolar(f1,n, lowerLimit = c(0,0), upperLimit = c(1,2*pi))
SA = 2*pi*int1$integral # 4.18601 - very close to the answer in Slader! (~4.1073)
SA+int1$error
SA-int1$error # still not really any closer to 4.1073





############################################################################################
############################################################################################

### 15.6 - Triple integrals

############################################################################################
############################################################################################

#23.
library(pracma)
f = function(x,y) sqrt(1 - y^2)
ymin = function(x) x
integral2(f, xmin = 0, xmax = 1, ymin = ymin, ymax = 1)

f = function(x,y,z) {1}
zmax = function(x,y) sqrt(1-y^2)
integral3(fun = f, xmin = 0, xmax = 1, ymin = ymin, ymax = 1, zmin = 0, zmax = zmax)
# Both of these get the same answer. There needs to be an x in the function for the zmax here.


bx = 3
by = 3
bz = 50
l = 50 # anything over 200 is probably too much
X = seq(-bx,bx, length.out = l)
Y = seq(-by,by, length.out = l)
Z = matrix(data = NA, nrow = length(X), ncol = length(Y))
for (i in 1:length(X)){
  for (j in 1:length(Y)){
    Z[i,j] = sqrt(X[i]^2 + Y[j]^2)
  }
}
persp3D(X,Y,Z, theta = 20, phi = 10, axes = TRUE, scale = 2, box = TRUE, nticks = 5, #zlim = c(-bz,bz),
        ticktype = "detailed", xlab = "X", ylab = "Y", zlab = "Z", main = "z = sqrt(x^2 + y^2)")






###########################################################################################################
# t (time) is in units of weeks:
Nnot = 1000
r = .13
for (t in 1:100){
  Nt = Nnot*exp(r*t)
  print(prettyNum(Nt,big.mark = ",", scientific = FALSE))
}
# Nnot2 (at 442,413,392) is the final Nt, on the 100th week, of the previous for loop.
Nnot2 = 442413392
r = .13
for (t in 1:10){
  Nt = Nnot2*exp(r*t)
  print(prettyNum(Nt,big.mark = ",", scientific = FALSE))
}
# After 10 more weeks you go from 440 million to 1.6 billion.
Nnot3 = 1623345985
r = .13
for (t in 1:10){
  Nt = Nnot3*exp(r*t)
  print(prettyNum(Nt,big.mark = ",", scientific = FALSE))
}
# After yet 10 more weeks, you go from 1.6 to 5.9 billion


gcd(78,169)
###########################################################################################################


### 16.6 - PARAMETRIC SURFACES AND THEIR AREAS

#53. find area of surface
fun = function(r) r*sqrt(1 + (4*r^2)/((r^2 + 2)^2))
r.int = integrate(fun, lower = 0, upper = 1)
class(r.int)
2*pi*r.int$value








