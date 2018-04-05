### Radiation from the stars - blackbody radiation (calculus Chapter 11 applied project)

# A blackbody is a system that absorbs all the radiation that falls on it. For instance, a 
# black surface or a large cavity with a small hole (like a blast furnace) is a blackbody
# and emits blackbody radiation. For some reason, the radiation emitted by the sun approximates
# blackbody radiation. 

# The Rayleigh-Jeans Law, proposed in the late 19th century, expresses the energy density of 
# blackbody of wavelength lambda as:

#    f(lambda) = 8*pi*k*T/lambda^4

# lambda is measured in meters, T in kelvins, and k is Boltzmann's constant. R-Jeans agrees
# with empirical data at long wavelengths but disagrees drastically for short wavelengths.
# The Law predicts that energy density, f(lambda), goes to infinity as lambda goes to zero, but
# observation shows that f(lambda) goes to zero. This discrepancy between R-Jeans and empirical
# fact is known as the ultraviolet catastrophe. 

# In 1900 Max Planck found a better model (known as Planck's Law) for blackbody radiation:

#     f(lambda) = (8*pi*h*c*lambda^(-5))/(exp(h*c/lambda*k*T) - 1)
# STEWART DOESN'T EVEN GET THE EQUATION RIGHT. IT'S ((2*h*c^2)/(lambda^5))(1/(exp(hc/kTlambda) - 1))

# Use l'Hospital's Rule to show that as lambda goes to 0, f(lambda) goes to 0 and that as lambda
# goes to infinity f(lambda) also goes to 0.

# Actually, as lambda goes to 0, f(lambda) goes to 1.

# Graph f as given by both laws. Use T = 5800 K (the temperature of the sun). 
h = 6.6262e-34
c = 2.997925e8
k = 1.3807e-23
curve((2*h*c^2*x^-5)/(exp(h*c/(x*k*5800)) - 1), from = 0, to = 1.5e-6, ylim = c(0,3e14), n = 1000)
curve((2*h*c^2*x^-5)/(exp(h*c/(x*k*6400)) - 1), from = 0, to = 1.5e-6, ylim = c(0,3e14), n = 1000, add = TRUE)
curve((2*h*c^2*x^-5)/(exp(h*c/(x*k*8000)) - 1), from = 0, to = 1.5e-6, ylim = c(0,3e14), n = 1000, add = TRUE)
curve((2*h*c^2*x^-5)/(exp(h*c/(x*k*9200)) - 1), from = 0, to = 1.5e-6, ylim = c(0,3e14), n = 1000, add = TRUE)

wavelengths = as.vector(seq(1e-11,1.5e-6, length.out = 100)); wavelengths

full.list = NULL
stuff2 = 0
for (i in 1:100){
  stuff2 = (2*h*(c^2)*wavelengths[i]^-5)/(exp(h*c/(wavelengths[i]*k*5800)) - 1)
  full.list = rbind(full.list, data.frame(stuff2))
}
blackbody = data.frame(wvlength = wavelengths[1:100], full.list)

the.Sun = ggplot(blackbody, aes(wvlength, stuff2)) + geom_point(col = "yellow3", size = .3) # freaking WORKS!!!!!!!!!!
the.Sun

Procyon = NULL
stuff3 = 0
for (i in 1:100){
  stuff3 = (2*h*(c^2)*wavelengths[i]^-5)/(exp(h*c/(wavelengths[i]*k*6400)) - 1)
  Procyon = rbind(Procyon, data.frame(stuff3))
}
blackProcyon = data.frame(wvlength = wavelengths[1:100], Procyon)

Vega = NULL
stuff4 = 0
for (i in 1:100){
  stuff4 = (2*h*(c^2)*wavelengths[i]^-5)/(exp(h*c/(wavelengths[i]*k*8100)) - 1)
  Vega = rbind(Vega, data.frame(stuff4))
}
blackVega = data.frame(wvlength = wavelengths[1:100], Vega)

Sirius = NULL
stuff5 = 0
for (i in 1:100){
  stuff5 = (2*h*(c^2)*wavelengths[i]^-5)/(exp(h*c/(wavelengths[i]*k*9200)) - 1)
  Sirius = rbind(Sirius, data.frame(stuff5))
}
blackSirius = data.frame(wvlength = wavelengths, Sirius)

Betelgeuse = NULL
stuff6 = 0
for (i in 1:100){
  stuff6 = (2*h*(c^2)*wavelengths[i]^-5)/(exp(h*c/(wavelengths[i]*k*3400)) - 1)
  Betelgeuse = rbind(Betelgeuse, data.frame(stuff6))
}
blackBetel = data.frame(wvlength = wavelengths, Betelgeuse)

vl.spectrum = data.frame(x1 = c(4e-7,4.5e-7,4.7e-7,5e-7,5.5e-7,5.8e-7,6.2e-7),
                         x2 = c(4.5e-7,4.7e-7,5e-7,5.5e-7,5.8e-7,6.2e-7,7e-7),
                         y1 = rep(0,7),
                         y2 = rep(2.75e14,7),
                         colors = c("A (purple)","B (blue)","C (aqua)","D (green)","E (yellow)","F (orange)","G (red)"))


########################################################
library(grid)
my_bdy.text = c("The sun","Sirius","Procyon","Vega")
my_bdy.grob = grid.text(my_bdy.text, x=c(.29,.2,.23,.23),y=c(.1,.9,.18,.5), gp=gpar(fontsize=6))

library(ggthemes)
ggplot() + geom_rect(data = vl.spectrum, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = colors)) +
  scale_fill_manual(values = alpha(c("purple","blue","turquoise1","green","yellow","orange","red"), .2)) + 
  geom_point(data = blackbody, mapping = aes(wvlength, stuff2), size = .3) + 
  geom_point(data = blackSirius, mapping = aes(wvlength, stuff5), size = .3) +
  geom_point(data = blackProcyon, mapping = aes(wvlength, stuff3), size = .3) +
  geom_point(data = blackVega, mapping = aes(wvlength, stuff4), size = .3) +
  labs(title = "Blackbody radiation of five stars", x = "wavelength (meters)", y = "power density (watts/m^3)") + 
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_x_continuous(breaks = seq(0,1.5e-6,.25e-6)) + 
  scale_y_continuous(breaks = seq(0,2.75e14,.5e14)) + 
  annotation_custom(my_bdy.grob) + theme_bw()
########################################################
library(ggrepel) # for nudge_y and nudge_x

h*c/(.5e-6*k*7000); h*c/.5e-6*k*7000 # NOT THE SAME!!!!!! PUT PARENTHESES AROUND THE DENOMINATOR!

############### OPTIMIZATION: FINDING THE PEAKS OF THE BLACKBODY CURVES
curve(-(x-5)^2 + 10, from = -10, to = 10, ylim = c(-10,10))
abline(h=0,v=0)
practice = function(x) -(x-5)^2 + 10
optimise(practice, interval = c(-10,10), maximum = TRUE)

curve((100*x^-5)/(exp(x^-1) - 1), from = 0, to = 2, ylim = c(0,4000), n = 1000)
one.curve = function(x) (100*x^-5)/(exp(x^-1) - 1) 
optimize(one.curve, interval = c(0,2), maximum = TRUE)
abline(h=2120.144,v=.2014)
points(.2014,2120.144, pch = 21)
#####
curve((x^-5)/(exp(x^-1) - 1), from = 0, to = 2, ylim = c(0,40), n = 1000)
one.a.curve = function(x) (x^-5)/(exp(x^-1) - 1) 
optimize(one.a.curve, interval = c(0,2), maximum = TRUE)
points(.201395,21.2014, pch = 20) # WORKS

curve((.000001*x^-5)/(exp(.0001*x^-1) - 1), from = 0, to = .0001, ylim = c(0,2.2e15), n = 1000)
one.b.curve = function(x) (.000001*x^-5)/(exp(.0001*x^-1) - 1) 
optimize(one.b.curve, interval = c(0,.0001), maximum = TRUE)
points(3.81966e-05,9.677735e14, pch = 20)
optim(c(0,.0001), one.b.curve) # THIS IS WHERE IT BREAKS DOWN. REMOVE ONE ZERO IN EACH 
# OF THE RELEVANT FIELDS, AND IT SUDDENLY WORKS (ACTUALLY, ONLY SORT OF. IT'S LIKE IT'S
# A GRADUAL FAILURE OR SOMETHING.)

uno = 2*h*c^2
dos = h*c/(k*5800)
curve((uno*x^-5)/(exp(dos*x^-1) - 1), from = 0, to = 4e-6, ylim = c(0,4e13), n=1000)
two.curve = function(x) (uno*x^-5)/(exp(dos*x^-1) - 1)
optimise(two.curve, interval = c(0,.001), maximum = TRUE) # doesn't work

###

187500*60*60*24*365 # 5.9 trillion miles = 1 light year

############################### SEA LEVEL RISE ############################

rise = 0
b = 0
for (i in 1:100){
  rise[i] = b + .084
  b = rise[i]
  print(c(i,b))
} # WORKS

sea.rise = 0
s.r.sum = 0
for (i in 1:100){
  sea.rise[i] = i*.084
  s.r.sum[i] = sum(sea.rise)
}
sea.rise
s.r.sum # 424.2
plot(sea.rise)
sea.func = function(x) x*.084
integrate(sea.func, lower = 0, upper = 100) # 420 - Is this just a coincidence?

curve((.084*x^2)/2, from = 0, to = 800)

sea.r.500 = 0
s.r.500.sum = 0
for (i in 1:500){
  sea.r.500[i] = i*.084
  s.r.500.sum[i] = sum(sea.r.500)
}
sea.r.500
s.r.500.sum
length(sea.r.500)
dfrm = data.frame(rate = sea.r.500, level = s.r.500.sum, no.acclrtn = (1:500)*20, nums = 1:500)

ggplot(dfrm, aes(x = nums)) + 
  geom_line(aes(y = dfrm$level), col = "red") + 
  geom_line(aes(y = dfrm$no.acclrtn), col = "green")

# 305 mm per foot. 1 foot of sea level rise in 100 years means ~3 mm per year:
305/100
curve(3*x + .084*x^2, from = 0, to = 107)
base.rate = 0
for (i in 1:107){
  base.rate[i] = 3*i
}


rise2 = 0
for (i in 1:10){
  rise2 = 3*(i + .084) # WRONG
  print(rise2)
}

rise3 = 0
for (i in 1:10){
  rise3 = (3 + .084)*i # ALSO WRONG
  print(rise3)
}

rise4 = 3
rise4.sum
for (i in 1:10){
  rise4[i] = rise4 + .084
  rise4.sum[i] = sum(rise4) # NOT EVEN THIS FREAKING WORKS!
}
rise4
rise4.sum

# BUT THIS DOES WORK FOR THE FIRST STEP:
rise5 = 3
for (i in 1:10){
  rise5 = rise5 + .084
  print(rise5)
}

library(stringr)
mystring = "3.084
3.168
3.252
3.336
3.42
3.504
3.588
3.672
3.756
3.84"
mystring = unlist(strsplit(mystring, "/n"))
mystring
length(mystring)
mystring = str_replace_all(mystring, "\n",",")
mystring
mystring = as.numeric(unlist(strsplit(mystring, ",")))
mystring
length(mystring)
sum(mystring) # I CAN'T BELIEVE SOMETHING THIS SIMPLE TOOK ME THIS LONG TO FIGURE OUT!!!!!!
# BUT AT LEAST IT FREAKING WORKS NOW!!!!!!!!!!!

# NOW FOR THE FULL 107 YEARS (FROM 1993 TO 2100):
rise6 = 3
for (i in 1:107){
  rise6 = rise6 + .084
  print(rise6)
}

mystring2 = "[1] 3.084
[1] 3.168
[1] 3.252
[1] 3.336
[1] 3.42
[1] 3.504
[1] 3.588
[1] 3.672
[1] 3.756
[1] 3.84
[1] 3.924
[1] 4.008
[1] 4.092
[1] 4.176
[1] 4.26
[1] 4.344
[1] 4.428
[1] 4.512
[1] 4.596
[1] 4.68
[1] 4.764
[1] 4.848
[1] 4.932
[1] 5.016
[1] 5.1
[1] 5.184
[1] 5.268
[1] 5.352
[1] 5.436
[1] 5.52
[1] 5.604
[1] 5.688
[1] 5.772
[1] 5.856
[1] 5.94
[1] 6.024
[1] 6.108
[1] 6.192
[1] 6.276
[1] 6.36
[1] 6.444
[1] 6.528
[1] 6.612
[1] 6.696
[1] 6.78
[1] 6.864
[1] 6.948
[1] 7.032
[1] 7.116
[1] 7.2
[1] 7.284
[1] 7.368
[1] 7.452
[1] 7.536
[1] 7.62
[1] 7.704
[1] 7.788
[1] 7.872
[1] 7.956
[1] 8.04
[1] 8.124
[1] 8.208
[1] 8.292
[1] 8.376
[1] 8.46
[1] 8.544
[1] 8.628
[1] 8.712
[1] 8.796
[1] 8.88
[1] 8.964
[1] 9.048
[1] 9.132
[1] 9.216
[1] 9.3
[1] 9.384
[1] 9.468
[1] 9.552
[1] 9.636
[1] 9.72
[1] 9.804
[1] 9.888
[1] 9.972
[1] 10.056
[1] 10.14
[1] 10.224
[1] 10.308
[1] 10.392
[1] 10.476
[1] 10.56
[1] 10.644
[1] 10.728
[1] 10.812
[1] 10.896
[1] 10.98
[1] 11.064
[1] 11.148
[1] 11.232
[1] 11.316
[1] 11.4
[1] 11.484
[1] 11.568
[1] 11.652
[1] 11.736
[1] 11.82
[1] 11.904
[1] 11.988"
mystring2 = unlist(strsplit(mystring2, "\n"))
mystring2
length(mystring2) # for some reason this actually says it's 107 elements long. Odd. 
my.frame2 = data.frame(col1 = 1:107, col2 = mystring2)
class(my.frame2$col2)
my.frame2$col2 = as.character(my.frame2$col2)
my.frame2$col2 = gsub("\\[","",my.frame2$col2)# HOLY SHIZ I FIGURED THIS OUT! I REMOVED A '[' !!!!!!!!!!
my.frame2$col2 = gsub("]","",my.frame2$col2) 
my.frame2$col2 = gsub("1 ","",my.frame2$col2)
my.frame2$col2 = as.numeric(my.frame2$col2)
sum(my.frame2$col2)
# NOW THIS IS HOW YOU GET THE GROWING SUM, YEAR BY YEAR (SHOWING US THE ACTUAL RISING SEA LEVEL):
for (i in 1:107){
  sum.it = sum(my.frame2$col2[1:i])
  print(sum.it)
}

m.string = "[1] 3.084
[1] 6.252
[1] 9.504
[1] 12.84
[1] 16.26
[1] 19.764
[1] 23.352
[1] 27.024
[1] 30.78
[1] 34.62
[1] 38.544
[1] 42.552
[1] 46.644
[1] 50.82
[1] 55.08
[1] 59.424
[1] 63.852
[1] 68.364
[1] 72.96
[1] 77.64
[1] 82.404
[1] 87.252
[1] 92.184
[1] 97.2
[1] 102.3
[1] 107.484
[1] 112.752
[1] 118.104
[1] 123.54
[1] 129.06
[1] 134.664
[1] 140.352
[1] 146.124
[1] 151.98
[1] 157.92
[1] 163.944
[1] 170.052
[1] 176.244
[1] 182.52
[1] 188.88
[1] 195.324
[1] 201.852
[1] 208.464
[1] 215.16
[1] 221.94
[1] 228.804
[1] 235.752
[1] 242.784
[1] 249.9
[1] 257.1
[1] 264.384
[1] 271.752
[1] 279.204
[1] 286.74
[1] 294.36
[1] 302.064
[1] 309.852
[1] 317.724
[1] 325.68
[1] 333.72
[1] 341.844
[1] 350.052
[1] 358.344
[1] 366.72
[1] 375.18
[1] 383.724
[1] 392.352
[1] 401.064
[1] 409.86
[1] 418.74
[1] 427.704
[1] 436.752
[1] 445.884
[1] 455.1
[1] 464.4
[1] 473.784
[1] 483.252
[1] 492.804
[1] 502.44
[1] 512.16
[1] 521.964
[1] 531.852
[1] 541.824
[1] 551.88
[1] 562.02
[1] 572.244
[1] 582.552
[1] 592.944
[1] 603.42
[1] 613.98
[1] 624.624
[1] 635.352
[1] 646.164
[1] 657.06
[1] 668.04
[1] 679.104
[1] 690.252
[1] 701.484
[1] 712.8
[1] 724.2
[1] 735.684
[1] 747.252
[1] 758.904
[1] 770.64
[1] 782.46
[1] 794.364
[1] 806.352"
m.string = unlist(strsplit(m.string, "\n"))
length(m.string)
my.frame2$col3 = m.string
class(my.frame2$col3)
my.frame2$col3 = gsub("\\[","",my.frame2$col3)
my.frame2$col3 = gsub("1]","",my.frame2$col3) # ALSO WORKS
my.frame2$col3 = as.numeric(my.frame2$col3)
my.frame2$col4 = seq(3,3*107,3)
my.frame2$col5 = seq(1993, 2099, 1)

ggplot(my.frame2, aes(x = col1)) + 
  geom_line(aes(y = my.frame2$col3), col = "red") + 
  geom_line(aes(y = my.frame2$col4), col = "green")

806/305 # sea level will rise 2.64 feet 
321/305 # sea level would rise only 1 foot



