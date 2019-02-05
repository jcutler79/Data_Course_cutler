# James Cutler
# WK 1 homework Zhang 

# ALSM 1.11, 1.12, 1.29, 1.30

### 1.11
## The regression function relating production output by an employee after taking
# a training program (Y) to the production output before the training program (X)
# is E{Y} = 20 + .95X, where X ranges from 40 to 100. An observer concludes that
# the training program does not raise production output on average because beta1
# is not greather than 1.0. Comment.
X = 40:100
Y = 20 + .95*X
plot(X, ylim = c(0,140), main = "post-training in red, pre-training in black",
     ylab = "productivity")
points(Y, col = "red", pch = 18)

## Based on the above plot, I would say that the training did work, because the 
# production is in fact higher. It's not really surprising that the improvement 
# would be increasingly smaller as one's prior production increases. Nonetheless, 
# the training appears to have been a success.



### 1.12
## In a study of the relationship for senior citizens between physical activity 
# and frequency of colds, participants were asked to monitor their weekly time 
# spent in exercise over a 5-year period and the frequency of colds. The study
# demonstrated a negative statistical relation exists between time spent in
# exercise and frequency of colds. The investigator conlcuded that increasing
# time spent in exercise is an effective strategy for reducing the frequency of
# colds for senior citizens. 

## a) This was observational data.

## b) The conclusion inferred a causal relationship between exercise and colds 
# (that exercise prevents colds); since this was an observational study, their
# are high standards to meet before expressing confidence about causality. 
# Experimental, rather than observational, data would better establish causality.

## c) 
# 1) Could it be that a hormone or some other genetically-determined factor
# both predisposes one to be more active and to have a healthier immune system
# (and thus less prone to getting colds).
# 2) Could it be that a larger portion of the participants who both exercized 
# more and got colds less were experiencing those two phenomena because they
# lived in warmer climates? The study design says nothing about where the 
# participants came from. If participants were recruited from many different
# places in the country, then perhaps warmer climate could explain higher
# exercise rates and lower infection rates (people are not cloistered indoors
# as often in warmer climates). 

## d) Randomly assign half of participants to a high-exercise group, and the 
# other half to a low-exercise group, and see if there is any association 
# between exercise and colds. Or create low-, medium-, and high-exercise groups.



### 1.29
## Model (1.1): Yi = beta0 + beta1*Xi + Ei
# This model is simple, linear in the parameters, and linear in the predictor
# variable:
# simple: only on predictor variable.
# linear parameters: no parameter appears as an exponent or is multipled/divided
# by another parameter.
# linear predictor variable: this variable appears only in the first power.

## Refer to regression model (1.1). Assume that X = 0 is within the scope of the
# model. What is the implication for the regression function if beta0 = 0 so
# that the model is Yi = beta1*Xi + Ei? How would the regression function plot
# on a graph? 

## I think the plot would have a straight line originating at 0,0, because that
# is what the Y intercept would be. 



### 1.30
## Refer to regression model (1.1). What is the implication for the regression 
# function if beta1 = 0 so that the model is Yi = beta0 + Ei?

## I think the function would be a horizontal line since the slope is zero, and
# that would mean that the predictor variable doesn't affect the dependent 
# variable in any visible way. 



