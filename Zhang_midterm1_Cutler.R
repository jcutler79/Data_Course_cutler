# Zhang_Midterm1_Cutler

# Cheese data:

# install.packages("segmented")
# install.packages("stargazer") # FOR MARKDOWN
library(segmented)
library(sas7bdat)
library(ggplot2)
library(car)
library(MASS)

ch = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/cheese.sas7bdat",
                   debug = TRUE)

colnames(ch) = c("case","taste","Lacid","Aacid","Hsulf")


Lmod = lm(taste ~ Lacid, data = ch)
plot(ch$Lacid,ch$taste, xlab = "lactic acid", ylab = "taste")
abline(Lmod, col = "red")
Amod = lm(taste ~ Aacid, data = ch)
plot(ch$Aacid,ch$taste, xlab = "acetic acid", ylab = "taste")
abline(Amod, col = "blue")
Hmod = lm(taste ~ Hsulf, data = ch)
plot(ch$Hsulf,ch$taste, xlab = "H sulfide", ylab = "taste")
abline(Hmod, col = "green")

summary(Lmod)
summary(Amod)
summary(Hmod)
confint(Lmod)
confint(Amod)
confint(Hmod)

ggplot(ch, aes(Lacid,taste)) +
  geom_point() +
  geom_smooth()

ggplot(ch, aes(Aacid,taste)) +
  geom_point() +
  geom_smooth()

ggplot(ch, aes(Hsulf,taste)) +
  geom_point() +
  geom_smooth()


# Quadratic models
## Lactic acid:
Lacid2 = (ch$Lacid)^2; Lacid2
Lacid = ch$Lacid
taste = ch$taste

Lquad = lm(taste ~ Lacid + Lacid2)


plot(Lacid,taste)
acidvals = seq(.85,2.05,.01); acidvals
predcnts = predict(Lquad, list(Lacid=acidvals,Lacid2=acidvals^2)); predcnts
lines(acidvals,predcnts, col = "purple", lwd = 3)

# MSE and R^2 of quad and linear lactic:
mse.L = mean(Lmod$residuals^2); mse.L
mse.Lq = mean(Lquad$residuals^2); mse.Lq
summary(Lquad)
summary(Lmod)


## Acetic acid quadratic:
Aacid2 = (ch$Aacid)^2
Aacid = ch$Aacid

Aquad = lm(taste ~ Aacid + Aacid2)

plot(Aacid,taste)
Aacidvals = seq(min(Aacid),max(Aacid),1)
predA = predict(Aquad, list(Aacid=Aacidvals,Aacid2=Aacidvals^2))
lines(Aacidvals,predA, col = "purple", lwd = 3)

mse.A = mean(Amod$residuals^2); mse.A
mse.Aq = mean(Aquad$residuals^2); mse.Aq
summary(Aquad)
summary(Amod)

Anova(Aquad, type = "III")


# 14. segmented regression model:
qplot(Hsulf, taste, group = Hsulf > 2500, geom = c('point','smooth'),
      method = "lm", se = F, data = ch) +
  geom_vline(xintercept = 2500, linetype = 3, col = "gray")

Hsegmod = segmented(Hmod, seg.Z = ~Hsulf, psi = 2500,
                    control = seg.control(display = FALSE))
summary(Hsegmod)
davies.test(Hmod, seg.Z = ~ Hsulf)
mean(Hsegmod$residuals^2)

plot(ch$Hsulf,ch$taste)
plot(Hsegmod, add = TRUE)
mse.Hseg = mean(Hsegmod$residuals^2); mse.Hseg

-.09156 + .09239
slope(Hsegmod)
pt(2.134,df = 26, lower.tail = FALSE)*2
pt(2.0605, df = 26, lower.tail = FALSE)*2

pscore.test(Hmod)


# 15. log transform of Hsulfide


logHS = log(ch$Hsulf)
Hlogmod = lm(taste ~ logHS)
summary(Hlogmod)

plot(log(ch$Hsulf),ch$taste)
abline(Hlogmod, col = "darkgreen")

mse.Hlog = mean(Hlogmod$residuals^2); mse.Hlog
mse.Hseg




# 17. residual plots for segmented and log-transformed H sulfide:
plot(Hsegmod$residuals, main = "Segmented residuals",
     ylab = "residuals"); abline(h = 0, col = "red", lty = 2)
plot(Hlogmod$residuals, main = "Log-transformed residuals",
     ylab = "residuals"); abline(h = 0, col= "red", lty = 2)

# Studentized (jackknifed - studres(lm) (requires MASS) ARE EXACTLY THE SAME:
plot(studres(Hsegmod)); abline(h = 0, col = "red", lty = 2)
plot(Hsegmod$residuals, main = "Segmented residuals",
     ylab = "residuals"); abline(h = 0, col = "red", lty = 2)

plot(studres(Hlogmod)); abline(h = 0, col = "red", lty = 2)
plot(Hlogmod$residuals, main = "Log-transformed residuals",
     ylab = "residuals"); abline(h = 0, col= "red", lty = 2)

# Semi-studentized (standardized - rstandard(lm)):
plot(rstandard(Hsegmod)); abline(h = 0, col= "red", lty = 2)




# 22. MR model
Aacid = ch$Aacid
Lacid; Aacid; logHS
MRfull = lm(taste ~ Lacid*Aacid*logHS); summary(MRfull)
# NO SIGNIFICANT INTERACTIONS!?!?!?

MRred = lm(taste ~ Lacid+Aacid+logHS); summary(MRred)
confint(MRred)
Hlogmod$coefficients
confint(Hlogmod)



###################################################################
###################################################################

# PDF code:


```{r echo=FALSE}
# See Zhang_midterm1_Cutler.R notepad for which libraries needed to be installed first.
suppressPackageStartupMessages(library(sas7bdat))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(segmented)) # installed in midterm1 notepad
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(stargazer)) # installed in midterm1 notepad
```

```{r echo=FALSE}
ch = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/cheese.sas7bdat",
                   debug = TRUE)

colnames(ch) = c("case","taste","Lacid","Aacid","Hsulf")
```


## 1. Why is violation of normality in the outcome variable not a problem:
### Because in this case it's close enough to being normal:
```{r echo=FALSE}
qqnorm(ch$taste)
qqline(ch$taste)
```

## 2. SLR and LOESS plots; SLR models (Tables 1-3, with CIs following):
```{r echo=FALSE, results='asis'}
Lmod = lm(taste ~ Lacid, data = ch)
plot(ch$Lacid,ch$taste, xlab = "lactic acid", ylab = "taste")
abline(Lmod, col = "red")
Amod = lm(taste ~ Aacid, data = ch)
plot(ch$Aacid,ch$taste, xlab = "acetic acid", ylab = "taste")
abline(Amod, col = "blue")
Hmod = lm(taste ~ Hsulf, data = ch)
plot(ch$Hsulf,ch$taste, xlab = "H sulfide", ylab = "taste")
abline(Hmod, col = "green")

stargazer(Lmod, title = "SLR model for lactic acid ('Lacid')", header = FALSE)
stargazer(Amod, title = "SLR model for acetic acid ('Aacid')", header = FALSE)
stargazer(Hmod, title = "SLR model for hydrogen sulfide ('Hsulf')", header = FALSE)
confint(Lmod)
confint(Amod)
confint(Hmod)

ggplot(ch, aes(Lacid,taste)) +
  geom_point() +
  geom_smooth()

ggplot(ch, aes(Aacid,taste)) +
  geom_point() +
  geom_smooth()

ggplot(ch, aes(Hsulf,taste)) +
  geom_point() +
  geom_smooth()

```


## 3. Equations of models:

### Association between taset and acetic acid:

### Taste and lactic acid:

### Taste and H sulfide:



## 4. Sentences quantifying the associations:

### Taste and acetic acid:

### Taste and lactic acid:

### Taste and H sulfide:



### 5. Because unless we use CIs, our point estimate of the population parameter in question will always be guaranteed to be wrong (not equal to the true parameter), at least when dealing with continuous data. This is because the probability of a random variable taking on a specific value is essentially zero, due to the laws of probability. A CI will at least give us a good chance of including the true parameter within the sweep of the CI.



### 6. 'Statistic' refers to a function of a sample (e.g. X-bar is defined as the sum of the Xi's divided by the sample size), and is meant to approximate a true population parameter. Parameter estimates can change depending on what methods are used to calculate them, while the sample statistics shouldn't vary for the same data. An example illustrating this will come up below, in question 24.



### 7. A least squares estimate is an estimate that minimizes the amount of squared error, or distance between Y-hat (predicted Y values) and the observed Y values.



## 8. Comparing linear and quadratic models for lactic acid (quadratic: Table 4):
```{r echo=FALSE, results='asis'}
Lacid2 = (ch$Lacid)^2
Lacid = ch$Lacid
taste = ch$taste

Lquad = lm(taste ~ Lacid + Lacid2)
stargazer(Lquad, title = "Quadratic model for lactic acid", header = FALSE)

plot(Lacid,taste)
Lacidvals = seq(.85,2.05,.01)
predL = predict(Lquad, list(Lacid=Lacidvals,Lacid2=Lacidvals^2))
lines(Lacidvals,predL, col = "purple", lwd = 3)
```

#### MSE and R^2 for lactic acid linear model:
```{r echo=FALSE}
mse.L = mean(Lmod$residuals^2); mse.L
rL = cor(ch$Lacid,ch$taste); rL^2
``` 

#### MSE and R^2 for lactic acid quadratic model:
```{r echo+FALSE}
mse.Lq = mean(Lquad$residuals^2); mse.Lq
# Get r^2 from Lquad?
```

### The quadratic model looks better than the linear, based on the higher R^2 value and the lower MSE.


## 9. Why must the quadratic model also include an unsquared term?



## 10. Comparing linear and quadratic models for acetic acid (quadratic: Table 5):
```{r echo=FALSE, results='asis'}
Aacid2 = (ch$Aacid)^2
Aacid = ch$Aacid

Aquad = lm(taste ~ Aacid + Aacid2)
stargazer(Aquad, title = "Quadratic model for acetic acid", header = FALSE)

plot(Aacid,taste)
Aacidvals = seq(min(Aacid),max(Aacid),1)
predA = predict(Aquad, list(Aacid=Aacidvals,Aacid2=Aacidvals^2))
lines(Aacidvals,predA, col = "purple", lwd = 3)
```

### MSE and R^2 for acetic acid linear model:
```{r echo=FALSE}
mse.A = mean(Amod$residuals^2); mse.A
rA = cor(ch$Aacid,ch$taste); rA^2
```

### MSE and R^2 for acetic acid quadratic model:
```{r echo=FALSE}
mse.Aq = mean(Aquad$residuals^2); mse.Aq
# Get R^2 for Aquad
```



## 11. Define the type III SS for the acetic acid quadratic model:
```{r echo=FALSE}
Anova(Aquad, type = "III")
```

### The type III sum of squares for the acetic acid quadratic model is 609.2 for the unsquared acetic acid, and 221.3 for the squared acetic acid, with a residual SS of 5,423.



## 12. Run a segmented regression model on hydrogen sulfide with a knot at concentration of 2500 mg/L:
```{r echo=FALSE}
# Get segmented regresion model

qplot(Hsulf, taste, group = Hsulf > 2500, geom = c('point','smooth'),
      method = "lm", se = F, data = ch) +
  geom_vline(xintercept = 2500, linetype = 3, col = "gray")
```



## 13. Hydrogen sulfide segmented regression model's coefficients and hypothesis tests:



## 14. MSE and R^2 for the hydrogen sulfide segmented regression model:



## 15. Simple linear regression of log-transformed hydrogen sulfide (Table 6):
```{r echo=FALSE, results='asis'}
logHS = log(ch$Hsulf)
Hlogmod = lm(taste ~ logHS)
stargazer(Hlogmod, title = "SLR model for log-transformed hydrogen sulfide", header = FALSE)
```

### MSE and R^2 for the logHS SLR model and untransformed SLR model:
#### logHS:
```{r echo=FALSE}
mse.Hlog = mean(Hlogmod$residuals^2); mse.Hlog
```

#### Untransformed:
```{r echo=FALSE}
mse.H = mean(Hmod$residuals^2); mse.H
```

### Plot of the log-transformed data:
```{r echo=FALSE}
plot(log(ch$Hsulf),ch$taste)
abline(Hlogmod, col = "darkgreen")
```



## 16. Comparing the hydrogen sulfide log-transformed SLR model to the segmented model (including definitions of the MSE and R^2 statistics):



## 17. Residual plots for the hydrogen sulfide log-transformed SLR model and segmented model (which is preferrable?):



## 18. What kind of residuals did I use to perform the regression diagnostics, and why?



## 19. What are the consequences for a statistical model whose underlying assumptions are violated? Why do we care?



## 20. What is the meaning of the intercept in a regression model?



## 21. We do not always interpret the intercept when we report results from a regression model. Under what circumstances would we be able to interpret the intercept?



## 22. Which multivariable regression model best fits the observations on taste? How did I arrive at my final model? (Full model is Table 7, and Reduced model is Table 8)
```{r echo=FALSE, results='asis'}
MRfull = lm(taste ~ Lacid*Aacid*logHS)
stargazer(MRfull, title = "Full MLR model", header = FALSE)
```

### The full model above shows no significant interactions. This indicates that the reduced or main effects model is worth examining:
```{r echo=FALSE, results='asis'}
MRred = lm(taste ~ Lacid+Aacid+logHS)
stargazer(MRred, title = "Reduced (main effects) MLR model", header = FALSE)
confint(MRred)
```

### The reduced model is my choice for the final model, given that no significant interactions exist.



## 23. Describe the association of taste with the chemicals using the coefficients and CIs from the final model. Recall in your description that the model's coefficients represent adjusted estimates.



## 24. Comparison between the coefficient and CI for the log-transformed hydrogen sulfide SLR model and the coefficient and CI for the log-transformed hydrogen sulfide in the MLR model:
### SLR:
```{r echo=FALSE}
Hlogmod$coefficients
confint(Hlogmod)
```

### As can be seen, the coefficients from the SLR model for log-transformed hydrogen sulfide and those of the same log-transformed chemical in the MLR model are different. Adjustment affects parameter estimates in MLR models ...




