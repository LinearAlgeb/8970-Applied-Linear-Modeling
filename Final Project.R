## Final Project_Wenjin Liu

#### Set working directory ####
setwd('/Users/wenjinl/Documents/UPenn/8970/dataset')
getwd()

#### Load dataset ####
good <- read.csv('good.csv')

# Subsetting
# use 5 additional variables
good <- na.omit(good[,c( 'mathraw97',
                         'readss97',
                         'WICpreg',
                         'AGE97',
                         'faminc97',
                         'bthwht',
                         'HOME97',
                         'bthord',
                         'pcgender',
                         'hdeduc97',
                         'wfeduc97',
                         'Parentspoor')])

##### Encoding, Scaling, and Subsetting #####
# Scaling: Standardization
library(scales)
good$mathss97 <- rescale(good$mathraw97, to = c(0, 50))
good$readss97 <- rescale(good$readss97, to = c(0, 50))
# Create new variable: score
good$score <- good$readss97 + good$mathss97

# Encode the binary variables
good$pcgender_dummy <- ifelse(good$pcgender == 1, 1, 0) # male:1, female:0
#good$bthord <- ifelse(good$bthord <= 3, 1, 0) # less than or equal to 3:1, greater than 3:0

# Encode the categorical variable
unique(good$Parentspoor)
good$Parentspoor_imputed <- ifelse(good$Parentspoor > 5, -1, 
                                   ifelse(good$Parentspoor == 0, -1, good$Parentspoor))
# poor:1, average/varied:3, pretty well:5, NK/NA/refused/other: NA

# Encode hdeduc97 and wfeuc97
par(mfrow=c(1,2))
boxplot(good$hdeduc97, main='hdeduc97')
boxplot(good$wfeduc97, main='wfeduc97')
good$hdeduc97 <- ifelse(good$hdeduc97 >= 98, NA, good$hdeduc97) # 98:DK, 99:NA, drop them
good$wfeduc97 <- ifelse(good$wfeduc97 >= 98, NA, good$wfeduc97)
## missing values and imputation
par(mfrow=c(1,2))
hist(good$hdeduc97, freq = F, breaks = 20)
hist(good$wfeduc97, freq = F, breaks = 20) # both are slightly left skewed
## thereby replace missing values with the median value
median_hdeduc97 <- median(good$hdeduc97, na.rm = TRUE)
median_wfeduc97 <- median(good$wfeduc97, na.rm = TRUE)
good$hdeduc97_imputed <- ifelse(is.na(good$hdeduc97), median_hdeduc97, good$hdeduc97)
good$wfeduc97_imputed <- ifelse(is.na(good$wfeduc97), median_wfeduc97, good$wfeduc97)

summary(good)
# subsetting
library(dplyr)
good <- select(good, -readss97, -mathss97, -mathraw97, -hdeduc97, -wfeduc97, -pcgender, -Parentspoor)


#### Data Exploration ####

##### Descriptives #####

# Descriptive Statistics
library(vtable)
st(good[,sapply(good, is.numeric)])

# Distributions
library(grid)
library(gridExtra)
library(ggplot2)
grid.arrange(ggplot(good, aes(score)) + geom_histogram(bins = 50),
             ggplot(good, aes(AGE97)) + geom_histogram(bins = 50),
             ggplot(good, aes(faminc97)) + geom_histogram(bins = 50),
             ggplot(good, aes(HOME97)) + geom_histogram(bins = 50),
             ggplot(good, aes(hdeduc97_imputed)) + geom_histogram(bins = 50),
             ggplot(good, aes(wfeduc97_imputed)) + geom_histogram(bins = 50),
             ncol = 3,
             top = textGrob('Distributions of 6 Variables'))

##### Log transformation #####

# faminc97:

## distribution of faminc97: highly right skewed.
par(mfrow=c(1,3))
hist(good$faminc97, freq = F, breaks = 50)
qqnorm(good$faminc97)
qqline(good$faminc97, col="red")

## relationship between faminc97 and mathrss97
plot(good$faminc97, good$score)
lines(lowess(good$faminc97, good$score), col="blue")

## log
min(good$faminc97)
good$logfaminc <- pmax(0, log1p(good$faminc97))

## After log-transformation:
par(mfrow=c(1,3))
hist(good$logfaminc, breaks = 50)
qqnorm(good$logfaminc)
qqline(good$logfaminc, col="red")
plot(good$logfaminc, good$score)
lines(lowess(good$logfaminc, good$score), col="blue")

## Considering moving the potential outliers
par(mfrow=c(1,3))
hist(good$logfaminc[good$logfaminc > 7], breaks = 50)
qqnorm(good$logfaminc[good$logfaminc > 7])
qqline(good$logfaminc[good$logfaminc > 7], col="red")
plot(good$logfaminc[good$logfaminc > 7], good$score[good$logfaminc > 7])
lines(lowess(good$logfaminc[good$logfaminc > 7], good$score[good$logfaminc > 7]), col="blue")

good$logfaminc <- ifelse(good$logfaminc > 7, good$logfaminc, NA)

##### Sqrt transformation #####

# HOME97:

## distribution of HOME97: left skewed.
par(mfrow=c(1,3))
hist(good$HOME97, freq = F, breaks = 20)
qqnorm(good$HOME97)
qqline(good$HOME97, col="red")

## relationship between faminc97 and mathss97
plot(good$HOME97, good$score)
lines(lowess(good$HOME97, good$score), col="blue")

## sqrt
min(good$HOME97)
good$sqhome <- pmax(0, sqrt(max(good$HOME97+1) - good$HOME97))

## After sqrt-transformation:
par(mfrow=c(1,3))
hist(good$sqhome, breaks = 20)
qqnorm(good$sqhome)
qqline(good$sqhome, col="red")
plot(good$sqhome, good$score)
lines(lowess(good$sqhome, good$score), col="blue")

library(dplyr)
goodR <- subset(good, select =c( 'score',
                                 'WICpreg',
                                 'AGE97',
                                 'logfaminc',
                                 'bthwht',
                                 'sqhome',
                                 'bthord',
                                 'pcgender_dummy',
                                 'hdeduc97_imputed',
                                 'wfeduc97_imputed',
                                 'Parentspoor_imputed'))
goodR <- na.omit(goodR)

##### Correlation ####
library(corrplot)
library(scico)
par(mfrow=c(1,1))
corrplot(cor(goodR[,sapply(goodR, is.numeric)],
             use = "pairwise.complete.obs"),  
         col = scico(200, direction = -1, palette = "batlow"), 
         tl.col = "black",
         tl.srt = 45,
         addgrid.col = 'grey',
         mar=c(0,0,1,0))

#par(mfrow=c(1,1))
#corrplot(cor(goodR[,sapply(goodR, is.numeric)], 
#             use = "pairwise.complete.obs"),
#         method = "color",
#         type="lower",
#         addCoef.col = T,
#         number.cex = 0.45,
#         number.digits = 3,
#         tl.col = "black",
#         tl.srt = 45,
#         addgrid.col = 'grey',
#         title = "Correlations",
#         mar=c(0,0,1,0))


#### Model Assumption Diagnostics and Corrections ####

##### Try Model #####
try_model <- step(lm(score ~ WICpreg + AGE97 +
                       logfaminc + sqhome + bthwht + bthord +
                       hdeduc97_imputed + wfeduc97_imputed+ pcgender_dummy + Parentspoor_imputed,
                     data = goodR),direction='both') # stepwise regression

library(stargazer)
stargazer::stargazer(try_model, type = "html")
library(car)
vif(try_model) # Multi-collinearity: wfeduc and pcgender, exclude them

##### Omitted Variable #####
## whether parentspoor_imputed is a relevant variable for the model

# Compute the correlation between parentspoor and other IVs
cor.test(goodR$Parentspoor_imputed, goodR$WICpreg, use = "complete.obs")
cor.test(goodR$Parentspoor_imputed, goodR$AGE97, use = "complete.obs")
cor.test(goodR$Parentspoor_imputed, goodR$logfaminc, use = "complete.obs")
cor.test(goodR$Parentspoor_imputed, goodR$bthwht, use = "complete.obs")
cor.test(goodR$Parentspoor_imputed, goodR$sqhome, use = "complete.obs")
cor.test(goodR$Parentspoor_imputed, goodR$bthord, use = "complete.obs")
cor.test(goodR$Parentspoor_imputed, goodR$hdeduc97_imputed, use = "complete.obs")

# Regression analysis
OVB <- lm(score ~  Parentspoor_imputed, data = goodR)
summary(OVB) # omit Parentspoor_imputed

try <- lm(score ~ WICpreg + AGE97 +
            logfaminc + sqhome + bthwht + bthord +
            hdeduc97_imputed, data = goodR)
stargazer::stargazer(try, type = "html")

##### Quadratic terms #####

# Distribution of AGE97
plot(jitter(goodR$AGE97), goodR$score)
lines(lowess(goodR$AGE97, goodR$score), col="blue")

# centering and squaring
goodR$AGE97c <- goodR$AGE97 - mean(goodR$AGE97)
goodR$AGE97c2 <- goodR$AGE97c^2
lm2 <- lm(score ~ WICpreg + AGE97c + AGE97c2 +
            logfaminc + sqhome + bthwht + bthord +
            hdeduc97_imputed, data = goodR)
summary(lm2)

# deciding what to include
anova(lm(score ~ WICpreg + AGE97c +
           logfaminc + sqhome + bthwht + bthord +
           hdeduc97_imputed, data = goodR),
      lm(score ~ WICpreg + AGE97c + AGE97c2 +
           logfaminc + sqhome + bthwht + bthord +
           hdeduc97_imputed, data = goodR))
# p-value less than 0.05, include age centered squared.

##### Base Model #####
base_model <- lm(score ~ WICpreg + AGE97c + AGE97c2 +
                   logfaminc + sqhome + bthwht + bthord + 
                   hdeduc97_imputed, data = goodR)
library(car)
vif(base_model)
library(stargazer)
stargazer::stargazer(base_model, type = "html", single.row=TRUE)


##### Diagnostics #####
par(mfrow = c(2, 2))
plot(base_model)

###### Linearity #####
scatterplot(score ~ AGE97c,  
            data = goodR, 
            boxplots = F, 
            grid = F, 
            jitter = list(x = 1), 
            col = rgb(0,0,0,.25), 
            pch = 16, 
            cex = .5, 
            regLine = list(method = lm, 
                           lty = 1, 
                           lwd = 2, 
                           col = 1), 
            smooth = list(method = loess, 
                          spread = T, 
                          lty.smooth = 2, 
                          lwd.smooth = 2, 
                          col.smooth = "red", 
                          lty.spread = 3, 
                          lwd.spread = 2, 
                          col.spread = "red"))
scatterplot(score ~ AGE97c2,  
            data = goodR, 
            boxplots = F, 
            grid = F, 
            jitter = list(x = 1), 
            col = rgb(0,0,0,.25), 
            pch = 16, 
            cex = .5, 
            regLine = list(method = lm, 
                           lty = 1, 
                           lwd = 2, 
                           col = 1), 
            smooth = list(method = loess, 
                          spread = T, 
                          lty.smooth = 2, 
                          lwd.smooth = 2, 
                          col.smooth = "red", 
                          lty.spread = 3, 
                          lwd.spread = 2, 
                          col.spread = "red"))
scatterplot(score ~ logfaminc,  
            data = goodR, 
            boxplots = F, 
            grid = F, 
            jitter = list(x = 1), 
            col = rgb(0,0,0,.25), 
            pch = 16, 
            cex = .5, 
            regLine = list(method = lm, 
                           lty = 1, 
                           lwd = 2, 
                           col = 1), 
            smooth = list(method = loess, 
                          spread = T, 
                          lty.smooth = 2, 
                          lwd.smooth = 2, 
                          col.smooth = "red", 
                          lty.spread = 3, 
                          lwd.spread = 2, 
                          col.spread = "red"))
scatterplot(score ~ sqhome,  
            data = goodR, 
            boxplots = F, 
            grid = F, 
            jitter = list(x = 1), 
            col = rgb(0,0,0,.25), 
            pch = 16, 
            cex = .5, 
            regLine = list(method = lm, 
                           lty = 1, 
                           lwd = 2, 
                           col = 1), 
            smooth = list(method = loess, 
                          spread = T, 
                          lty.smooth = 2, 
                          lwd.smooth = 2, 
                          col.smooth = "red", 
                          lty.spread = 3, 
                          lwd.spread = 2, 
                          col.spread = "red"))
scatterplot(score ~ hdeduc97_imputed,  
            data = goodR, 
            boxplots = F, 
            grid = F, 
            jitter = list(x = 1), 
            col = rgb(0,0,0,.25), 
            pch = 16, 
            cex = .5, 
            regLine = list(method = lm, 
                           lty = 1, 
                           lwd = 2, 
                           col = 1), 
            smooth = list(method = loess, 
                          spread = T, 
                          lty.smooth = 2, 
                          lwd.smooth = 2, 
                          col.smooth = "red", 
                          lty.spread = 3, 
                          lwd.spread = 2, 
                          col.spread = "red"))

###### Homoskedasticity #####
scatterplot(x = fitted(base_model),  
            y = resid(base_model), 
            main = "Residuals vs Fitted, Base Model", 
            boxplots = F, 
            grid = F, 
            col = rgb(0,0,0,.5), 
            pch = 16, 
            cex = .5, 
            regLine = F, 
            smooth = list(method = loess, 
                          spread = T, 
                          lty.smooth = 2, 
                          lwd.smooth = 2, 
                          col.smooth = "red", 
                          lty.spread = 3, 
                          lwd.spread = 2, 
                          col.spread = "red")) 
scatterplot(x = fitted(base_model),  
            y = abs(resid(base_model)), 
            main = "Absolute Residuals vs Fitted, Base Model", 
            boxplots = F, 
            grid = F, 
            col = rgb(0,0,0,.5), 
            pch = 16, 
            cex = .5, 
            regLine = F, 
            smooth = list(method = loess, 
                          spread = T, 
                          lty.smooth = 2, 
                          lwd.smooth = 2, 
                          col.smooth = "red", 
                          lty.spread = 3, 
                          lwd.spread = 2, 
                          col.spread = "red")) 

###### Normality of Residuals ######
par(mfrow = c(1, 2))
hist(resid(base_model), 
     border = "white", 
     main = "Histogram of Base Model Residuals", 
     cex.main = 0.8, 
     breaks = 100) 
qqPlot(rstandard(base_model), 
       main = list("Normal Q-Q, Base Model", cex = 0.8),
       col = rgb(0,0,0,.25), 
       pch = 16, 
       cex = .5, 
       col.lines = "red", 
       lwd = 1, 
       grid = F)

###### Outliers ######


## Add metrics from the regression output to the dataset
goodR$rstudent <- rstudent(base_model)       # studentized residuals
goodR$leverage <- hatvalues(base_model)      # leverage
goodR$cooksd <- cooks.distance(base_model)   # Cook's D

## Discrepancy: distance between predicted and observed values.
par(mfrow = c(1, 1))
plot(goodR$rstudent,
     xlab = "Index",
     ylab = "Studentized residuals",
     pch = 19)                          # Create scatterplot of studentized residuals
abline(h=0, col = "red")                # Add horizontal line at 0, +/-2, and +/-3.5
abline(h=2, col = "blue") 
abline(h=-2, col = "blue") 
abline(h=3.5, col = "green")
abline(h=-3.5, col = "green")
outlier1<-subset(goodR, abs(rstudent)>3.5)

## Leverage:
# threshold is (2k+2)/N
# k = 9: the number of IVs
# N = 1808: the number of observations
# So, (2k+2)/N = (2*8+2)/1801 = 0.00999
# Plot the observations with leverage exceeding 0.01106
plot(goodR$leverage, 
     xlab = "Index",
     ylab = "Leverage",
     pch = 19)
abline(h=.00999, col="blue")
abline(h=.02, col="green")
outlier2<-subset(goodR, leverage>.02)

# or directly see:
par(mfrow = c(1, 2))
plot(base_model, main = "Cook's distance, Base Model", which = 4) 
plot(base_model, main = "Residuals vs Leverage, Base Model", which = 5)

## Influence:
# Cook's D. Threshold is 4/N = 4/1801

## Estimate a linear model without observations with Cook's D greater than 4/1801
m2 <- lm(score ~ WICpreg + AGE97c + AGE97c2 +
           logfaminc + sqhome + bthwht + bthord +
           hdeduc97_imputed,
         data = goodR[goodR$cooksd <= (4/1801),])
library(stargazer)
stargazer::stargazer(base_model, m2, type = "html", single.row=TRUE)
# Increased R-square!

par(mfrow = c(1, 1))
plot(goodR$cooksd, 
     xlab = "Index",
     ylab = "Cook's D",
     pch = 19)
abline(h=4/1801, col="green")
# But we don't want to remove points in bulk solely based on their value of Cook's D
outlier3 <- subset(goodR, cooksd > 4/1808)

hist(outlier3$cooksd)
round(quantile(outlier3$cooksd, 
               probs = seq(0, 1, 0.05)), 4)
# a big jump between the 90th and 95th percentile, 95th:0.0115

## How to deal with outliers
# Method 1: Delete them
m3 <- lm(score ~ WICpreg + AGE97c + AGE97c2 +
           logfaminc + sqhome + bthwht + bthord +
           hdeduc97_imputed,
         data = goodR[goodR$cooksd < 0.0115,])
library(stargazer)
stargazer::stargazer(base_model, m3, type = "html", single.row=TRUE)
# Method 2: Set specific variable values to missing.
goodR$logfaminc_2 <- ifelse(goodR$logfaminc < 1, NA, goodR$logfaminc)
m4 <- lm(score ~ WICpreg + AGE97c + AGE97c2 +
           logfaminc_2 + sqhome + bthwht + bthord +
           hdeduc97_imputed,
         data = goodR)
stargazer::stargazer(base_model, m4, type = "html", single.row=TRUE)

###### Detecting Multicollinearity  #####
vif(base_model)
vif(m2)
vif(m3)
vif(m4)
# <10, didn't have multi-collinearity problems

##### Make choices: AIC #####
AIC(base_model)/nobs(base_model)
AIC(m2)/nobs(m2)
AIC(m3)/nobs(m3)
AIC(m4)/nobs(m4)
# The best model is m2, with the lowest AIC score. ##



#### Corrected Base Model ####

goodRR <- goodR[goodR$cooksd <= (4/1801),]

corrected_base_model <- lm(score ~ WICpreg + AGE97c + AGE97c2 +
                             logfaminc + sqhome + bthwht + bthord + 
                             hdeduc97_imputed, data = goodRR)
library(stargazer)
stargazer::stargazer(corrected_base_model, type = "html", single.row=TRUE)

#### Interaction effects ####

# Interaction effect: bthwht and bthord
bth <- lm(score ~ WICpreg  + AGE97c + AGE97c2 + 
            logfaminc + sqhome + bthwht + bthord + bthwht:bthord +
            hdeduc97_imputed, data = goodRR)
stargazer::stargazer(corrected_base_model, bth, type = "html", single.row=TRUE)

# Interaction effect: WICpreg and AGE97c2
lm_int1 <- lm(score ~ WICpreg + AGE97c + AGE97c2 + WICpreg:AGE97c2 +
                logfaminc + sqhome + bthwht + bthord +
                hdeduc97_imputed, data = goodRR)
stargazer::stargazer(corrected_base_model, lm_int1, type = "html", single.row=TRUE)

# Interaction effect: WICpreg and sqhome
lm_int2 <- lm(score ~ WICpreg  + AGE97c + AGE97c2 + 
                logfaminc + sqhome+ WICpreg:sqhome + bthwht + bthord +
                hdeduc97_imputed, data = goodRR)
stargazer::stargazer(corrected_base_model, lm_int2, type = "html", single.row=TRUE)
stargazer::stargazer(corrected_base_model, lm_int1, lm_int2, type = "html", single.row=TRUE)

# Final Model
lm_int <- lm(score ~ WICpreg  + AGE97c + AGE97c2 + WICpreg:AGE97c2 +
               logfaminc + sqhome+ WICpreg:sqhome + bthwht + bthord +
               hdeduc97_imputed, data = goodRR)
lm <- lm(score ~ WICpreg + AGE97c + AGE97c2 +
           logfaminc + sqhome + bthwht + bthord + 
           hdeduc97_imputed, data = goodRR)

stargazer::stargazer(lm, lm_int1, lm_int2, lm_int, type = "html", single.row=TRUE)
stargazer::stargazer(lm_int, type = "html", single.row=TRUE)
