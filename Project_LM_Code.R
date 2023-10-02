#### LINEAR MODELS ####
### GROUP 5 - PROJECT ###



rm(list=ls())


#loading the necessary libraries
library(MASS) 
library(lmtest)
library(skedastic)


#setting up the working directory and loading the data and creating the model
setwd("C:/Users/anaso/Desktop/SOFIA MENDES/KU Leuven/Year 1/Semester 1/Linear Models/Assignment")
data.full = read.table("dataset.txt",header=T)

set.seed(202205) #group number 05
names(data.full)
attach(data.full)


#changing edu, wealth and akan to factors and changing the position of HAZ 
data.full$edu <- as.factor(data.full$edu)
data.full$wealth <- as.factor(data.full$wealth)
data.full$akan <- as.factor(data.full$akan)
data.full <- data.frame(data.full[,-2],HAZ)

# Splitting the data in training and validation set
d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]
attach(data.training)






###################################
### Exploratory Factor Analysis ###
###################################


library(GGally)
ggpairs(data.full)  + theme_bw() #graphical representation

#Categorical variables

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
plotEdu <- ggplot(data.full)+ theme_light()+ geom_bar(aes(edu))+ggtitle("edu")
plotWealth <- ggplot(data.full)+ theme_light()+ geom_bar(aes(wealth))+ggtitle("wealth")
plotAkan <- ggplot(data.full)+ theme_light()+ geom_bar(aes(akan))+ggtitle("akan")
grid.arrange(plotEdu,plotWealth,plotAkan, ncol=2)


#Numerical variables

#histograms
par(mfrow = c(1,3))
hist(age) #the histogram shows that the distribution is not normal.
hist(blf)
hist(blm)

#exploratory addition
boxplot(HAZ[which(akan == 0)], HAZ[which(akan == 1)], names = c("0", "1"), ylab = "HAZ")
boxplot(HAZ[which(edu == 1)], HAZ[which(edu == 2)], HAZ[which(edu == 3)], names = c("1", "2", "3"), ylab = "HAZ")
boxplot(HAZ[which(wealth == 1)], HAZ[which(wealth == 2)], HAZ[which(wealth == 3)], names = c("1", "2", "3"), ylab = "HAZ")
#avg HAZ is higher for more educated, more rich, and children with the akan ethnicity


par(mfrow = c(1,1))

#correlation among variables
summary(data.training)
head(data.training)
pairs(data.training)
pairs(data.training, panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")})



#multicollinearity
corx <- cor(data.training[,-c(2,3,4,7)]) 
corx
#removing the correlation caused by AKAN, edu, wealth (because they are factors) variable
#the numerical variables have low correlation


#VIF (Variance inflation factor)
data.VIF <- diag(solve(corx))
data.VIF
# VIF of all variables is close to 1, weak correlated, not enough to be overly concerned

#eigenvalues
corx.eig <- eigen(corx)$values
corx.eig
sqrt(max(corx.eig)/corx.eig)
# eigenvalues does not indicate multicollinearity


#boxplot
boxplot(HAZ, main = "HAZ") #outliers found
boxplot(age, main = "age")
boxplot(blm, main = "BLM") #outliers found
boxplot(blf, main = "BLF") #outliers found





###########################
### ANALYSIS FULL MODEL ###
###########################


#fitting the full model
#HAZ ~ age + edu + wealth + akan + blm + blf
fit1 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf, data = data.training)
fit1.sum <- summary(fit1)
fit1.sum
#here age, wealth and blf (weakly) are the only the significant regressors.
#The p-value associated with F-statistic suggest that the regressors are jointly
#significant.
#Adjusted R-squared = 0.1131, which means that only 11.31% of the variance is explained 
#by the regressors, which is very low for a good model. 


#checking model assumptions
fit1.res <- residuals(fit1)
fit1.stdres <- stdres(fit1)
fit1.fittedvalues <- fitted.values(fit1)


par(mfrow=c(2,2))
qqnorm(fit1.stdres, main="")
qqline(fit1.stdres)
#the QQ-Plot shows that the tails are heavier than a normal distribution.
plot(fit1.res, xlab = "Index", ylab = "Residual")



###check heteroscedasticity
plot(fit1.fittedvalues, fit1.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit1.res ~ fit1.fittedvalues), col = "red")
#We might be in presence of heteroskedasticty.
plot(fit1.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
#some outliers

#standardized residuals vs regressors
par(mfrow = c(1,3))
plot(age, fit1.stdres, ylab = "Residual")
lines(lowess(fit1$residuals ~ age), col = "red") # not linear convex parabolic
plot(blm, fit1$residuals, ylab = "Residual")
lines(lowess(fit1$residuals ~ blm), col = "red") #not linear concave
plot(blf, fit1$residuals, ylab = "Residual")
lines(lowess(fit1$residuals ~ blf), col = "red") #not linear curved
par(mfrow = c(1,1))

# Partial residual plots
fit1.coef <- coefficients(fit1)
fit1.pres.age <- fit1.res + fit1.coef[2] *age
fit1.pres.blm <- fit1.res + fit1.coef[8] *blm
fit1.pres.blf <- fit1.res + fit1.coef[9] *blf
par(mfrow = c(1,3))
plot(age, fit1.pres.age, ylab = "Partial residual (age)")
abline(lm(unname(fit1.pres.age) ~ age))
lines(lowess(age, fit1.pres.age), col = "red") # convex curvature
plot(blm,fit1.pres.blm, ylab = "Partial residual (blm)")
abline(lm(fit1.pres.blm ~ blm))
lines(lowess(blm, fit1.pres.blm), col = "red") #concave curvature
plot(blf,fit1.pres.blf, ylab = "Partial residual (blf)")
abline(lm(fit1.pres.blf ~ blf))
lines(lowess(blf, fit1.pres.blf), col = "red") #not linear
# plots suggest non linear terms






######################################
#### Improvement of the full model####
######### Transformations ############
######################################


### Box-Cox transformation 
#HAZ has a minimum of -6.052. For this step, we add 7 to HAZ to ensure all responses>0
out <- boxcox((HAZ+7) ~ age + edu + wealth + akan + blm + blf, lambda = seq(-0.5,0.5,0.001), plotit = TRUE, data=data.training)
lambda <- out$x[which(out$y == max(out$y))]
lambda
fit2_boxcox <- lm(((HAZ+7)^lambda - 1)/lambda ~ age + edu + wealth + akan + blm + blf, data=data.training)
summary(fit2_boxcox)
#the R^2 gets lower than the full model


# Detection of heteroscedasticity for fit2_boxcox
fit2_boxcox.stdres <- stdres(fit2_boxcox)
fit2_boxcox.fittedvalues <- fitted.values(fit2_boxcox)
par(mfrow = c(1,3))
plot(fit2_boxcox.fittedvalues, fit2_boxcox.stdres, xlab = "Fitted value", ylab = "Standardized residual")
lines(lowess(fit2_boxcox.fittedvalues, fit2_boxcox.stdres), col = "red")
plot(fit2_boxcox.fittedvalues, abs(fit2_boxcox.stdres), xlab = "Fitted value", ylab = "|Standardized residual|")
lines(lowess(fit2_boxcox.fittedvalues , fit2_boxcox.stdres), col = "red")
plot(fit2_boxcox.fittedvalues, fit2_boxcox.stdres^2, xlab = "Fitted value", ylab = "Squared standardized residual")
lines(lowess(fit2_boxcox.fittedvalues , fit2_boxcox.stdres), col = "red")
par(mfrow = c(1,3))
plot(age, fit2_boxcox.stdres, ylab = "Standardized residual")
plot(blf, fit2_boxcox.stdres, ylab = "Standardized residual")
plot(blm, fit2_boxcox.stdres, ylab = "Standardized residual")



#### Weighted least squares
w <- 1/(lm(abs(fit1.stdres) ~ fit1.fittedvalues, data = data.training)$fitted.values^2)
fit_wls <- lm(HAZ ~ age + wealth + edu + blf + blm + akan, data = data.training, weight = w)
summary(fit_wls)

#checking model assumptions
par(mfrow = c(2, 2))
plot(fit_wls) #QQ-Plot suggests that the tails are heavier than the normal distribution
fit_wls.stdres <- stdres(fit_wls)
fit_wls.fittedvalues <- fitted.values(fit_wls)

fit_wls.res <- fit_wls$residuals
fit_wls.fittedvalues <- fit_wls$fitted.values
plot(fit_wls.fittedvalues, fit1.res, xlab = "Fitted value", ylab = "residual")
lines(lowess(fit1.fittedvalues, fit1.stdres), col = "red")
plot(fit_wls.fittedvalues, fit_wls.res*sqrt(w), xlab = "Fitted value", ylab = "Weighted residual")
lines(lowess(fit_wls.fittedvalues, fit_wls.stdres), col = "red")


plot(data.training$age, fit_wls.res, xlab = "age", ylab = "Residual")
lines(lowess(data.training$age , fit1.stdres), col = "red")
plot(data.training$age, fit_wls.res*sqrt(w), xlab = "age", ylab = "Weighted residual")
lines(lowess(data.training$age , fit_wls.res*sqrt(w)), col = "red")
#the residual and weighted residuals are similar

plot(data.training$blm, fit_wls.res, xlab = "blm", ylab = "Residual")
lines(lowess(data.training$blm , fit1.stdres), col = "red")
plot(data.training$blm, fit_wls.res*sqrt(w), xlab = "blm", ylab = "Weigthed residual")
lines(lowess(data.training$blm , fit_wls.res*sqrt(w)), col = "red")
#similar 

plot(data.training$blf, fit_wls.res, xlab = "blf", ylab = "Residual")
lines(lowess(data.training$blf , fit1.stdres), col = "red")
plot(data.training$blf, fit_wls.res*sqrt(w), xlab = "blf", ylab = "Weigthed residual")
lines(lowess(data.training$blf , fit_wls.res*sqrt(w)), col = "red")
#similar





### Log(HAZ)

#HAZ has a minimum of -6.052. For this step, we add 7.052 to HAZ to ensure all responses>0
fit2_logy <- lm(log(HAZ+7.052) ~ age + edu + wealth + akan + blm + blf, data=data.training)
summary(fit2_logy)
#The R^2 is lower

# Detection of heteroscedasticity for fit2_logy
fit2_logy.stdres <- stdres(fit2_logy)
fit2_logy.fittedvalues <- fitted.values(fit2_logy)
par(mfrow = c(1,3))
plot(fit2_logy.fittedvalues, fit2_logy.stdres, xlab = "Fitted value", ylab = "Standardized residual")
lines(lowess(fit2_logy.fittedvalues, fit2_logy.stdres), col = "red")
plot(fit2_logy.fittedvalues, abs(fit2_logy.stdres), xlab = "Fitted value", ylab = "|Standardized residual|")
lines(lowess(fit2_logy.fittedvalues , fit2_logy.stdres), col = "red")
plot(fit2_logy.fittedvalues, fit2_logy.stdres^2, xlab = "Fitted value", ylab = "Squared standardized residual")
lines(lowess(fit2_logy.fittedvalues , fit2_logy.stdres), col = "red")
par(mfrow = c(1,3))
plot(age, fit2_logy.stdres, ylab = "Standardized residual")
plot(blf, fit2_logy.stdres, ylab = "Standardized residual")
plot(blm, fit2_logy.stdres, ylab = "Standardized residual")



###Adding higher order terms

#age^2 - term that is ***
fit_agesq <- lm(HAZ ~ age + edu + wealth + akan + blm + blf+ I(age^2), data = data.training)
summary(fit_agesq)
# age^2 is significant, R^2 increased

#blf^2 - term is not significant
fit_blfsq <- lm(HAZ ~ age + edu + wealth + akan + blm + blf+ I(blf^2), data = data.training)
summary(fit_blfsq)

#blm^2- term is not significant
fit_blmsq <- lm(HAZ ~ age + edu + wealth + akan + blm + blf+ I(blm^2), data = data.training)
summary(fit_blmsq)


#checking model assumptions for fit_agesq as age^2 is significant
fit_agesq.res <- residuals(fit_agesq)
fit_agesq.stdres <- stdres(fit_agesq)
fit_agesq.fittedvalues <- fitted.values(fit_agesq)
par(mfrow = c(2,2))
qqnorm(fit_agesq.stdres, main="")
qqline(fit_agesq.stdres)
#the QQ-Plot shows that the tails are heavier than a normal distribution.
plot(fit_agesq.res, xlab = "Index", ylab = "Residual")
#HETEROSKEDASTIC
plot(fit_agesq.fittedvalues, fit_agesq.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit_agesq.res ~ fit_agesq.fittedvalues), col = "red")
#The lowess curve is more horizontal than in the full model.
plot(fit_agesq.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
#some outliers


#standardized residuals vs regressors
par(mfrow = c(1,3))
plot(age, fit_agesq.stdres, ylab = "Residual")
lines(lowess(fit_agesq$residuals ~ age), col = "red") # curvature reduced
plot(blm, fit_agesq$residuals, ylab = "Residual")
lines(lowess(fit_agesq$residuals ~ blm), col = "red") #not linear concave
plot(blf, fit_agesq$residuals, ylab = "Residual")
lines(lowess(fit_agesq$residuals ~ blf), col = "red") #not linear curved
par(mfrow = c(1,1))




### Adding interaction effects

#edu*wealth- not significant
fitI1 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +edu*wealth, data = data.training)
summary(fitI1)

#age*edu- not significant
fitI2 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +edu*age, data = data.training)
summary(fitI2)

#age*wealth- weak significance *
fitI3 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +age*wealth, data = data.training)
summary(fitI3)

#blf*blm not significant
fitI4 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +blm*blf, data = data.training)
summary(fitI4)

#age*akan- not significant
fitI5 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +age*akan, data = data.training)
summary(fitI5)

#age*blm- not significant
fitI6 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +age*blm, data = data.training)
summary(fitI6)

#age*blf- not significant
fitI7 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +age*blf, data = data.training)
summary(fitI7)

#edu*blm- weak significant
fitI8 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +blm*edu, data = data.training)
summary(fitI8)

#akan*edu - not significant
fitI9 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +akan*edu, data = data.training)
summary(fitI9)

#akan*wealth - not significant
fitI10 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +akan*wealth, data = data.training)
summary(fitI10)



###model with the two weak interactions found before
#blm*edu and age*wealth - both are significant
fitI11 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +blm*edu + age*wealth, data = data.training)
summary(fitI11)
#both the interactions are significant and R^2 is higher



### model with age^2 and two interactions
fitI12 <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +blm*edu + age*wealth + I(age^2), data = data.training)
summary(fitI12)
# best fitting model


#comparing the two best models- fitI12(both interactions+ age^2) and fit_agesq(age^2)
anova(fitI12, fit_agesq)
#the p-value suggests that the predictive power of two models is significantly 
#different. fitI12 might be better because it has higher R^2.


### checking model assumptions for fitI12
fitI12.res <- residuals(fitI12)
fitI12.stdres <- stdres(fitI12)
fitI12.fittedvalues <- fitted.values(fitI12)
par(mfrow = c(2,2))
qqnorm(fitI12.stdres, main="")
qqline(fitI12.stdres)
#the QQ-Plot shows that the tails are heavier than a normal distribution.
plot(fitI12.res, xlab = "Index", ylab = "Residual")

# Detection of heteroscedasticity for fitI12
plot(fitI12.fittedvalues, fitI12.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fitI12.res ~ fitI12.fittedvalues), col = "red")
#The lowess curve is more horizontal than in the full model.
plot(fitI12.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
#some outliers

fitI12.stdres <- stdres(fitI12)
fitI12.fittedvalues <- fitted.values(fitI12)

plot(fitI12.fittedvalues, fitI12.stdres, xlab = "Fitted value", ylab = "Standardized residual") #few outliers
plot(fitI12.fittedvalues, abs(fitI12.stdres), xlab = "Fitted value", ylab = "|Standardized residual|")
plot(fitI12.fittedvalues, fitI12.stdres^2, xlab = "Fitted value", ylab = "Squared standardized residual")
# all plots detect heteroscedasticity

#standardized residuals vs regressors
par(mfrow = c(1,3))
plot(age, fitI12.stdres, ylab = "Residual")
lines(lowess(fitI12$residuals ~ age), col = "red") # curvature reduced
plot(blm, fitI12$residuals, ylab = "Residual")
lines(lowess(fitI12$residuals ~ blm), col = "red") #not linear concave
plot(blf, fitI12$residuals, ylab = "Residual")
lines(lowess(fitI12$residuals ~ blf), col = "red") #not linear curved
par(mfrow = c(1,1))




### standardizing the training data
std.data <- data.frame(scale(data.training[,-c(2,3,4)], center = T,scale = T))
fit_std <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +blm*edu + age*wealth + I(age^2), data = std.data)
summary(fit_std)

#This does not improve the model









#########################
### Outlier Detection ###
#########################


## linear regression model with all variables (full model - fitI12)

# Standardized residuals
fitI12.stdres <- stdres(fitI12)
plot(fitI12.stdres, ylim = c(-5,5), ylab = "Standardized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Studentized residuals
fitI12.studres <- studres(fitI12)
plot(fitI12.studres, ylim = c(-5,5), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagonal elements of hat matrix
fitI12.influence <- influence(fitI12)
plot(fitI12.influence$hat, ylab = "Diagonal elements of hat matrix")
n <- dim(data.training)[1]
p <- dim(data.training)[2]
abline(h = 2*p/n, col = "red")
2*p/n
fitI12.influence$hat[fitI12.influence$hat>2*p/n]
# approximately 1/3 of the training data seems to be considered vertical outliers


# DFFITS
fitI12.dffits <- dffits(fitI12)
plot(fitI12.dffits, ylab = "DFFITS")
abline(h = 2*sqrt(p/n), col = "red")
2*sqrt(p/n)
fitI12.dffits[fitI12.dffits>(2*sqrt(p/n))]
# outliers: 112,170,174,200,223,240,354,387,420,444,504,515,647,680,711,757,927,1007,1013,1022,1044,1067,1102,1121,1136,1152,1153,1196,1208,1212 


# Cook's distance
fitI12.Cd <- cooks.distance(fitI12)
plot(fitI12.Cd, ylab = "Cook's distance")
abline(h = 1, col = "red") 
fitI12.Cd[fitI12.Cd>1]
#no outlier from this metric


# DFBETAS
fitI12.dfbetas <- dfbetas(fitI12)
fitI12.dfbetas
2/sqrt(n)
fitI12.dfbetas[fitI12.dfbetas>(2/sqrt(n))] 
plot(fitI12.dfbetas,ylab='DFBETAS')
abline(h = (2/sqrt(n)), col = "red")
#there are aprox 200 outliers from this metric (again, 1/3 of the training data)


# RLTS (50% breakdown value)
library(robustbase)
RLTS <- ltsReg(HAZ ~ age + edu + wealth +akan + blm + blf+I(age^2) + age*wealth + edu*blm , data = data.training, alpha=0.5)
summary(RLTS)

plot(RLTS, which = "rindex")
plot(RLTS, which = "rdiag") #MCD covariance matrix is singular - we can´t plot

# Standardized residuals
RLTS.stdres <- RLTS$residuals/RLTS$scale
plot(RLTS.stdres, ylim = c(-12,12), ylab = "Standardized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagnostic plot
# we can't plot since RLTS$RD shows singularity
plot(RLTS$RD,RLTS.stdres, ylim = c(-12,12), xlab = "Robust distances", ylab = "Standardized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")



# RLTS (10% breakdown value)
RLTS2 <- ltsReg(HAZ ~ age + edu + wealth +akan + blm+I(age^2) + blf + age*wealth + edu*blm, data = data.training, alpha = 0.9)
summary(RLTS2)

plot(RLTS2, which = "rindex")
plot(RLTS2, which = "rdiag")  #MCS covariance matrix is singular - we can´t plot

# Standardized residuals
RLTS.stdres <- RLTS$residuals/RLTS$scale
plot(RLTS.stdres, ylim = c(-12,12), ylab = "Standardized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagnostic plot
# we can't plot since RLTS$RD shows singularity
plot(RLTS2$RD,RLTS2.stdres, ylim = c(-12,12), xlab = "Robust distances", ylab = "Standardized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")


# removing outliers identified with "rindex"
data.training.wout <- data.training[-c(49,64,84,86,108,119,166,252,254,260,266,331,343,355,360,377,422,461)]
fitI12.wout <- lm(HAZ ~ age + edu + wealth + akan + blm + blf +blm*edu + age*wealth + I(age^2), data = data.training.wout)
summary(fitI12.wout)
# R^2 remains the same - no improvement













##########################
### Variable Selection ###
##########################


#AKAN, Wealth, Edu are categorical variables and we check it's significance using Partial F-test.
#Backward elimination based on AIC
fit.full <- lm(HAZ ~ age + edu + wealth + akan + blm + blf+I(age^2)+age*wealth + edu*blm, data=data.training)
fit.full
summary(fit.full)
library(MASS)
stepAIC(fit.full, scope = list(upper = ~ age + edu + wealth + akan + blm + age*wealth + edu*blm +
                                 blf +I(age^2), lower = ~ 1), direction = "backward")
#AIC=215.68
#Result : HAZ ~ age + edu + wealth + blm + blf + I(age^2) + age:wealth + edu:blm

#Forward elimination based on AIC
fit.null <- lm(HAZ ~ 1, data=data.training)
fit.null
stepAIC(fit.null, scope = list(upper = ~ age + edu + wealth + akan + blm + age*wealth + edu*blm +
                                 blf+I(age^2) , lower = ~ 1), direction = "forward", data=data.training)
#AIC=214.25
#Result : HAZ ~ wealth + age + I(age^2) + blf + wealth:age

#Stepwise selection based on AIC (started at null model)
stepAIC(fit.null, scope=list(upper = ~ age + edu + wealth + akan + blm + edu*blm +
                               age*wealth + blf+I(age^2), lower = ~ 1), direction = "both")
#AIC=214.25
#Result : HAZ ~ wealth + age + I(age^2) + blf + wealth:age




#Removing the dropped terms by AIC and making another linear model
fit_good <- lm(HAZ ~ wealth + age + I(age^2) + blf + wealth*age, data = data.training)  
summary(fit_good)
AIC(fit_good)
#the regressors are all significant
#the p-value associated with F-Statistic shows that all the regressors are
#jointly significant

#comparing the two linear models
anova(fit.full, fit_good)
# the prediction is not significantly different




# Check model assumptions for fit_good
par(mfrow =c(2,2))
plot(fit_good)

fit_good.res <- residuals(fit_good)
fit_good.stdres <- stdres(fit_good)
fit_good.fittedvalues <- fitted.values(fit_good)
par(mfrow = c(2,2))
qqnorm(fit_good.stdres, main="")
qqline(fit_good.stdres)
plot(fit_good.res, xlab = "Index", ylab = "Residual")
plot(fit_good.fittedvalues, fit_good.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit_good.res ~ fit_good.fittedvalues), col = "red")
plot(fit_good.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)


par(mfrow = c(2,2))
plot(age, fit_good.res, ylab = "Residual")
lines(lowess(fit_good.res ~ age), col = "red")
plot(blf, fit_good.res, ylab = "Residual")
lines(lowess(fit_good.res ~ blf), col = "red")

par(mfrow = c(1,1))










########################
### MODEL VALIDATION ###
########################
# comparison of some of the models previously analysed, fitted on the test set
attach(data.test)


#model 1
#training set
fit1.train <- lm(HAZ ~ age + edu + wealth + akan + blm + blf, data = data.training)
fit1.train
anova(fit1.train)
summary(fit1.train)
MSE <- summary(fit1.train)$sigma^2
MSE
sqrt(MSE)
AIC(fit1.train)
PRESS <- sum((residuals(fit1.train) / (1 - lm.influence(fit1.train)$hat))^2)
PRESS

#validation set
fit1.val <- lm(HAZ ~ age + edu + wealth + akan + blm + blf, data = data.test)
fit1.val
anova(fit1.val)
summary(fit1.val)
MSE1 <- summary(fit1.val)$sigma^2
MSE1
RMSE1 <- sqrt(MSE1)
RMSE1
MSEP1 <- mean((predict(fit1.val, newdata = data.test) - HAZ)^2)
MSEP1
AIC(fit1.val)
PRESS1 <- sum((residuals(fit1.val) / (1 - lm.influence(fit1.val)$hat))^2)
PRESS1




#model 2 --- HAZ ~ age + edu + wealth + akan + blm + blf + I(age^2) + age*wealth + blm*edu
fit2.train <- lm(HAZ ~ age + edu + wealth + akan + blm + blf + I(age^2) + age*wealth + blm*edu, data=data.training)
fit2.train
anova(fit2.train)
summary(fit2.train)
MSE <- summary(fit2.train)$sigma^2
MSE
sqrt(MSE)
AIC(fit2.train)
PRESS <- sum((residuals(fit2.train) / (1 - lm.influence(fit2.train)$hat))^2)
PRESS

#validation set
fit2.val <- lm(HAZ ~ age + edu + wealth + akan + blm + blf + I(age^2) + age*wealth + blm*edu, data=data.test)
fit2.val
anova(fit2.val)
summary(fit2.val)
MSE2 <- summary(fit2.val)$sigma^2
MSE2
RMSE2 <- sqrt(MSE2)
RMSE2
MSEP2 <- mean((predict(fit2.val, newdata = data.test) - HAZ)^2)
MSEP2
AIC(fit2.val)
PRESS2 <- sum((residuals(fit2.val) / (1 - lm.influence(fit2.val)$hat))^2)
PRESS2




#model 3 --- ((HAZ+7)^lambda - 1)/lambda ~ age + edu + wealth + akan + blm + blf    [Box-Cox]
fit3.train <- lm(((HAZ+7)^lambda - 1)/lambda ~ age + edu + wealth + akan + blm + blf, data=data.training) #lambda=0.5
fit3.train
anova(fit3.train)
summary(fit3.train)
MSE <- summary(fit3.train)$sigma^2
MSE
sqrt(MSE)
AIC(fit3.train)
PRESS <- sum((residuals(fit3.train) / (1 - lm.influence(fit3.train)$hat))^2)
PRESS

fit3.val <- lm(((HAZ+7)^lambda - 1)/lambda ~ age + edu + wealth + akan + blm + blf, data=data.test)
fit3.val
anova(fit3.val)
summary(fit3.val)
MSE3 <- summary(fit3.val)$sigma^2
MSE3
RMSE3 <- sqrt(MSE3)
RMSE3
MSEP3 <- mean((predict(fit3.val, newdata = data.test) - ((HAZ+7)^lambda - 1)/lambda)^2)
MSEP3
AIC(fit3.val)
PRESS3 <- sum((residuals(fit3.val) / (1 - lm.influence(fit3.val)$hat))^2)
PRESS3




#model 4 --- HAZ ~ age + edu + wealth + akan + blm + blf + I(age^2)
fit4.train <- lm(HAZ ~ age + edu + wealth + akan + blm + blf+I(age^2), data=data.training)
fit4.train
anova(fit4.train)
summary(fit4.train)
MSE <- summary(fit4.train)$sigma^2
MSE
sqrt(MSE)
AIC(fit4.train)
PRESS <- sum((residuals(fit4.train) / (1 - lm.influence(fit4.train)$hat))^2)
PRESS

fit4.val <- lm(HAZ ~ age + edu + wealth + akan + blm + blf+I(age^2), data=data.test)
fit4.val
anova(fit4.val)
summary(fit4.val)
MSE <- summary(fit4.val)$sigma^2
MSE
sqrt(MSE)
MSEP <- mean((predict(fit4.val, newdata = data.test) - HAZ)^2)
MSEP
AIC(fit4.val)
PRESS <- sum((residuals(fit4.val) / (1 - lm.influence(fit4.val)$hat))^2)
PRESS




#model 5
fit5.train <- lm(log(HAZ+7) ~ age + edu + wealth + akan + blm + blf, data=data.training) 
fit5.train
anova(fit5.train)
summary(fit5.train)
MSE <- summary(fit5.train)$sigma^2
MSE
sqrt(MSE)
AIC(fit5.train)
PRESS <- sum((residuals(fit5.train) / (1 - lm.influence(fit5.train)$hat))^2)
PRESS

fit5.val <- lm(log(HAZ+7) ~ age + edu + wealth + akan + blm + blf, data=data.test)
fit5.val
anova(fit5.val)
summary(fit5.val)
MSE <- summary(fit5.val)$sigma^2
MSE
sqrt(MSE)
MSEP <- mean((predict(fit5.val, newdata = data.test) - log(HAZ+7))^2)
MSEP
AIC(fit6.val)
PRESS <- sum((residuals(fit5.val) / (1 - lm.influence(fit5.val)$hat))^2)
PRESS




#model 6
fit6.train <- lm(HAZ ~ age + edu + wealth + akan + blm + blf+age*wealth, data=data.training)
fit6.train
anova(fit6.train)
summary(fit6.train)
MSE <- summary(fit6.train)$sigma^2
MSE
sqrt(MSE)
AIC(fit6.train)
PRESS <- sum((residuals(fit6.train) / (1 - lm.influence(fit6.train)$hat))^2)
PRESS

fit6.val <- lm(HAZ ~ age + edu + wealth + akan + blm + blf+edu*blm, data=data.test)
fit6.val
anova(fit6.val)
summary(fit6.val)
summary(fit6.val)
MSE <- summary(fit6.val)$sigma^2
MSE
sqrt(MSE)
MSEP <- mean((predict(fit6.val, newdata = data.test) - HAZ)^2)
MSEP
AIC(fit6.val)
PRESS <- sum((residuals(fit6.val) / (1 - lm.influence(fit6.val)$hat))^2)
PRESS






#table comparing models 1, 2 and 3

# MSE (Mean Squared Error)
MSE <- c(MSE1, MSE2, MSE3)
names(MSE) <- c("model1", "model2", "model3")
sort(MSE)

# RMSE
RMSE <- c(RMSE1, RMSE2, RMSE3)
names(RMSE) <- c("model1", "model2", "model3")
sort(RMSE)

# MSEP (Mean Squared Error for Predictions)
MSEP <- c(MSEP1, MSEP2, MSEP3)
names(MSEP) <- c("model1", "model2", "model3")
sort(MSEP)

# PRESS
PRESS <- c(PRESS1, PRESS2, PRESS3)
names(PRESS) <- c("model1", "model2", "model3")
sort(PRESS)

# Results
validation.results <- data.frame(rbind(MSE, RMSE, MSEP, PRESS), row.names = c("MSE", "RMSE", "MSEP", "PRESS"))
names(validation.results) <- c("model1", "model2", "model3")
validation.results
# Model 3 is the optimal model with respect to PRESS/n, MSE and MSEP


detach(data.training)
detach(data.test)
detach(data.full)
