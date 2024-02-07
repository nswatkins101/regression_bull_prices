# STAT611
# Final Project
# Nathaniel Watkins

# Use the dataset “E15F HW7.txt” (see the following figure 
# for description) and α = 0.05 when needed.

# sets working directory
setwd('C:/Users/nswat/OneDrive/Documents/University of Delaware/STAT611/final project')
# read the data from a text file
o_dat = read.table("bulls.txt")
colnames(o_dat) = c("breed", "selling_price", "yearling_height", "weight", "percent_body_fat", "frame", "back_fat", "sale_height", "sale_weight")
names(o_dat)

# The goal is to relate the selling price to the other characteristics. For the purpose of
# statistical analysis, take a natural logarithm of selling price. Also force breed and
# frame to be factors in R. Now the dataset is updated such that the response variable is
# the transformed selling price and the eight predictors are the characteristics of the bulls,
# where breed and frame are considered as categorical variables. From now on, all the
# subsequent statistical analysis is based on this updated dataset.
# In the analysis below, always include an intercept term in every model under
# consideration. Also always use a=0.05 whenever needed.
dat=o_dat
dat$logprice = log(o_dat$selling_price)
dat=dat[,-2]
dat$breed=as.factor(dat$breed)
dat$frame=as.factor(dat$frame)
colnames(dat)
print(dat)

# Questions:
# a) Obtain a matrix plot of all the variables. Also obtain the correlation
# matrix for all the variables except for breed and frame. What are your findings?
pairs(dat)
dat.corr = cor(dat[,c(-1, -5)])
print(dat.corr)
# The following pairs of variables have a correlation greater than 0.8:
# (yearling height, sale height). The following have a correlation 0.6-0.8:
# (yearling height, weight), (percent body fat, weight), (sale weight, weight)
# These numeric results can be graphically seen through the matrix plot of variables;
# there is a strong linear relationship between those pairs of variables.

# b) Fit a regression model with the eight predictors. Obtain the parameter
# estimates and the ANOVA table (as in the course lecture notes). Plot the residuals
# against the fitted response. Obtain a normal QQ plot of the standardized residuals.
# Are there any highly influential observations? Summarize your findings from all
# these R output.
fitb = lm(logprice ~ ., data=dat)
summary(fitb)
anova(fitb)
plot(fitb$fitted.values, fitb$residuals, main="Residuals vs fitted response")
std_residuals = rstandard(fitb)
qqnorm(std_residuals)
qqline(std_residuals)

# c) Carry out the backward elimination based on partial F-tests with all eight
# predictors and obtain a final model. Also show what models are considered in the
# intermediate steps (only show what predictors are included in each intermediate
# model; no need to show parameter estimates).
# Note: Since the model selection involves categorical variables, in each step, larger
# partial F statistic is not equivalent to smaller p-value. Therefore, please use pvalues to decide which predictors should be added or removed in each step.
full = lm(logprice ~ ., data=dat)
drop1(full, scope=~., test='F', data=dat)
back1 = update(full, ~.-percent_body_fat)
# current model is logprice ~ breed + yearling_height + weight + frame + back_fat + sale_height + sale_weight
drop1(back1, scope=~., test='F', data=dat)
back2 = update(back1, ~.-yearling_height)
# current model is logprice ~ breed + weight + frame + back_fat + sale_height + sale_weight
drop1(back2, scope=~., test='F', data=dat)
back3 = update(back2, ~.-sale_weight)
# current model is logprice ~ breed + weight + frame + back_fat + sale_height
drop1(back3, scope=~., test='F', data=dat)
back4 = update(back3, ~.-back_fat)
# current model is logprice ~ breed + weight + frame + sale_height
drop1(back4, scope=~., test='F', data=dat)
# stop backward elimination
# final model is logprice ~ breed + weight + frame + sale_height
fitc = back4


# d) (10 pts) Repeat part b) for the final model in part c).
summary(fitc)
anova(fitc)
std_residuals = rstandard(fitc)
plot(fitc$fitted.values, fitc$residuals, main="Residuals vs fitted response")
qqnorm(std_residuals)
qqline(std_residuals)

# e) It is desired to see if the interactions between breed and the other
# predictors are important in predicting the response. So the set of predictors now
# includes these interactions in addition to the original predictors. Carry out the
# backward elimination based on BIC and obtain a final model. Also show what
# models are considered in the intermediate steps (only show what predictors are
# included in each intermediate model; no need to show parameter estimates).
# Note: When you implement the backward elimination algorithm, please ignore
# the priority relations between linear terms and interaction terms.
full_int = lm(logprice ~ . +breed*., data=dat)
summary(full_int)
# I use step to calculate and print all the intermediate steps, BIC
fite = step(full_int, direction = "backward", k=log(nrow(dat)))

# f) Repeat part b) for the final model in part e).
summary(fite)
anova(fite)
plot(fite$fitted.values, fite$residuals, main="Residuals vs fitted response")
std_residuals = rstandard(fite)
qqnorm(std_residuals)
qqline(std_residuals)


# g) Of the models you have obtained in parts (b), (c) and (e), which one
# would you choose? Justify your answer.
# I would choose the model from part (c), because it only includes the most significant 
# predictors and does not have too many parameters. 

# what model to use for the rest of the project? Full model or the one picked above is fine

# h) Check for multicollinearity among the predictors. Use the Variance
# Inflation Factor (VIF) to detect if any variables are highly correlated. Which
# variable(s) show signs of multicollinearity, if any?
# model to be used for the rest of the project
full = lm(logprice ~ ., data=dat)
require(car)
alias(full)
vif(full)
# The variables yearling_height, sale height and frame have VIF>5, so they are highly correlated
# with the other predictors.

# i) Investigate heteroscedasticity in the residuals. Conduct a formal test for
# heteroscedasticity. What are your findings?
library(lmtest)
bptest(full)
# The p-value of the Breusch-Pagan test is p=0.564>a=0.05, so I cannot reject the null hypothesis
# that is, that the residuals are distributed with a constant variance. 

# j) Examine the linearity assumption. Plot the residuals against each
# predictor to check if any nonlinear patterns emerge. Based on your findings, do
# you think any transformations or polynomial terms are needed?
plot(dat$yearling_height, full$residuals)
plot(dat$weight, full$residuals)
plot(dat$percent_body_fat, full$residuals)
plot(dat$back_fat, full$residuals)
plot(dat$sale_height, full$residuals)
plot(dat$sale_weight, full$residuals)
# Considering the model after the log transformation
# It doesn't appear from the plots that there are any nonlinear patterns in the plots
# of the residuals against the predictors, for each the residuals are distributed uniformly
# about zero.


# k) Which breed seems to have the highest average selling price after
# adjusting for other variables in the model? Justify your answer using the results
# from the regression output.
summary(full)
# The intercepts of the linear model give us information about the mean sale price for
# the different breeds. Therefore, breed 1 (Angus) has the highest average selling price,
# as the coefficients of the breed 5 and breed 8 dummary variables are negative. 

# l) Interpret the coefficient of 'back fat' in the context of the data (in the full
# model). How does an increase in back fat (by 1 inch) affect the selling price,
# keeping other factors constant?
# The coefficient for the back fat variable describes the derivative of the selling price
# with respect to the back fat variable. Therefore, the price increases by the value
# of the coefficient for every unit increase of the back fat. In this case, the log price
# increases by 0.333 when the back fat increases by one inch.


# m) Estimate the confidence interval of the coefficient of 'yearling height at
# shoulder' (in the full model). Interpret the results.
# From the summary, I get the estimate to be 0.032094 and the standard error
# to be 0.0467328. Now, I just need to calculate the t statistic for
# the confidence interval.
n = nrow(dat)
coef = 0.032094
se = 0.0467328
width = qt(0.975,df=n-1)*se
lowerbound = coef - width
upperbound = coef + width
sprintf("95%% CI for yearling height coefficient is [%f, %f]", lowerbound, upperbound)
# This interval includes zero, which means the coefficient is not statistically significant,
# however, I already knew this from the output of the summary of the full model, where p=0.4947,
# for the coefficient of the yearling height variable.

# n) Which predictor has the least influence on the selling price according to
# the absolute size of its standardized coefficient?
# bi -> sy/sxi*bi
library(MuMIn)
std.coef(full, partial.sd=FALSE)

# The variable yearling height has a small t value, meaning that it has the least
# influence on te selling price of all the variables.

# o) Suppose a new variable is added to the dataset: "age of the bull (in
# months)". Discuss potential challenges and steps to incorporate this variable in
# your regression model. Would you expect multicollinearity issues? How would
# you address them? 
# There would definitely be multicollinearity issues with introducing the age of the bull
# as a variable. Presumably, the age of the bull would be correlated to the sale height and
# sale weight of the bull. In the presence of multicollinearity, one could use ridge regression
# to bypass the near singularity of the data matrix, or one could remove the sale height and
# sale weight variables, as in this case, the age of the bull would capture the same information.


