
########################################
#                                      #
#         Shrinkage Methods            #
#                                      #
########################################

setwd("~/Documents/Politecnico/Didattica/Master/MIP/2024_02 BABD/DAY V | Linear Models/R session")

#_______________________________________________________________________________
##### Ridge and Lasso regression of the Hitters dataset

library(ISLR)
#SET YOUR WOKRING DIRECatORY YO
data = read.csv('standardized_data.csv')

### Ridge Regression 

library(glmnet)
  help(glmnet)

# Build the matrix of predictors
x <- as.matrix(data[c('exports','health','imports','income','inflation','life_expec','total_fer','gdpp')])
# Build the vector of response
y <- data$child_mort

lambda.grid <- 10^seq(10,-2,length=100)
# That is for lambda = 4, but there may be better options!
# We could do cross-validation on the training set, to get the optimal lambda (homework)

# Instead, we now choose the optimal lambda via cross-validation on the entire dataset

## Cross-validation to choose the parameter lambda
set.seed(250117)

#optimizing for 
cv.out <- cv.glmnet(x,y,alpha=1) # default: 10-fold cross validation

# Returned results
names(cv.out)
cv.out$lambda # lambda used to fit the models
cv.out$cvm    # mean cross-validated error for each lambda
cv.out$lambda.min # value of lambda that gives minimum cvm

# Try to make a plot
plot(cv.out)
# MSE increases as lambda increases
abline(v=-2, lty=1)
# Indeed, the best lambda is the smallest
bestlam <- cv.out$lambda.min
bestlam
log(bestlam)

# refit the  regression model using lambda chosen by cross validation


fit.lasso <- glmnet(x,y,alpha=1,lambda=lambda.grid)
plot(fit.lasso,xvar='lambda',label=TRUE, col = rainbow(dim(x)[2]))
abline(v=log(bestlam), lty=1)
abline(v=-2, lty=1)
legend('bottomright', dimnames(x)[[2]], col =  rainbow(dim(x)[2]), lty=1, cex=.5)


graphics.off()
### The Lasso Regression
fit.lasso <- glmnet(x,y, lambda = exp(-2))# default: alpha=1 -> lasso

plot(fit.lasso)

result <- lm(y ~ as.matrix(data[c('imports','income')]))
shapiro.test(residuals(result))

par(mfrow=c(2,2))
plot(result)
summary(result)
#HOLY FUCK A SUPER CLEAR NON LINEAR RELATIONSHIP
result <- lm(y ~ data$imports + data$income + I(data$income^2))
shapiro.test(residuals(result))

par(mfrow=c(2,2))
plot(result)

graphics.off()

