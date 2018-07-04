
setwd('D:/pred-rep/tanzeem_noor-promise17_data')

data.raw <- read.csv('Closure-Compiler Metrics Raw_Data.csv',header=T)

data.test <- read.csv('RQ1_Closure-Compiler.csv',header=T)


train_data <- subset(data.raw,select=c(1, 4, 6, 7, 11, 12))


  
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM,data=train_data,family=binomial(link='logit'))

summary(fit)

anova(fit, test="Chisq")

#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)

test_data

fitted.results <- data.frame(predict(fit,newdata=test_data,type='response'))

confint(fit) # 95% CI for the coefficients

exp(coef(fit)) # exponentiated coefficients

exp(confint(fit)) # 95% CI for exponentiated coefficients

predict(fit, type="response") # predicted values

residuals(fit, type="deviance") # residuals


