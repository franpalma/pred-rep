setwd('D:/pred-rep/tanzeem_noor-promise17_data')

data.raw <- read.csv('Closure-Compiler Metrics Raw_Data.csv',header=T,na.strings=c(""))


data <- subset(data.raw,select=c(1, 4, 6, 7, 11, 12))

train_data <- data[ which(data$Version < max(data$Version)), ]

train_data <- train_data[-1]

summary(train_data)

test_data <- data[ which(data$Version == max(data$Version)), ]

test_data <- test_data[-1]

summary(test_data)

model <- glm(Result ~.,family=binomial(link='logit'), data = train_data)

summary(model)

anova(model, test="Chisq")

library(pscl)

pR2(model)

#fitted.results <- predict(model,newdata=subset(test_data,select=c(4)),type='response')
fitted.results <- predict.glm(model,newdata = as.data.frame(as.integer(data$ST), as.integer(data$MC), as.integer(data$CMC), as.integer(data$TM)) ,type='response')
?predict()

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)

print(paste('Accuracy',1-misClasificError))

library(ROCR)

p <- predict(model, newdata=subset(test,select=c(3,4,5,6,7,8)), type="response")

pr <- prediction(p, test$Survived)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)

auc <- performance(pr, measure = "auc")

auc <- auc@y.values[[1]]

auc
