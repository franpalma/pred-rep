setwd('D:/pred-rep/tanzeem_noor-promise17_data')

data <- read.csv('Closure-Compiler Metrics Raw_Data.csv',header=T,na.strings=c(""))

linear1 <- lm(data$Result~data$MC, data=data) 

summary(linear1)
