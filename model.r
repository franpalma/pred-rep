setwd('D:/pred-rep/tanzeem_noor-promise17_data')

#load raw data
closure_compiler <- read.csv("Closure-Compiler Metrics Raw_Data.csv", header = TRUE)


library(caTools)

#my_model <- glm(closure_compiler$Result ~ closure_compiler$ST + closure_compiler$MC + closure_compiler$BC, family = binomial(link = 'logit'), data = closure_compiler)

#summary(my_model)

#plot(my_model)

lrfit <- glm(closure_compiler$Result ~ closure_compiler$ST + closure_compiler$MC + closure_compiler$BC, family = binomial)

lrfit

summary(lrfit)

pdf(file='model.pdf')
plot(lrfit)
dev.off()

residuals(lrfit)

fitted.values(lrfit)

coef(lrfit)

