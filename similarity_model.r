setwd('D:/pred-rep/tanzeem_noor-promise17_data')
library(dplyr)

####WITH TRADITIONAL METRICS...

#Closure-Compiler

#TRADITIONAL MODEL
data.raw.compiler <- read.csv('Closure-Compiler Metrics Raw_Data.csv',header=T)
train_data <- data.raw.compiler
fit <- glm(train_data$Result~train_data$CMC+train_data$MC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_compiler_Ver <- c()
output_rank_model_compiler_rankIndex <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_compiler_Ver[[idx]] <- i
        output_rank_model_compiler_rankIndex[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_compiler_Ver <- data.frame(output_rank_model_compiler_Ver)
output_rank_model_compiler_rankIndex <-  data.frame(output_rank_model_compiler_rankIndex)

#SIMILARITY MODEL
data.raw.compiler <- read.csv('Closure-Compiler Metrics Raw_Data.csv',header=T)
train_data <- data.raw.compiler
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_compiler_Ver_sim <- c()
output_rank_model_compiler_rankIndex_sim <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_compiler_Ver_sim[[idx]] <- i
        output_rank_model_compiler_rankIndex_sim[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_compiler_Ver_sim <- data.frame(output_rank_model_compiler_Ver_sim)
output_rank_model_compiler_rankIndex_sim <-  data.frame(output_rank_model_compiler_rankIndex_sim)

#MODIFIED MODEL
data.raw.compiler <- read.csv('Closure-Compiler Metrics Raw_Data.csv',header=T)
train_data <- data.raw.compiler
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+(train_data$MC/train_data$ST)+(train_data$CMC/train_data$ST)+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_compiler_Ver_sim_mod <- c()
output_rank_model_compiler_rankIndex_sim_mod <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_compiler_Ver_sim_mod[[idx]] <- i
        output_rank_model_compiler_rankIndex_sim_mod[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_compiler_Ver_sim_mod <- data.frame(output_rank_model_compiler_Ver_sim_mod)
output_rank_model_compiler_rankIndex_sim_mod <-  data.frame(output_rank_model_compiler_rankIndex_sim_mod)

final_output <- cbind(output_rank_model_compiler_Ver, output_rank_model_compiler_rankIndex, output_rank_model_compiler_rankIndex_sim, output_rank_model_compiler_rankIndex_sim_mod)
write.csv(final_output, file = "predict/similarity_predict_results_compiler.csv")


#Commons-Lang

#TRADITIONAL MODEL
data.raw.lang <- read.csv('Commons-Lang Metrics Raw_Data.csv',header=T)
train_data <- data.raw.lang
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_lang_Ver <- c()
output_rank_model_lang_rankIndex <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_lang_Ver[[idx]] <- i
        output_rank_model_lang_rankIndex[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_lang_Ver <- data.frame(output_rank_model_lang_Ver)
output_rank_model_lang_rankIndex <-  data.frame(output_rank_model_lang_rankIndex)

#SIMILARITY MODEL
data.raw.lang <- read.csv('Commons-Lang Metrics Raw_Data.csv',header=T)
train_data <- data.raw.lang
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_lang_Ver_sim <- c()
output_rank_model_lang_rankIndex_sim <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_lang_Ver_sim[[idx]] <- i
        output_rank_model_lang_rankIndex_sim[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_lang_Ver_sim <- data.frame(output_rank_model_lang_Ver_sim)
output_rank_model_lang_rankIndex_sim <-  data.frame(output_rank_model_lang_rankIndex_sim)

#MODIFIED MODEL
data.raw.lang <- read.csv('Commons-Lang Metrics Raw_Data.csv',header=T)
train_data <- data.raw.lang
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+(train_data$MC/train_data$ST)+(train_data$CMC/train_data$ST)+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_lang_Ver_sim_mod <- c()
output_rank_model_lang_rankIndex_sim_mod <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_lang_Ver_sim_mod[[idx]] <- i
        output_rank_model_lang_rankIndex_sim_mod[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_lang_Ver_sim_mod <- data.frame(output_rank_model_lang_Ver_sim_mod)
output_rank_model_lang_rankIndex_sim_mod <-  data.frame(output_rank_model_lang_rankIndex_sim_mod)

final_output <- cbind(output_rank_model_lang_Ver, output_rank_model_lang_rankIndex, output_rank_model_lang_rankIndex_sim, output_rank_model_lang_rankIndex_sim_mod)
write.csv(final_output, file = "predict/similarity_predict_results_lang.csv")


#Commons-Math

#TRADITIONAL MODEL
data.raw.math <- read.csv('Commons-Math Metrics Raw_Data.csv',header=T)
train_data <- data.raw.math
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_math_Ver <- c()
output_rank_model_math_rankIndex <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_math_Ver[[idx]] <- i
        output_rank_model_math_rankIndex[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_math_Ver <- data.frame(output_rank_model_math_Ver)
output_rank_model_math_rankIndex <-  data.frame(output_rank_model_math_rankIndex)


#SIMILARITY MODEL
data.raw.math <- read.csv('Commons-Math Metrics Raw_Data.csv',header=T)
train_data <- data.raw.math
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_math_Ver_sim <- c()
output_rank_model_math_rankIndex_sim <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_math_Ver_sim[[idx]] <- i
        output_rank_model_math_rankIndex_sim[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_math_Ver_sim <- data.frame(output_rank_model_math_Ver_sim)
output_rank_model_math_rankIndex_sim <-  data.frame(output_rank_model_math_rankIndex_sim)

#SIMILARITY MODEL
data.raw.math <- read.csv('Commons-Math Metrics Raw_Data.csv',header=T)
train_data <- data.raw.math
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+(train_data$MC/train_data$ST)+(train_data$CMC/train_data$ST)+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_math_Ver_sim_mod <- c()
output_rank_model_math_rankIndex_sim_mod <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_math_Ver_sim_mod[[idx]] <- i
        output_rank_model_math_rankIndex_sim_mod[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_math_Ver_sim_mod <- data.frame(output_rank_model_math_Ver_sim_mod)
output_rank_model_math_rankIndex_sim_mod <-  data.frame(output_rank_model_math_rankIndex_sim_mod)

final_output <- cbind(output_rank_model_math_Ver, output_rank_model_math_rankIndex, output_rank_model_math_rankIndex_sim, output_rank_model_math_rankIndex_sim_mod)
write.csv(final_output, file = "predict/similarity_predict_results_math.csv")


#JFreeChart

#TRADITIONAL MODEL
data.raw.jfreechart <- read.csv('JFreeChart Metrics Raw_Data.csv',header=T)
train_data <- data.raw.jfreechart
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_jfreechart_Ver <- c()
output_rank_model_jfreechart_rankIndex <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_jfreechart_Ver[[idx]] <- i
        output_rank_model_jfreechart_rankIndex[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_jfreechart_Ver <- data.frame(output_rank_model_jfreechart_Ver)
output_rank_model_jfreechart_rankIndex <-  data.frame(output_rank_model_jfreechart_rankIndex)

#SIMILARITY MODEL
data.raw.jfreechart <- read.csv('JFreeChart Metrics Raw_Data.csv',header=T)
train_data <- data.raw.jfreechart
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_jfreechart_Ver_sim <- c()
output_rank_model_jfreechart_rankIndex_sim <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_jfreechart_Ver_sim[[idx]] <- i
        output_rank_model_jfreechart_rankIndex_sim[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_jfreechart_Ver_sim <- data.frame(output_rank_model_jfreechart_Ver_sim)
output_rank_model_jfreechart_rankIndex_sim <-  data.frame(output_rank_model_jfreechart_rankIndex_sim)

#MODIFIED MODEL
data.raw.jfreechart <- read.csv('JFreeChart Metrics Raw_Data.csv',header=T)
train_data <- data.raw.jfreechart
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+(train_data$MC/train_data$ST)+(train_data$CMC/train_data$ST)+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_jfreechart_Ver_sim_mod <- c()
output_rank_model_jfreechart_rankIndex_sim_mod <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_jfreechart_Ver_sim_mod[[idx]] <- i
        output_rank_model_jfreechart_rankIndex_sim_mod[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_jfreechart_Ver_sim_mod <- data.frame(output_rank_model_jfreechart_Ver_sim_mod)
output_rank_model_jfreechart_rankIndex_sim_mod <-  data.frame(output_rank_model_jfreechart_rankIndex_sim_mod)

final_output <- cbind(output_rank_model_jfreechart_Ver, output_rank_model_jfreechart_rankIndex, output_rank_model_jfreechart_rankIndex_sim, output_rank_model_jfreechart_rankIndex_sim_mod)
write.csv(final_output, file = "predict/similarity_predict_results_jfreechart.csv")


#Joda-Time

#TRADITIONAL MODEL
data.raw.time <- read.csv('Joda-Time Metrics Raw_Data.csv',header=T)
train_data <- data.raw.time
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_time_Ver <- c()
output_rank_model_time_rankIndex <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_time_Ver[[idx]] <- i
        output_rank_model_time_rankIndex[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_time_Ver <- data.frame(output_rank_model_time_Ver)
output_rank_model_time_rankIndex <-  data.frame(output_rank_model_time_rankIndex)

#SIMILARITY MODEL
data.raw.time <- read.csv('Joda-Time Metrics Raw_Data.csv',header=T)
train_data <- data.raw.time
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_time_Ver_sim <- c()
output_rank_model_time_rankIndex_sim <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_time_Ver_sim[[idx]] <- i
        output_rank_model_time_rankIndex_sim[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_time_Ver_sim <- data.frame(output_rank_model_time_Ver_sim)
output_rank_model_time_rankIndex_sim <-  data.frame(output_rank_model_time_rankIndex_sim)

#MODIFIED MODEL
data.raw.time <- read.csv('Joda-Time Metrics Raw_Data.csv',header=T)
train_data <- data.raw.time
fit <- glm(train_data$Result~train_data$CMC+train_data$MC+train_data$ST+train_data$TM+(train_data$MC/train_data$ST)+(train_data$CMC/train_data$ST)+train_data$BC+train_data$HD+train_data$ED+train_data$IBC,data=train_data,family=binomial(link='logit'))
summary(fit)
#run the anova() function on the model to analyze the table of deviance
anova(fit, test="Chisq")
#no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(fit)
fitted.results <- data.frame(predict(fit,newdata=train_data,type='response'))
train_data <- predicted_merge_values <- cbind(train_data, fitted.results)
sort_train_data <- train_data %>% group_by(Version) %>% arrange(desc(predict.fit..newdata...train_data..type....response..))
output_rank_model_time_Ver_sim_mod <- c()
output_rank_model_time_rankIndex_sim_mod <- c()
idx <- 1
for (i in c(1:132)){
  if(i %in% sort_train_data$Version){
    split_train_data <- sort_train_data[sort_train_data$Version == i,]
    for(j in c(1:nrow(split_train_data))){
      if (split_train_data$Result[j] == 1) {
        output_rank_model_time_Ver_sim_mod[[idx]] <- i
        output_rank_model_time_rankIndex_sim_mod[[idx]] <- j/nrow(split_train_data)
        idx <- idx + 1
        break;
      }
    }
  }
}
output_rank_model_time_Ver_sim_mod <- data.frame(output_rank_model_time_Ver_sim_mod)
output_rank_model_time_rankIndex_sim_mod <-  data.frame(output_rank_model_time_rankIndex_sim_mod)

final_output <- cbind(output_rank_model_time_Ver, output_rank_model_time_rankIndex, output_rank_model_time_rankIndex_sim, output_rank_model_time_rankIndex_sim_mod)
write.csv(final_output, file = "predict/similarity_predict_results_time.csv")

