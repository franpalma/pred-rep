setwd('D:/pred-rep/tanzeem_noor-promise17_data/predict')

my_traditional_predict_results_compiler <- read.csv('traditional_predict_results_compiler.csv',header=T)
my_traditional_predict_results_jfreechart <- read.csv('traditional_predict_results_jfreechart.csv',header=T)
my_traditional_predict_results_lang <- read.csv('traditional_predict_results_lang.csv',header=T)
my_traditional_predict_results_math <- read.csv('traditional_predict_results_math.csv',header=T)
my_traditional_predict_results_time <- read.csv('traditional_predict_results_time.csv',header=T)
my_similarity_predict_results_compiler <- read.csv('similarity_predict_results_compiler.csv',header=T)
my_similarity_predict_results_jfreechart <- read.csv('similarity_predict_results_jfreechart.csv',header=T)
my_similarity_predict_results_lang <- read.csv('similarity_predict_results_lang.csv',header=T)
my_similarity_predict_results_math <- read.csv('similarity_predict_results_math.csv',header=T)
my_similarity_predict_results_time <- read.csv('similarity_predict_results_time.csv',header=T)

their_traditional_predict_results_compiler <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ1_Closure-Compiler.csv',header=T)
their_traditional_predict_results_jfreechart <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ1_Commons-Lang.csv',header=T)
their_traditional_predict_results_lang <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ1_Commons-Math.csv',header=T)
their_traditional_predict_results_math <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ1_JFreeChart.csv',header=T)
their_traditional_predict_results_time <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ1_Joda-Time.csv',header=T)
their_similarity_predict_results_compiler <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ2_Closure-Compiler.csv',header=T)
their_similarity_predict_results_jfreechart <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ2_Commons-Lang.csv',header=T)
their_similarity_predict_results_lang <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ2_Commons-Math.csv',header=T)
their_similarity_predict_results_math <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ2_JFreeChart.csv',header=T)
their_similarity_predict_results_time <- read.csv('D:/pred-rep/tanzeem_noor-promise17_data/RQ2_Joda-Time.csv',header=T)


#compiler
dt1 <- their_traditional_predict_results_compiler$rank_Model
dt2 <- my_traditional_predict_results_compiler$output_rank_model_compiler_rankIndex
dt3 <- my_traditional_predict_results_compiler$output_rank_model_compiler_rankIndex_mod
dt4 <- their_similarity_predict_results_compiler$rank_Proposed_Model
dt5 <- my_similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim
dt6 <- my_similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim_mod
#pdf("comparison_compiler.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, 
        names=c("Origin_Traditional","Rep_Traditional","Traditional2","Origin_Similarity","Rep_Similarity","Similarity2"), 
        ylab="% TCs to be executed for finding the first failing TC", 
        col="white", cex.lab=.95, cex.axis=.95, srt = 45)
dev.flush()


#jfreechart
dt1 <- their_traditional_predict_results_jfreechart$rank_Model
dt2 <- my_traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex
dt3 <- my_traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_mod
dt4 <- their_similarity_predict_results_jfreechart$rank_Proposed_Model
dt5 <- my_similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim
dt6 <- my_similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim_mod
#pdf("comparison_jfreechart.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, 
        names=c("Origin_Traditional","Rep_Traditional","Traditional2","Origin_Similarity","Rep_Similarity","Similarity2"), 
        ylab="% TCs to be executed for finding the first failing TC", 
        col="white", cex.lab=.95, cex.axis=.95, srt = 45)
dev.flush()


#lang
dt1 <- their_traditional_predict_results_lang$rank_Model
dt2 <- my_traditional_predict_results_lang$output_rank_model_lang_rankIndex
dt3 <- my_traditional_predict_results_lang$output_rank_model_lang_rankIndex_mod
dt4 <- their_similarity_predict_results_lang$rank_Proposed_Model
dt5 <- my_similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim
dt6 <- my_similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim_mod
#pdf("comparison_lang.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, 
        names=c("Origin_Traditional","Rep_Traditional","Traditional2","Origin_Similarity","Rep_Similarity","Similarity2"), 
        ylab="% TCs to be executed for finding the first failing TC", 
        col="white", cex.lab=.95, cex.axis=.95, srt = 45)
dev.flush()



#math
dt1 <- their_traditional_predict_results_math$rank_Model
dt2 <- my_traditional_predict_results_math$output_rank_model_math_rankIndex
dt3 <- my_traditional_predict_results_math$output_rank_model_math_rankIndex_mod
dt4 <- their_similarity_predict_results_math$rank_Proposed_Model
dt5 <- my_similarity_predict_results_math$output_rank_model_math_rankIndex_sim
dt6 <- my_similarity_predict_results_math$output_rank_model_math_rankIndex_sim_mod
#pdf("comparison_math.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, 
        names=c("Origin_Traditional","Rep_Traditional","Traditional2","Origin_Similarity","Rep_Similarity","Similarity2"), 
        ylab="% TCs to be executed for finding the first failing TC", 
        col="white", cex.lab=.95, cex.axis=.95, srt = 45)
dev.flush()



#time
dt1 <- their_traditional_predict_results_time$rank_Model
dt2 <- my_traditional_predict_results_time$output_rank_model_time_rankIndex
dt3 <- my_traditional_predict_results_time$output_rank_model_time_rankIndex_mod
dt4 <- their_similarity_predict_results_time$rank_Proposed_Model
dt5 <- my_similarity_predict_results_time$output_rank_model_time_rankIndex_sim
dt6 <- my_similarity_predict_results_time$output_rank_model_time_rankIndex_sim_mod
#pdf("comparison_time.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, 
        names=c("Origin_Traditional","Rep_Traditional","Traditional2","Origin_Similarity","Rep_Similarity","Similarity2"), 
        ylab="% TCs to be executed for finding the first failing TC", 
        col="white", cex.lab=.95, cex.axis=.95, srt = 45)
dev.flush()

library(effsize)

a <- my_similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim_mod
b <- their_traditional_predict_results_compiler$rank_Model
wilcox.test(a, b, conf.level = 0.95, paired = TRUE, exact = FALSE)
