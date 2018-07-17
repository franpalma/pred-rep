setwd('D:/pred-rep/tanzeem_noor-promise17_data/predict')

traditional_predict_results_compiler <- read.csv('traditional_predict_results_compiler.csv',header=T)
traditional_predict_results_jfreechart <- read.csv('traditional_predict_results_jfreechart.csv',header=T)
traditional_predict_results_lang <- read.csv('traditional_predict_results_lang.csv',header=T)
traditional_predict_results_math <- read.csv('traditional_predict_results_math.csv',header=T)
traditional_predict_results_time <- read.csv('traditional_predict_results_time.csv',header=T)
similarity_predict_results_compiler <- read.csv('similarity_predict_results_compiler.csv',header=T)
similarity_predict_results_jfreechart <- read.csv('similarity_predict_results_jfreechart.csv',header=T)
similarity_predict_results_lang <- read.csv('similarity_predict_results_lang.csv',header=T)
similarity_predict_results_math <- read.csv('similarity_predict_results_math.csv',header=T)
similarity_predict_results_time <- read.csv('similarity_predict_results_time.csv',header=T)

#PLOTS TRADITIONAL

#compiler
dt1 <- traditional_predict_results_compiler$output_rank_ST_compiler_rankIndex
dt2 <- traditional_predict_results_compiler$output_rank_MC_compiler_rankIndex
dt3 <- traditional_predict_results_compiler$output_rank_CMC_compiler_rankIndex
dt4 <- traditional_predict_results_compiler$output_rank_TM_compiler_rankIndex
dt5 <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex
dt6 <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex_mod

pdf("traditional_compiler.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, names=c("ST","MC","CMC","TM","Traditional","Traditional2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

#jfreechart
dt1 <- traditional_predict_results_jfreechart$output_rank_ST_jfreechart_rankIndex
dt2 <- traditional_predict_results_jfreechart$output_rank_MC_jfreechart_rankIndex
dt3 <- traditional_predict_results_jfreechart$output_rank_CMC_jfreechart_rankIndex
dt4 <- traditional_predict_results_jfreechart$output_rank_TM_jfreechart_rankIndex
dt5 <- traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex
dt6 <- traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_mod

pdf("traditional_jfreechart.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, names=c("ST","MC","CMC","TM","Traditional","Traditional2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

#lang
dt1 <- traditional_predict_results_lang$output_rank_ST_lang_rankIndex
dt2 <- traditional_predict_results_lang$output_rank_MC_lang_rankIndex
dt3 <- traditional_predict_results_lang$output_rank_CMC_lang_rankIndex
dt4 <- traditional_predict_results_lang$output_rank_TM_lang_rankIndex
dt5 <- traditional_predict_results_lang$output_rank_model_lang_rankIndex
dt6 <- traditional_predict_results_lang$output_rank_model_lang_rankIndex_mod

pdf("traditional_lang.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, names=c("ST","MC","CMC","TM","Traditional","Traditional2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

#math
dt1 <- traditional_predict_results_math$output_rank_ST_math_rankIndex
dt2 <- traditional_predict_results_math$output_rank_MC_math_rankIndex
dt3 <- traditional_predict_results_math$output_rank_CMC_math_rankIndex
dt4 <- traditional_predict_results_math$output_rank_TM_math_rankIndex
dt5 <- traditional_predict_results_math$output_rank_model_math_rankIndex
dt6 <- traditional_predict_results_math$output_rank_model_math_rankIndex_mod

pdf("traditional_math.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, names=c("ST","MC","CMC","TM","Traditional","Traditional2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

#time
dt1 <- traditional_predict_results_time$output_rank_ST_time_rankIndex
dt2 <- traditional_predict_results_time$output_rank_MC_time_rankIndex
dt3 <- traditional_predict_results_time$output_rank_CMC_time_rankIndex
dt4 <- traditional_predict_results_time$output_rank_TM_time_rankIndex
dt5 <- traditional_predict_results_time$output_rank_model_time_rankIndex
dt6 <- traditional_predict_results_time$output_rank_model_time_rankIndex_mod

pdf("traditional_time.pdf")
boxplot(dt1, dt2, dt3, dt4, dt5, dt6, names=c("ST","MC","CMC","TM","Traditional","Traditional2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)



#PLOTS SIMILAR

#compiler
dt1 <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex
dt2 <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim
dt3 <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim_mod
pdf("similarity_compiler.pdf")
boxplot(dt1, dt2, dt3, names=c("Traditional","Similarity","Similarity2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

#jfreechart
dt1 <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex
dt2 <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim
dt3 <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim_mod
pdf("similarity_jfreechart.pdf")
boxplot(dt1, dt2, dt3, names=c("Traditional","Similarity","Similarity2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

#lang
dt1 <- similarity_predict_results_lang$output_rank_model_lang_rankIndex
dt2 <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim
dt3 <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim_mod
pdf("similarity_lang.pdf")
boxplot(dt1, dt2, dt3, names=c("Traditional","Similarity","Similarity2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

#math
dt1 <- similarity_predict_results_math$output_rank_model_math_rankIndex
dt2 <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim
dt3 <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim_mod
pdf("similarity_math.pdf")
boxplot(dt1, dt2, dt3, names=c("Traditional","Similarity","Similarity2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

#time
dt1 <- similarity_predict_results_time$output_rank_model_time_rankIndex
dt2 <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim
dt3 <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim_mod
pdf("similarity_time.pdf")
boxplot(dt1, dt2, dt3, names=c("Traditional","Similarity","Similarity2"), ylab="% TCs to be executed to find the first failing TC", col="white", cex.lab=.9, cex.axis=.9, las=2)

library(effsize)

#compare traditional model with 4 metrics vs similar model with 8 metrics

a <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex_mod
b <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- traditional_predict_results_lang$output_rank_model_lang_rankIndex_mod
b <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- traditional_predict_results_math$output_rank_model_math_rankIndex_mod
b <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_mod
b <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- traditional_predict_results_time$output_rank_model_time_rankIndex_mod
b <- similarity_predict_results_time$output_rank_model_time_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)


#compare traditional model with 8 metrics vs modified traditional model with 10 metrics

a <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex_mod
b <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- traditional_predict_results_lang$output_rank_model_lang_rankIndex_mod
b <- traditional_predict_results_lang$output_rank_model_lang_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- traditional_predict_results_math$output_rank_model_math_rankIndex_mod
b <- traditional_predict_results_math$output_rank_model_math_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_mod
b <- traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- traditional_predict_results_time$output_rank_model_time_rankIndex_mod
b <- traditional_predict_results_time$output_rank_model_time_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

#compare similarity model with 8 metrics vs modified similarity model with 10 metrics

a <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim_mod
b <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim_mod
b <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim_mod
b <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim_mod
b <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim_mod
b <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)


#compare similarity model with 10 metrics vs replicated traditional with 4 metrics

a <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim_mod
b <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim_mod
b <- traditional_predict_results_lang$output_rank_model_lang_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim_mod
b <- traditional_predict_results_math$output_rank_model_math_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim_mod
b <- traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)

a <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim_mod
b <- traditional_predict_results_time$output_rank_model_time_rankIndex
wilcox.test(a, b, conf.level = 0.95)
VD.A(a ~ b)



kruskal.test(list(similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim_mod, similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim, similarity_predict_results_compiler$output_rank_model_compiler_rankIndex))

kruskal.test(list(similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim_mod, similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim, similarity_predict_results_lang$output_rank_model_lang_rankIndex))

kruskal.test(list(similarity_predict_results_math$output_rank_model_math_rankIndex_sim_mod, similarity_predict_results_math$output_rank_model_math_rankIndex_sim, similarity_predict_results_math$output_rank_model_math_rankIndex))

kruskal.test(list(similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim_mod, similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim, similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex))

kruskal.test(list(similarity_predict_results_time$output_rank_model_time_rankIndex_sim_mod, similarity_predict_results_time$output_rank_model_time_rankIndex_sim, similarity_predict_results_time$output_rank_model_time_rankIndex))


library(effsize)


a <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex_mod
b <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex
VD.A(a ~ b, conf.level = 0.95)



a <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim_mod
b <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim_mod
b <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim_mod
b <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim_mod
b <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim_mod
b <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim
VD.A(a ~ b, conf.level = 0.95)



a <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim_mod
b <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim_mod
b <- traditional_predict_results_lang$output_rank_model_lang_rankIndex
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim_mod
b <- traditional_predict_results_math$output_rank_model_math_rankIndex
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim_mod
b <- traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim_mod
b <- traditional_predict_results_time$output_rank_model_time_rankIndex
VD.A(a ~ b, conf.level = 0.95)



a <- similarity_predict_results_compiler$output_rank_model_compiler_rankIndex_sim
b <- traditional_predict_results_compiler$output_rank_model_compiler_rankIndex
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_lang$output_rank_model_lang_rankIndex_sim
b <- traditional_predict_results_lang$output_rank_model_lang_rankIndex
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_math$output_rank_model_math_rankIndex_sim
b <- traditional_predict_results_math$output_rank_model_math_rankIndex
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex_sim
b <- traditional_predict_results_jfreechart$output_rank_model_jfreechart_rankIndex
VD.A(a ~ b, conf.level = 0.95)

a <- similarity_predict_results_time$output_rank_model_time_rankIndex_sim
b <- traditional_predict_results_time$output_rank_model_time_rankIndex
VD.A(a ~ b, conf.level = 0.95)

