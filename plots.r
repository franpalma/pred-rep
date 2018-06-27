setwd('D:/pred-rep/tanzeem_noor-promise17_data')

#load data
rq1_closure_compiler <- read.csv("RQ1_Closure-Compiler.csv", header = TRUE)
rq1_commons_lang <- read.csv("RQ1_Commons-Lang.csv", header = TRUE)
rq1_commons_math <- read.csv("RQ1_Commons-Math.csv", header = TRUE)
rq1_jfreechart <- read.csv("RQ1_JFreeChart.csv", header = TRUE)
rq1_joda_time <- read.csv("RQ1_Joda-Time.csv", header = TRUE)

#Fig. 2

pdf(file='rq1_closure_compiler.pdf')
boxplot(rq1_closure_compiler$rank_CMC, rq1_closure_compiler$rank_ST, rq1_closure_compiler$rank_MC, rq1_closure_compiler$rank_Model,
        names = c("CMC", "ST", "MC", "Model"))
dev.off()

pdf(file='rq1_commons_lang.pdf')
boxplot(rq1_commons_lang$rank_TM, rq1_commons_lang$rank_CMC, rq1_commons_lang$rank_ST, rq1_commons_lang$rank_MC, rq1_commons_lang$rank_Model,
        names = c("TM", "CMC", "ST", "MC", "Model"))
dev.off()

pdf(file='rq1_commons_math.pdf')
boxplot(rq1_commons_math$rank_TM, rq1_commons_math$rank_CMC, rq1_commons_math$rank_ST, rq1_commons_math$rank_MC, rq1_commons_math$rank_Model,
        names = c("TM", "CMC", "ST", "MC", "Model"))
dev.off()

pdf(file='rq1_jfreechart.pdf')
boxplot(rq1_jfreechart$rank_CMC, rq1_jfreechart$rank_ST, rq1_jfreechart$rank_MC, rq1_jfreechart$rank_Model,
        names = c("CMC", "ST", "MC", "Model"))
dev.off()

pdf(file='rq1_joda_time.pdf')
boxplot(rq1_joda_time$rank_CMC, rq1_joda_time$rank_ST, rq1_joda_time$rank_MC, rq1_joda_time$rank_Model,
        names = c("CMC", "ST", "MC", "Model"))
dev.off()

#FIg. 3

library(effsize)

rq1_treatment_model_jfreechart = rq1_jfreechart$rank_Model
rq1_treatment_model_joda_time = rq1_joda_time$rank_Model
rq1_treatment_model_commons_lang = rq1_commons_lang$rank_Model
rq1_treatment_model_commons_math = rq1_commons_math$rank_Model
rq1_treatment_model_closure_compiler = rq1_closure_compiler$rank_Model

rq1_control_cmc_jfreechart = rq1_jfreechart$rank_CMC
rq1_control_cmc_joda_time = rq1_joda_time$rank_CMC
rq1_control_cmc_commons_lang = rq1_commons_lang$rank_CMC
rq1_control_cmc_commons_math = rq1_commons_math$rank_CMC
rq1_control_cmc_closure_compiler = rq1_closure_compiler$rank_CMC

rq1_control_st_jfreechart = rq1_jfreechart$rank_ST
rq1_control_st_joda_time = rq1_joda_time$rank_ST
rq1_control_st_commons_lang = rq1_commons_lang$rank_ST
rq1_control_st_commons_math = rq1_commons_math$rank_ST
rq1_control_st_closure_compiler = rq1_closure_compiler$rank_ST

rq1_control_mc_jfreechart = rq1_jfreechart$rank_MC
rq1_control_mc_joda_time = rq1_joda_time$rank_MC
rq1_control_mc_commons_lang = rq1_commons_lang$rank_MC
rq1_control_mc_commons_math = rq1_commons_math$rank_MC
rq1_control_mc_closure_compiler = rq1_closure_compiler$rank_MC

rq1_control_tm_commons_lang = rq1_commons_lang$rank_TM
rq1_control_tm_commons_math = rq1_commons_math$rank_TM

CMC1 <- NULL
for(i in c(1:length(rq1_treatment_model_closure_compiler)))
  CMC1[i] <- (VD.A(rq1_control_cmc_closure_compiler[i], rq1_treatment_model_closure_compiler[i])$estimate)

CMC2 <- NULL
for(i in c(1:length(rq1_treatment_model_commons_math)))
  CMC2[i] <- (VD.A(rq1_control_cmc_commons_math[i], rq1_treatment_model_commons_math[i])$estimate)

CMC3 <- NULL
for(i in c(1:length(rq1_treatment_model_commons_lang)))
  CMC3[i] <- (VD.A(rq1_control_cmc_commons_lang[i], rq1_treatment_model_commons_lang[i])$estimate)

CMC4 <- NULL
for(i in c(1:length(rq1_treatment_model_joda_time)))
  CMC4[i] <- (VD.A(rq1_control_cmc_joda_time[i], rq1_treatment_model_joda_time[i])$estimate)

CMC5 <- NULL
for(i in c(1:length(rq1_treatment_model_jfreechart)))
  CMC5[i] <- (VD.A(rq1_control_cmc_jfreechart[i], rq1_treatment_model_jfreechart[i])$estimate)

pdf(file='rq1_Model_CMC.pdf')
boxplot(CMC5, CMC4, CMC3, CMC2, CMC1, names = c("JFreeChart", "Joda Time", "Commons Lang", "Commons Math", "Closure Compile"), cex.lab=.65, cex.axis=.65)
dev.off()

ST1 <- NULL
for(i in c(1:length(rq1_treatment_model_closure_compiler)))
  ST1[i] <- (VD.A(rq1_control_st_closure_compiler[i], rq1_treatment_model_closure_compiler[i])$estimate)

ST2 <- NULL
for(i in c(1:length(rq1_treatment_model_commons_math)))
  ST2[i] <- (VD.A(rq1_control_st_commons_math[i], rq1_treatment_model_commons_math[i])$estimate)

ST3 <- NULL
for(i in c(1:length(rq1_treatment_model_commons_lang)))
  ST3[i] <- (VD.A(rq1_control_st_commons_lang[i], rq1_treatment_model_commons_lang[i])$estimate)

ST4 <- NULL
for(i in c(1:length(rq1_treatment_model_joda_time)))
  ST4[i] <- (VD.A(rq1_control_st_joda_time[i], rq1_treatment_model_joda_time[i])$estimate)

ST5 <- NULL
for(i in c(1:length(rq1_treatment_model_jfreechart)))
  ST5[i] <- (VD.A(rq1_control_st_jfreechart[i], rq1_treatment_model_jfreechart[i])$estimate)

pdf(file='rq1_Model_ST.pdf')
boxplot(ST5, ST4, ST3, ST2, ST1, names = c("JFreeChart", "JodaTime", "CommonsLang", "CommonsMath", "ClosureCompile"), cex.lab=.65, cex.axis=.65)
dev.off()

MC1 <- NULL
for(i in c(1:length(rq1_treatment_model_closure_compiler)))
  MC1[i] <- (VD.A(rq1_control_mc_closure_compiler[i], rq1_treatment_model_closure_compiler[i])$estimate)

MC2 <- NULL
for(i in c(1:length(rq1_treatment_model_commons_math)))
  MC2[i] <- (VD.A(rq1_control_mc_commons_math[i], rq1_treatment_model_commons_math[i])$estimate)

MC3 <- NULL
for(i in c(1:length(rq1_treatment_model_commons_lang)))
  MC3[i] <- (VD.A(rq1_control_mc_commons_lang[i], rq1_treatment_model_commons_lang[i])$estimate)

MC4 <- NULL
for(i in c(1:length(rq1_treatment_model_joda_time)))
  MC4[i] <- (VD.A(rq1_control_mc_joda_time[i], rq1_treatment_model_joda_time[i])$estimate)

MC5 <- NULL
for(i in c(1:length(rq1_treatment_model_jfreechart)))
  MC5[i] <- (VD.A(rq1_control_mc_jfreechart[i], rq1_treatment_model_jfreechart[i])$estimate)

pdf(file='rq1_Model_MC.pdf')
boxplot(MC5, MC4, MC3, MC2, MC1, names = c("JFreeChart", "JodaTime", "CommonsLang", "CommonsMath", "ClosureCompile"), cex.lab=.65, cex.axis=.65)
dev.off()


TM2 <- NULL
for(i in c(1:length(rq1_treatment_model_commons_math)))
  TM2[i] <- (VD.A(rq1_control_tm_commons_math[i], rq1_treatment_model_commons_math[i])$estimate)

TM3 <- NULL
for(i in c(1:length(rq1_treatment_model_commons_lang)))
  TM3[i] <- (VD.A(rq1_control_tm_commons_lang[i], rq1_treatment_model_commons_lang[i])$estimate)


pdf(file='rq1_Model_TM.pdf')
boxplot(TM3, TM2, names = c("CommonsLang", "CommonsMath"), cex.lab=.65, cex.axis=.65)
dev.off()

#load data
rq2_closure_compiler <- read.csv("RQ2_Closure-Compiler.csv", header = TRUE)
rq2_commons_lang <- read.csv("RQ2_Commons-Lang.csv", header = TRUE)
rq2_commons_math <- read.csv("RQ2_Commons-Math.csv", header = TRUE)
rq2_jfreechart <- read.csv("RQ2_JFreeChart.csv", header = TRUE)
rq2_joda_time <- read.csv("RQ2_Joda-Time.csv", header = TRUE)


#Fig. 4

pdf(file='rq2_closure_compiler.pdf')
boxplot(rq2_closure_compiler$rank_Traditional_Model, rq2_closure_compiler$rank_Proposed_Model,
        names = c("Traditional Model", "Proposed Model"))
dev.off()

pdf(file='rq2_commons_lang.pdf')
boxplot(rq2_commons_lang$rank_Traditional_Model, rq2_commons_lang$rank_Proposed_Model,
        names = c("Traditional Model", "Proposed Model"))
dev.off()

pdf(file='rq2_commons_math.pdf')
boxplot(rq2_commons_math$rank_Traditional_Model, rq2_commons_math$rank_Proposed_Model,
        names = c("Traditional Model", "Proposed Model"))
dev.off()

pdf(file='rq2_jfreechart.pdf')
boxplot(rq2_jfreechart$rank_Traditional_Model, rq2_jfreechart$rank_Proposed_Model,
        names = c("Traditional Model", "Proposed Model"))
dev.off()

pdf(file='rq2_joda_time.pdf')
boxplot(rq2_joda_time$rank_Traditional_Model, rq2_joda_time$rank_Proposed_Model,
        names = c("Traditional Model", "Proposed Model"))
dev.off()

#Fig. 5

rq2_treatment_model_jfreechart = rq2_jfreechart$rank_Proposed_Model
rq2_treatment_model_joda_time = rq2_joda_time$rank_Proposed_Model
rq2_treatment_model_commons_lang = rq2_commons_lang$rank_Proposed_Model
rq2_treatment_model_commons_math = rq2_commons_math$rank_Proposed_Model
rq2_treatment_model_closure_compiler = rq2_closure_compiler$rank_Proposed_Model

rq2_control_model_jfreechart = rq2_jfreechart$rank_Traditional_Model
rq2_control_model_joda_time = rq2_joda_time$rank_Traditional_Model
rq2_control_model_commons_lang = rq2_commons_lang$rank_Traditional_Model
rq2_control_model_commons_math = rq2_commons_math$rank_Traditional_Model
rq2_control_model_closure_compiler = rq2_closure_compiler$rank_Traditional_Model

PM_TM_1 <- NULL
for(i in c(1:length(rq2_treatment_model_closure_compiler)))
  PM_TM_1[i] <- (VD.A(rq2_control_model_closure_compiler[i], rq2_treatment_model_closure_compiler[i])$estimate)

PM_TM_2 <- NULL
for(i in c(1:length(rq2_treatment_model_commons_math)))
  PM_TM_2[i] <- (VD.A(rq2_control_model_commons_math[i], rq2_treatment_model_commons_math[i])$estimate)

PM_TM_3 <- NULL
for(i in c(1:length(rq2_treatment_model_commons_lang)))
  PM_TM_3[i] <- (VD.A(rq2_control_model_commons_lang[i], rq2_treatment_model_commons_lang[i])$estimate)

PM_TM_4 <- NULL
for(i in c(1:length(rq2_treatment_model_joda_time)))
  PM_TM_4[i] <- (VD.A(rq2_control_model_joda_time[i], rq2_treatment_model_joda_time[i])$estimate)

PM_TM_5 <- NULL
for(i in c(1:length(rq2_treatment_model_jfreechart)))
  PM_TM_5[i] <- (VD.A(rq2_control_model_jfreechart[i], rq2_treatment_model_jfreechart[i])$estimate)

pdf(file='rq2_Model_Traditional.pdf')
boxplot(PM_TM_5, PM_TM_4, PM_TM_3, PM_TM_2, PM_TM_1, names = c("JFreeChart", "Joda Time", "Commons Lang", "Commons Math", "Closure Compile"), cex.lab=.65, cex.axis=.65)
dev.off()
