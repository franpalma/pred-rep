setwd('D:/pred-rep/tanzeem_noor-promise17_data')

closure_compiler_raw <- read.csv('Closure-Compiler Metrics Raw_Data.csv',header=T)
closure_compiler_raw$Project <- "ClosureCompiler"

commons_lang_raw <- read.csv('Commons-Lang Metrics Raw_Data.csv',header=T)
commons_lang_raw$Project <- "CommonsLang"

commons_math_raw <- read.csv('Commons-Math Metrics Raw_Data.csv',header=T)
commons_math_raw$Project <- "CommonsMath"

jfreechart_raw <- read.csv('JFreeChart Metrics Raw_Data.csv',header=T)
jfreechart_raw$Project <- "JFreeChart"

joda_time_raw <- read.csv('Joda-Time Metrics Raw_Data.csv',header=T)
joda_time_raw$Project <- "JodaTime"

combined_raw <- rbind(closure_compiler_raw, commons_lang_raw, commons_math_raw, jfreechart_raw, joda_time_raw)

combined_raw_compiler <- data.frame(combined_raw[combined_raw$Project == 'ClosureCompiler',])

library(dplyr)

sort_st_combined_raw_compiler <- combined_raw_compiler %>% group_by(Version) %>% arrange(desc(ST))
version_st_combined_raw_compiler <- split(sort_st_combined_raw_compiler, sort_st_combined_raw_compiler$Version, drop=FALSE)

sort_st_combined_raw_compiler <- combined_raw_compiler %>% group_by(Version) %>% arrange(desc(MC))
version_st_combined_raw_compiler <- split(sort_st_combined_raw_compiler, sort_st_combined_raw_compiler$Version, drop=FALSE)

sort_st_combined_raw_compiler <- combined_raw_compiler %>% group_by(Version) %>% arrange(desc(CMC))
version_st_combined_raw_compiler <- split(sort_st_combined_raw_compiler, sort_st_combined_raw_compiler$Version, drop=FALSE)

sort_st_combined_raw_compiler <- combined_raw_compiler %>% group_by(Version) %>% arrange(desc(TM))
version_st_combined_raw_compiler <- split(sort_st_combined_raw_compiler, sort_st_combined_raw_compiler$Version, drop=FALSE)

sort_st_combined_raw_compiler <- combined_raw_compiler %>% group_by(Version) %>% arrange(desc(BC))
version_st_combined_raw_compiler <- split(sort_st_combined_raw_compiler, sort_st_combined_raw_compiler$Version, drop=FALSE)

sort_st_combined_raw_compiler <- combined_raw_compiler %>% group_by(Version) %>% arrange(HD)
version_st_combined_raw_compiler <- split(sort_st_combined_raw_compiler, sort_st_combined_raw_compiler$Version, drop=FALSE)
?arrange
sort_st_combined_raw_compiler <- combined_raw_compiler %>% group_by(Version) %>% arrange(ED)
version_st_combined_raw_compiler <- split(sort_st_combined_raw_compiler, sort_st_combined_raw_compiler$Version, drop=FALSE)


combined_raw_lang <- data.frame(combined_raw[combined_raw$Project == 'CommonsLang',])
combined_raw_lang_split_ver <- split(combined_raw_compiler, combined_raw_compiler$Version)

combined_raw_math <- data.frame(combined_raw[combined_raw$Project == 'CommonsMath',])
combined_raw_math_split_ver <- split(combined_raw_compiler, combined_raw_compiler$Version)

combined_raw_jfreechart <- data.frame(combined_raw[combined_raw$Project == 'JFreeChart',])
combined_raw_jfreechart_split_ver <- split(combined_raw_compiler, combined_raw_compiler$Version)

combined_raw_time <- data.frame(combined_raw[combined_raw$Project == 'JodaTime',])
combined_raw_time_split_ver <- split(combined_raw_compiler, combined_raw_compiler$Version)

