#############
### ROI-GSR association

### ModelA

rm(list = ls())

pvals_total = c()

#library(readxl)
data_gsr = read_excel("D:/VHI/Analysis/GSR/Tables/table_GSRbetas_VHI_filter_5e-3_5e0.xlsx")

for (roi in 1:21) {
  
  # roi = 20
  
  pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelA/1stLevel_movCor2_5s_2_6mm_run_illusion/"
  tableFile = list.files(path = pathFolder)
  
  #library(readxl)
  data_brain <-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
  print(tableFile[roi])
  
  data_gsr<-subset(data_gsr,!is.na(subj))
  
  data=cbind(data_gsr[,1:6],data_brain[,5:6])
  
  df<-subset(data,!is.na(value))
  df<-subset(df,!is.na(gsr_beta))
  
  df$subj = as.factor(df$subj)
  df$illusion = as.factor(df$illusion)
  
  #library(lme4)
  #library(lmerTest) # for the p-values
  
  m01 <-lmer(value ~ gsr_beta*illusion + (1|subj), data=df)
  res01 = anova(m01)
  print(res01)
  
  pvals_test = res01$`Pr(>F)`
  
  pvals_total = cbind(pvals_total,pvals_test)
}

pvals_adjusted = c()

for (row in 1:3) {
  pvals_temp = p.adjust(pvals_total[row,], method = "fdr")
  pvals_adjusted = rbind(pvals_adjusted,pvals_temp)
}

# install.packages("effects")
# library(effects)
plot(effect("gsr_beta*illusion",m01))



#############
### ModelB

rm(list = ls())

pvals_total = c()

#library(readxl)
data_gsr = read_excel("D:/VHI/Analysis/GSR/Tables/table_GSRbetas_VHI_filter_5e-3_5e0.xlsx")

for (roi in 1:21) {
  
  # roi = 13
  
  pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelB/1stLevel_modelB3/"
  tableFile = list.files(path = pathFolder)
  
  #library(readxl)
  data_brain <-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
  print(tableFile[roi])
  
  data_brain = data_brain[data_brain$time == "secondHalf",]
  
  data_gsr<-subset(data_gsr,!is.na(subj))
  
  data=cbind(data_gsr[,1:6],data_brain[,6:7])
  
  df<-subset(data,!is.na(value))
  df<-subset(df,!is.na(gsr_beta))
  
  df$subj = as.factor(df$subj)
  df$illusion = as.factor(df$illusion)
  
  #library(lme4)
  #library(lmerTest) # for the p-values
  
  m01 <-lmer(value ~ gsr_beta*illusion + (1|subj), data=df)
  res01 = anova(m01)
  print(res01)
  
  pvals_test = res01$`Pr(>F)`
  
  pvals_total = cbind(pvals_total,pvals_test)
}

pvals_adjusted = c()

for (row in 1:3) {
  pvals_temp = p.adjust(pvals_total[row,], method = "fdr")
  pvals_adjusted = rbind(pvals_adjusted,pvals_temp)
}

# install.packages("effects")
# library(effects)
plot(effect("gsr_beta*illusion",m01))



#############
### ModelC

rm(list = ls())

pvals_total = c()

#library(readxl)
data_gsr = read_excel("D:/VHI/Analysis/GSR/Tables/table_GSRbetas_VHI_filter_5e-3_5e0.xlsx")

for (roi in 1:21) {
  
  # roi = 1
  
  pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelC/1stLevel_modelC3/"
  tableFile = list.files(path = pathFolder)
  
  #library(readxl)
  data_brain <-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
  print(tableFile[roi])
  
  data_brain = data_brain[data_brain$time == "After",]
  
  data_gsr<-subset(data_gsr,!is.na(subj))
  
  data=cbind(data_gsr[,1:6],data_brain[,6])
  
  df<-subset(data,!is.na(value))
  df<-subset(df,!is.na(gsr_beta))
  
  df$subj = as.factor(df$subj)
  
  #library(lme4)
  #library(lmerTest) # for the p-values
  
  m01 <-lmer(value ~ gsr_beta + (1|subj), data=df)
  res01 = anova(m01)
  print(res01)
  
  pvals_test = res01$`Pr(>F)`
  
  pvals_total = cbind(pvals_total,pvals_test)
}

pvals_adjusted = c()

for (row in 1:1) {
  pvals_temp = p.adjust(pvals_total[row,], method = "fdr")
  pvals_adjusted = rbind(pvals_adjusted,pvals_temp)
}

# install.packages("effects")
# library(effects)
plot(effect("gsr_beta*illusion",m01))
