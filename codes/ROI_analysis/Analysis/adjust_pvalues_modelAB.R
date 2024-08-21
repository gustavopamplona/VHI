#############
# ROI analysis VHI study - syringe block - contrast varying with factors stimulation, visibility, and illusion

rm(list = ls())

pvals_total = c()

for (roi in 1:21) {
  
  # pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelA/1stLevel_movCor2_5s_2_6mm_run_illusion/"
  pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelB/1stLevel_modelB3/"
  
  tableFile = list.files(path = pathFolder)
  
  #library(readxl)
  data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
  tableFile[roi]
  
  df<-subset(data,!is.na(value))
  df = df[df$time == "secondHalf",]
  
  df$subj = as.factor(df$subj)
  df$run = as.factor(df$run)
  df$stim = as.factor(df$stim)
  df$vis = as.factor(df$vis)
  df$illusion = as.factor(df$illusion)
  
  #library(lme4)
  #library(lmerTest) # for the p-values
  m04 <-lmer(value ~ stim*vis*illusion + (1|subj), data=df)
  res = anova(m04)
  
  pvals_test = res$`Pr(>F)`
  
  pvals_total = cbind(pvals_total,pvals_test)
}

pvals_adjusted = c()

for (row in 1:7) {
  pvals_temp = p.adjust(pvals_total[row,], method = "fdr")
  pvals_adjusted = rbind(pvals_adjusted,pvals_temp)
}
