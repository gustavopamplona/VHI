#############
### ROI-Q2 association

### ModelA

rm(list = ls())

pvals_total = c()

#library(readxl)
data_quest <- read_excel("D:/VHI/Analysis/SubjectiveMeasures/Results/table_subjectiveMeasures_VHI.xlsx")

for (roi in 1:21) {
  
  # roi = 20
  
  pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelA/1stLevel_movCor2_5s_2_6mm_run_illusion/"
  tableFile = list.files(path = pathFolder)
  
  #library(readxl)
  data_brain <-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
  print(tableFile[roi])
  
  data_quest<-subset(data_quest,!is.na(subj))
  
  data=cbind(data_quest[,1:8],data_brain[,5:6])
  
  df<-subset(data,!is.na(value))
  df<-subset(df,!is.na(q2))
  
  df$subj = as.factor(df$subj)
  df$illusion = as.factor(df$illusion)
  
  #library(lme4)
  #library(lmerTest) # for the p-values
  
  m01 <-lmer(value ~ q2*illusion + (1|subj), data=df)
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
plot(effect("q2*illusion",m01))



#############
### ModelB

rm(list = ls())

# pvals_total = c()

#library(readxl)
data_quest <- read_excel("D:/VHI/Analysis/SubjectiveMeasures/Results/table_subjectiveMeasures_VHI.xlsx")

# for (roi in 1:21) {
  
  roi = 11
  
  pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelB/1stLevel_modelB3/"
  tableFile = list.files(path = pathFolder)
  
  #library(readxl)
  data_brain <-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
  print(tableFile[roi])
  
  data_brain = data_brain[data_brain$time == "secondHalf",]
  
  data_quest<-subset(data_quest,!is.na(subj))
  
  data=cbind(data_quest[,1:8],data_brain[,6:7])
  
  df<-subset(data,!is.na(value))
  df<-subset(df,!is.na(q2))
  
  df$subj = as.factor(df$subj)
  df$illusion = as.factor(df$illusion)
  
  #library(lme4)
  #library(lmerTest) # for the p-values
  
  m01 <-lmer(value ~ q2*illusion + (1|subj), data=df)
  res01 = anova(m01)
  print(res01)
  
  df2<-subset(df,df$illusion=="0")
  m02 <-lmer(value ~ q2 + (1|subj), data=df2)
  summary(m02)
  
  df3<-subset(df,df$illusion=="1")
  m03 <-lmer(value ~ q2 + (1|subj), data=df3)
  summary(m03)
  
  m04 <-lmer(value ~ q2 + (1|subj), data=df)
  summary(m04)
  
  # Effect size model
  print("EFFECT SIZE")
  print(r.squaredGLMM(m01))
  
  pvals_test = res01$`Pr(>F)`
  
  pvals_total = cbind(pvals_total,pvals_test)
# }

pvals_adjusted = c()

# for (row in 1:3) {
#   pvals_temp = p.adjust(pvals_total[row,], method = "fdr")
#   pvals_adjusted = rbind(pvals_adjusted,pvals_temp)
# }

# install.packages("effects")
# library(effects)
eff = effect("q2*illusion",m01)

illusion_names <- as_labeller(c(`q2`="No Illusion",`illusion`="Illusion"))
plot_model(m01, type = "pred", terms = c("q2", "illusion"), title = "", axis.title =c("Scores Statement 2","Contrast"))+
  facet_wrap(~fct_rev(c("q2", "illusion")),nrow = 1, labeller = illusion_names)+
  geom_line(size = .8)+
  scale_fill_manual(values=c("#C488C4","#C488C4"))+
  scale_color_manual(values=c("#C488C4","#C488C4"))+
  theme(legend.position="none")+
  theme(axis.text.y = element_text(size = 15, color="black"))+
  theme(axis.text.x = element_text(size = 15, color="black"))+
  theme(axis.title.y = element_text(size = 15, color="black"))+
  theme(axis.title.x = element_text(size = 15, color="black"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major = element_line(color = "lightgray"))+
  theme(panel.grid.minor.x = element_blank())+
  theme(panel.grid.minor.y = element_blank())

# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Associations/Figures';
ggsave(paste("association_q2_illusion_modelB_roi",as.character(roi),".jpg"), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

plot_model(m01, type = "pred", terms = c("q2", "illusion"), title = "", axis.title =c("Scores Statement 2","Contrast"))+
  facet_wrap(~fct_rev(c("q2", "illusion")),nrow = 1, labeller = illusion_names)+
  geom_line(size = .8)+
  scale_fill_manual(values=c("darkgray","darkgray"))+
  scale_color_manual(values=c("black","black"))+
  theme(legend.position="none")+
  theme(axis.text.y = element_text(size = 15, color="black"))+
  theme(axis.text.x = element_text(size = 15, color="black"))+
  theme(axis.title.y = element_text(size = 15, color="black"))+
  theme(axis.title.x = element_text(size = 15, color="black"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major = element_line(color = "lightgray"))+
  theme(panel.grid.minor.x = element_blank())+
  theme(panel.grid.minor.y = element_blank())

# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Associations/Figures';
ggsave(paste("association_q2_illusion_modelB_roi",as.character(roi),"_bw.jpg"), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)




#############
### ModelC

rm(list = ls())

pvals_total = c()

#library(readxl)
data_quest <- read_excel("D:/VHI/Analysis/SubjectiveMeasures/Results/table_subjectiveMeasures_VHI.xlsx")

for (roi in 1:21) {
  
  # roi = 1
  
  pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelC/1stLevel_modelC3/"
  tableFile = list.files(path = pathFolder)
  
  #library(readxl)
  data_brain <-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
  print(tableFile[roi])
  
  data_brain = data_brain[data_brain$time == "After",]
  
  data_quest<-subset(data_quest,!is.na(subj))
  
  data=cbind(data_quest[,1:8],data_brain[,6])
  
  df<-subset(data,!is.na(value))
  df<-subset(df,!is.na(q2))
  
  df$subj = as.factor(df$subj)
  
  #library(lme4)
  #library(lmerTest) # for the p-values
  
  m01 <-lmer(value ~ q2 + (1|subj), data=df)
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
plot(effect("q2*illusion",m01))
