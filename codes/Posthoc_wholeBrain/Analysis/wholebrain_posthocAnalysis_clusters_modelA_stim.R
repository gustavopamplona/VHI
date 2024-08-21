rm(list = ls())

# library(rstatix)
# library(ggpubr)
# library(emmeans)
# library(readxl)
# library(dplyr)
# library(ggplot2)
# library(DescTools)
# library(esvis)
# library(FSA)
# remotes::install_github("coolbutuseless/ggpattern")
# install.packages(ggpattern)
# library("ggpattern")

# for (roi in 1:5) {
roi = 1

pathFolder = "D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelA/1-MainEffect_Stim/Clusters/Selected/tables/"
tableFile = list.files(path = pathFolder)

data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
print(roi)

data=data %>%
  group_by(subj,stim,vis) %>%
  summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

data<-subset(data,!is.na(value))

data$subj = as.factor(data$subj)
data$stim = as.factor(data$stim)
data$vis = as.factor(data$vis)

fit = aov(value ~ (stim*vis) + Error(subj/(stim*vis)), data=data) 
summary(fit)

df=data %>%
  group_by(subj,stim) %>%
  summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

pwc = df %>% 
  emmeans_test(
    value ~ stim, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc <- pwc %>% add_xy_position('stim')

# c=1
# vec=pwc$y.position
# for (i in 1:1){
#   if (pwc$p.adj.signif[i]!="ns"){
#     pwc$y.position[i]=vec[c]
#     print(pwc$y.position[i])
#     c=c+1
#   }
# }

lab.names <- as_labeller(c('sync' = "Sync", 'async' = "Async"))
ggplot(df, aes(y=value, x=stim)) +
  geom_boxplot(aes(fill = stim))+
  theme_bw()+
  scale_fill_manual(values=c("#8888ff", "#ff8888"))+ #c9c9ff 8888ff 4545e5 #ffc9c9 ff8888 e54545
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1) +
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("sync" = "Sync", "async" = "Async"))+
  labs(y= "Contrast")+
  labs(x= "Stimulation")

# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelA/1-MainEffect_Stim/Clusters/Selected/figures';
ggsave(paste("MEstim_modelA_roi",as.character(roi),".jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

lab.names <- as_labeller(c('sync' = "Sync", 'async' = "Async"))
ggplot(df, aes(y=value, x=stim)) +
  # geom_boxplot(aes(fill = stim))+
  theme_bw()+
  geom_boxplot_pattern(pattern = c("none", "stripe" ),
                       pattern_angle = c(45, 45),
                       fill            = c('lightgray','lightgray'),
                       colour          = 'black', 
                       pattern_density = .1, 
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("sync" = "Sync", "async" = "Async"))+
  labs(y= "Contrast")+
  labs(x= "Stimulation")

# Save figure BW
path='D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelA/1-MainEffect_Stim/Clusters/Selected/figures';
ggsave(paste("MEstim_modelA_roi",as.character(roi),"_bw.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(summary(fit))

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(Summarize(value ~ stim, data=df, digits=4))

# Eta-square
print("EFFECT SIZE - ETA-SQUARE")
print(EtaSq(fit, type = 1))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc)

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
print(df %>% coh_d(value~ stim, ref = "sync", se = TRUE))

}
