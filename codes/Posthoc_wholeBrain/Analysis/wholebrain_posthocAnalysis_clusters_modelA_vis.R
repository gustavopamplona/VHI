rm(list = ls())

# library(rstatix)
# library(ggpubr)
# library(emmeans)
# library(readxl)
# library(dplyr)
# library(ggplot2)
# library(DescTools)
# library(esvis)
# library(psych)
# # remotes::install_github("coolbutuseless/ggpattern")
# # install.packages(ggpattern)
# library("ggpattern")

roi = 3

pathFolder = "D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelA/2-MainEffect_Vis/Clusters/Selected/tables/"
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
data$vis <- ordered(data$vis, levels =c("low", "mid", "high"))

fit = aov(value ~ (stim*vis) + Error(subj/(stim*vis)), data=data) 
summary(fit)

# df$vis = factor(df$vis,levels = c("low","mid","high"))

df=data %>%
  group_by(subj,vis) %>%
  summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

pwc = df %>% 
  emmeans_test(
    value ~ vis, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc = pwc[c(1,3,2),]
pwc <- pwc %>% add_xy_position('vis', step.increase = 0.2)

# c=1
# vec=pwc$y.position
# for (i in 1:3){
#   if (pwc$p.adj.signif[i]!="ns"){
#     pwc$y.position[i]=vec[c]
#     c=c+1
#   }
# }


lab.names <- as_labeller(c("low" = "Low", "med" = "Med", "high" = "High"))
ggplot(df, aes(y=value, x=vis)) +
  geom_boxplot(aes(fill = vis))+
  theme_bw()+
  scale_fill_manual(values=c("#954595", "#C488C4", "#E4C9E4"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1) +
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  labs(y= "Contrast")+
  labs(x= "Visibility")

# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelA/2-MainEffect_Vis/Clusters/Selected/figures';
ggsave(paste("MEvis_modelA_roi",as.character(roi),".jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

df$vis = factor(df$vis,levels = c("low","mid","high"))
lab.names <- as_labeller(c("low" = "Low", "med" = "Med", "high" = "High"))
ggplot(df, aes(y=value, x=vis)) +
  geom_boxplot(aes(fill = vis))+
  theme_bw()+
  scale_fill_manual(values=c("#656565", "#A0A0A0", "#D4D4D4"))+ #gray: 656565, A0A0A0, D4D4D4
  # geom_boxplot_pattern(pattern = c("none", "stripe" ),
  #                      pattern_angle = c(45, 45),
  #                      fill            = c('lightgray','lightgray'),
  #                      colour          = 'black', 
  #                      pattern_density = .1, 
  #                      pattern_fill    = 'black',
  #                      pattern_colour  = 'black')+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  labs(y= "Contrast")+
  labs(x= "Visibility")

# Save figure BW
path='D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelA/2-MainEffect_Vis/Clusters/Selected/figures';
ggsave(paste("MEvis_modelA_roi",as.character(roi),"_bw.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(summary(fit))

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$value, df$vis), digits = 4)

# Eta-square
print("EFFECT SIZE - ETA-SQUARE")
print(EtaSq(fit, type = 1))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc)

# pwc_real = data %>% 
#   emmeans_test(
#     value ~ vis, p.adjust.method = "sidak",
#     model = fit, detailed = TRUE
#   )
# pwc_real = pwc_real[c(1,3,2),]
# pwc_real <- pwc_real %>% add_xy_position('vis', step.increase = 0.2)
# print(pwc_real)

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
print(data %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(data %>% coh_d(value~ vis, ref = "mid", se = TRUE))

