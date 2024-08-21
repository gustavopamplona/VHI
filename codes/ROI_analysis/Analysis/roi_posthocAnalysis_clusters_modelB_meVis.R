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
# library(r2glmm)
# # remotes::install_github("coolbutuseless/ggpattern")
# # install.packages(ggpattern)
# library("ggpattern")

roi = 19

pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelB/1stLevel_modelB3/"
tableFile = list.files(path = pathFolder)

data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
print(roi)

data<-subset(data,!is.na(value))

data$subj = as.factor(data$subj)
data$stim = as.factor(data$stim)
data$vis = as.factor(data$vis)
data$vis <- ordered(data$vis, levels =c("low", "mid", "high"))
data$time = as.factor(data$time)
data$illusion = as.factor(data$illusion)

fit <-lmer(value ~ stim*vis*illusion*time + (1|subj), data=data)
res01 = anova(fit)
print(res01)

df=data %>%
  group_by(subj,vis) %>%
  summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

pwc = df %>% 
  emmeans_test(
    value ~ vis, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc <- pwc %>% add_xy_position('vis', step.increase = 0.3)
pwc_ytemp = pwc$y.position
pwc = pwc[c(1,3,2),]
pwc$y.position = pwc_ytemp

# stim_names <- as_labeller(c(`sync` = "Sync", `async` = "Async"))
ggplot(df, aes(y=value, x=vis)) +
  geom_boxplot(aes(fill = vis))+
  theme_bw()+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High")) +
  scale_fill_manual(values=c("#954595", "#C488C4", "#E4C9E4"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  labs(y= "Contrast")+
  labs(x= "Visibility")

# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Figures/ModelB/';
ggsave(paste("MainEffectVis_modelB_roi",as.character(roi),".jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(df, aes(y=value, x=vis)) +
  theme_bw()+
  scale_fill_manual(values=c("#656565", "#A0A0A0", "#D4D4D4"))+ #gray: 656565, A0A0A0, D4D4D4
  geom_boxplot_pattern(aes(fill = vis),
                       pattern = c("none", "none", "none"),
                       pattern_angle = c(0,0,0),
                       colour          = 'black',
                       pattern_density = .1,
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High")) +
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  # theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  labs(y= "Contrast")+
  labs(x= "Visibility")

# Save figure BW
path='D:/VHI/Analysis/ROI_analysis/Figures/ModelB/';
ggsave(paste("MainEffectVis_modelB_roi",as.character(roi),"_bw.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(res01)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$value, df$vis), digits = 4)

# Eta-square
print("EFFECT SIZE")
print(r2beta(fit, method = "kr"))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc[,1:13])
print(pwc[,14:15])

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
print(df %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df %>% coh_d(value~ vis, ref = "mid", se = TRUE))
