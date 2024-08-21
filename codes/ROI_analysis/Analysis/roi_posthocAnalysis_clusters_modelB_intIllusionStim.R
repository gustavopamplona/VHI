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

roi = 9

pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelB/1stLevel_modelB3/"
tableFile = list.files(path = pathFolder)

data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
print(roi)

# data=data %>%
#   group_by(subj,stim,vis,time,illusion) %>%
#   summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

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
  group_by(subj,stim,illusion) %>%
  summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

pwc = df %>% group_by(illusion) %>%
  emmeans_test(
    value ~ stim, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc <- pwc %>% add_xy_position('illusion', step.increase = 0.2)

pwc2 = df %>% group_by(stim) %>%
  emmeans_test(
    value ~ illusion, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc2 <- pwc2 %>% add_xy_position('stim', step.increase = 0.2)
pwc2$xmin = c(0.8,1.2)
pwc2$xmax = pwc2$xmin+1
pwc2$y.position[2] = pwc2$y.position[2]+.6

ggplot(df, aes(y=value, x=illusion)) +
  geom_boxplot(aes(fill = interaction(stim,illusion)))+
  theme_bw()+
  # facet_wrap(~illusion, ncol = 2, labeller = ill_names)+
  scale_x_discrete(labels=c('0' = "No Illusion", "1" = "Illusion"))+
  scale_fill_manual(values=c("#8888ff", "#ff8888","#8888ff", "#ff8888"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0)+
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  labs(y= "Contrast")+
  labs(x= "Illusion")

# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Figures/ModelB/';
ggsave(paste("InteractionStimIll_modelB_roi",as.character(roi),".jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(df, aes(y=value, x=illusion)) +
  theme_bw()+
  scale_fill_manual(values=c("#A0A0A0", "#A0A0A0", "#A0A0A0", "#A0A0A0"))+ #gray: 656565, A0A0A0, D4D4D4
  geom_boxplot_pattern(aes(fill = interaction(stim,illusion)),
                       pattern = c("none", "stripe", "none","stripe"),
                       pattern_angle = c(0,45,0,45),
                       colour          = 'black',
                       pattern_density = .1,
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  scale_x_discrete(labels=c('0' = "No Illusion", "1" = "Illusion"))+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0)+
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  # theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  labs(y= "Contrast")+
  labs(x= "Illusion")

# Save figure BW
path='D:/VHI/Analysis/ROI_analysis/Figures/ModelB/';
ggsave(paste("InteractionStimIll_modelB_roi",as.character(roi),"_bw.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(res01)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$value, df$illusion:df$stim), digits = 4)

# Eta-square
print("EFFECT SIZE")
print(r2beta(fit, method = "kr"))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc[,1:13])
print(pwc[,14:15])
print(pwc2[,1:13])
print(pwc2[,14:15])

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")

df2<-subset(df,df$illusion=="0")
print("No illusion")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$illusion=="1")
print("Illusion")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$stim=="async")
print("Async")
print(df2 %>% coh_d(value~ illusion, ref = "0", se = TRUE))

df2<-subset(df,df$stim=="sync")
print("Sync")
print(df2 %>% coh_d(value~ illusion, ref = "0", se = TRUE))
