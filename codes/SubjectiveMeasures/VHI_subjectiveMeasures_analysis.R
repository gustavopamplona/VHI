# Compute statistics for subjective measures - VHI study
# Gustavo Pamplona, 11.01.2023


### Questionnaire 1 ###
rm(list = ls())

# if(!require(psych)){install.packages("psych")}
# if(!require(FSA)){install.packages("FSA")}
# if(!require(lattice)){install.packages("lattice")}
# if(!require(ggplot2)){install.packages("ggplot2")}
# if(!require(ordinal)){install.packages("ordinal")}
# if(!require(car)){install.packages("car")}
# if(!require(RVAideMemoire)){install.packages("RVAideMemoire")}
# if(!require(lsmeans)){install.packages("lsmeans")}
# if(!require(multcompView)){install.packages("multcompView")}
# if(!require(plyr)){install.packages("plyr")}
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(boot)){install.packages("boot")}
# if(!require(rcompanion)){install.packages("rcompanion")}
# if(!require(readxl)){install.packages("readxl")}
# # install.packages("mixOmics")
# # install.packages("devtools")
# # if (!requireNamespace("BiocManager", quietly = TRUE))
# # install.packages("BiocManager")
# # BiocManager::install("mixOmics")
# # install.packages("RVAideMemoire")
# # library(RVAideMemoire)
# library(lme4)
# library(lsmeans)
# library(dplyr)
# library(forcats)
# library(hrbrthemes)
# library(viridis)
# library(ggpubr)
# library(lme4)

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$q1),]
data$q1[data$q1<0]=0

data<-subset(data,!is.na(q1))

Summarize(q1 ~ vis + stim,data=data,digits=2)

# data_temp=data

# data=data_temp %>% 
#   group_by(subj,stim,vis) %>%
#   summarise(q1=mean(q1))

data$stim <- factor(data$stim)
data$subj <- factor(data$subj)
# data$vis = factor(data$vis,levels=unique(data$vis))
data$vis = factor(data$vis,levels = c("low","mid","high"))
# data$vis = factor(data$vis,ordered = TRUE)
# data$Likert.f = factor(data$q1,ordered = TRUE)
# data$run <- factor(data$run)
data$run = factor(data$run,levels = c("1","2","3","4"))
# data$run = factor(data$run,ordered = TRUE)

# model = clmm(Likert.f ~ vis*stim + (1|subj),data = data, threshold = "symmetric")
# car::Anova(model, type=2)

# model = clmm(Likert.f ~ vis*stim*run + (1|subj),data = data, threshold = "symmetric", control = clmm.control(maxIter = 200, maxLineIter = 200))
# model = clmm(Likert.f ~ vis*stim*run + (1|subj),data = data, threshold = "symmetric", gradTol = 1e-5)
# car::Anova(model, type=2)

model2 = lmer(q1 ~ vis*stim*run + (1|subj),data = data)
res = anova(model2)
res

shapiro.test(resid(model2))
plot(resid(model2))
qqnorm(residuals(model2))
qqline(residuals(model2))

leveneTest(residuals(model2) ~ vis*stim*run, data = data)
boxplot(residuals(model2) ~ vis*stim*run, data = data)
leveneTest(residuals(model2) ~ vis*stim, data = data)
boxplot(residuals(model2) ~ vis*stim, data = data)

# Violin plot
stim_names <- c('async' = "Async",'sync' = "Sync")
dodge <- position_dodge(width = .7)
data %>%
  mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  ggplot(aes(fill=stim, y=q1, x=vis))+
  geom_boxplot(aes(group=interaction(stim,vis)), position=dodge, width=0.1, alpha=0.7, color="black", outlier.colour="transparent") +
  geom_violin(aes(fill = stim), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 1")+
  xlab("Visibility")
# ylim(-3,3.6)

# vis
df = data[,1:5]
df=df %>%
  group_by(subj,vis) %>%
  summarise(q1 = mean(q1, na.rm=TRUE), .groups = "drop")

model2 = lmer(q1 ~ vis*stim*run + (1|subj),data = df)
res = anova(model2)
res

# post-hoc analyses
lsmeans(model2,pairwise ~ vis, adjust="sidak")

data %>%
  mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  ggplot(aes(fill=vis, y=q1, x=vis))+
  geom_violin(aes(fill = vis), position=dodge, width=0.6, alpha=1, outlier.colour="transparent")+
  geom_boxplot(aes(group=vis), position=dodge, width=0.1, alpha=1, color="black", outlier.colour="transparent")+
  theme_bw()+
  scale_fill_manual(values=c("#954595", "#C488C4", "#E4C9E4"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  # geom_hline(yintercept=50,linetype="dashed")+
  # facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  scale_y_continuous(breaks=seq(0,120,25), limits=c(0, 120))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Scores Statement 1")+
  xlab("Visibility")

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q1scores_vis.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(df, aes(y=q1, x=vis))+
  theme_bw()+
  scale_fill_manual(values=c("#656565", "#A0A0A0", "#D4D4D4"))+ #gray: 656565, A0A0A0, D4D4D4
  geom_violin_pattern(aes(fill = vis),
                      pattern = c("none"))
                       
data %>%
  mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  theme_bw()+
  geom_violin_pattern(aes(fill = vis))

  ggplot(aes(fill=vis, y=q1, x=vis))+
  geom_violin(aes(fill = vis), position=dodge, width=0.6, alpha=1, outlier.colour="transparent")+
  geom_boxplot(aes(group=vis), position=dodge, width=0.1, alpha=1, color="black", outlier.colour="transparent")+
  
  scale_fill_manual(values=c("#954595", "#C488C4", "#E4C9E4"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  # geom_hline(yintercept=50,linetype="dashed")+
  # facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  scale_y_continuous(breaks=seq(0,120,25), limits=c(0, 120))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Scores Statement 1")+
  xlab("Visibility")

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q1scores_vis.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)



# stim

lsmeans(model2,pairwise ~ stim, adjust="sidak")

data %>%
  ggplot(aes(fill=stim, y=q1, x=stim))+
  geom_boxplot(aes(group=stim), position=dodge, width=0.4, alpha=0.7, color="black", outlier.colour="transparent") +
  # geom_violin(aes(fill = run), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  # facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 1")+
  xlab("Stimulation")
# ylim(-3,3.6)



# run

lsmeans(model2,pairwise ~ run, adjust="sidak")

data %>%
  ggplot(aes(fill=run, y=q1, x=run))+
  geom_boxplot(aes(group=run), position=dodge, width=0.4, alpha=0.7, color="black", outlier.colour="transparent") +
  # geom_violin(aes(fill = run), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  # facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 1")+
  xlab("Run")
# ylim(-3,3.6)


###############################

### Questionnaire 2 ###
rm(list = ls())

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$q2),]
data$q2[data$q2<0]=0

Summarize(q2 ~ vis + stim,data=data,digits=2)

# data_temp=data

# data=data_temp %>% 
#   group_by(subj,stim,vis) %>%
#   summarise(q1=mean(q1))

data$stim <- factor(data$stim)
data$subj <- factor(data$subj)
# data$vis = factor(data$vis,levels=unique(data$vis))
data$vis = factor(data$vis,levels = c("low","mid","high"))
# data$vis = factor(data$vis,ordered = TRUE)
# data$Likert.f = factor(data$q2,ordered = TRUE)
# data$run <- factor(data$run)
data$run = factor(data$run,levels = c("1","2","3","4"))
# data$run = factor(data$run,ordered = TRUE)

# model = clmm(Likert.f ~ vis*stim + (1|subj),data = data, threshold = "symmetric")
# car::Anova(model, type=2)

# model = clmm(Likert.f ~ vis*stim*run + (1|subj),data = data, threshold = "symmetric")
# car::Anova

model2 = lmer(q2 ~ vis*stim*run + (1|subj),data = data)
car::Anova(model2, type=2)

shapiro.test(resid(model2))
plot(resid(model2))
qqnorm(residuals(model2))
qqline(residuals(model2))

leveneTest(residuals(model2) ~ vis*stim*run, data = data)
boxplot(residuals(model2) ~ vis*stim*run, data = data)
leveneTest(residuals(model2) ~ vis*stim, data = data)
boxplot(residuals(model2) ~ vis*stim, data = data)



# post-hoc analyses
lsmeans(model,pairwise ~ vis, adjust="sidak")
lsmeans(model,pairwise ~ stim, adjust="sidak")
lsmeans(model,pairwise ~ run, adjust="sidak")
lsmeans(model,pairwise ~ vis|stim, adjust="sidak")
lsmeans(model,pairwise ~ vis|stim, adjust="sidak")


# Violin plot
stim_names <- c('async' = "Async",'sync' = "Sync")
dodge <- position_dodge(width = .7)
data %>%
  mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  ggplot(aes(fill=stim, y=q2, x=vis))+
  geom_boxplot(aes(group=interaction(stim,vis)), position=dodge, width=0.1, alpha=0.7, color="black", outlier.colour="transparent") +
  geom_violin(aes(fill = stim), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 2")+
  xlab("Visibility")
# ylim(-3,3.6)


# vis

# post-hoc analyses
lsmeans(model2,pairwise ~ vis, adjust="sidak")

data %>%
  mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  ggplot(aes(fill=vis, y=q2, x=vis))+
  geom_boxplot(aes(group=vis), position=dodge, width=0.4, alpha=0.7, color="black", outlier.colour="transparent") +
  # geom_violin(aes(fill = run), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  # facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 2")+
  xlab("Visibility")
# ylim(-3,3.6)


# stim

lsmeans(model2,pairwise ~ stim, adjust="sidak")

data %>%
  ggplot(aes(fill=stim, y=q2, x=stim))+
  geom_boxplot(aes(group=stim), position=dodge, width=0.4, alpha=0.7, color="black", outlier.colour="transparent") +
  # geom_violin(aes(fill = run), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  # facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 2")+
  xlab("Stimulation")
# ylim(-3,3.6)



# run

lsmeans(model2,pairwise ~ run, adjust="sidak")

data %>%
  ggplot(aes(fill=run, y=q2, x=run))+
  geom_boxplot(aes(group=run), position=dodge, width=0.4, alpha=0.7, color="black", outlier.colour="transparent") +
  # geom_violin(aes(fill = run), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  # facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 2")+
  xlab("Run")
# ylim(-3,3.6)


# vis*stim

lsmeans(model2,pairwise ~ vis|stim, adjust="sidak")
lsmeans(model2,pairwise ~ stim|vis, adjust="sidak")

data %>%
  ggplot(aes(fill=stim, y=q2, x=vis))+
  geom_boxplot(aes(group=interaction(vis,stim)), position=dodge, width=0.4, alpha=0.7, color="black", outlier.colour="transparent") +
  # geom_violin(aes(fill = run), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 2")+
  xlab("Stimulation")
# ylim(-3,3.6)




################################

### Questionnaire 3 ###
rm(list = ls())

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$q3),]
data$q3[data$q3<0]=0

Summarize(q3 ~ vis + stim,data=data,digits=2)

# data_temp=data

# data=data_temp %>% 
#   group_by(subj,stim,vis) %>%
#   summarise(q1=mean(q1))

data$stim <- factor(data$stim)
data$subj <- factor(data$subj)
# data$vis = factor(data$vis,levels=unique(data$vis))
data$vis = factor(data$vis,levels = c("low","mid","high"))
# data$vis = factor(data$vis,ordered = TRUE)
# data$Likert.f = factor(data$q3,ordered = TRUE)
# data$run <- factor(data$run)
data$run = factor(data$run,levels = c("1","2","3","4"))
# data$run = factor(data$run,ordered = TRUE)

# model = clmm(Likert.f ~ vis*stim + (1|subj),data = data, threshold = "symmetric")
# car::Anova(model, type=2)

# model = clmm(Likert.f ~ vis*stim*run + (1|subj),data = data, threshold = "symmetric")
# car::Anova(model, type=2)

model2 = lmer(q3 ~ vis*stim*run + (1|subj),data = data)
car::Anova(model2, type=2)

# post-hoc analyses
lsmeans(model,pairwise ~ stim, adjust="sidak")


# Violin plot
stim_names <- c('async' = "Async",'sync' = "Sync")
dodge <- position_dodge(width = .7)
data %>%
  mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  ggplot(aes(fill=stim, y=q3, x=vis))+
  geom_boxplot(aes(group=interaction(stim,vis)), position=dodge, width=0.1, alpha=0.7, color="black", outlier.colour="transparent") +
  geom_violin(aes(fill = stim), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 3")+
  xlab("Visibility")
# ylim(-3,3.6)##################################


# stim

lsmeans(model2,pairwise ~ stim, adjust="sidak")

data %>%
  ggplot(aes(fill=stim, y=q3, x=stim))+
  geom_boxplot(aes(group=stim), position=dodge, width=0.4, alpha=0.7, color="black", outlier.colour="transparent") +
  # geom_violin(aes(fill = run), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  # facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  # scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Statement 3")+
  xlab("Stimulation")
# ylim(-3,3.6)



#############################

### Illusion indication ###
rm(list = ls())

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$illusion_perceived),]

Summarize(illusion_perceived ~ vis + stim,data=data,digits=2)

data$stim <- factor(data$stim)
data$subj <- factor(data$subj)
data$vis = factor(data$vis,levels = c("low","mid","high"))
data$vis = factor(data$vis,ordered = TRUE)
data$run = factor(data$run,levels = c("1","2","3","4"))

model2 = glmer(illusion_perceived ~ vis*stim*run + (1|subj),data = data, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
car::Anova(model2, type=2)

# Mosaic plot
# library(ggmosaic)
stim_names <- c('async' = "Async",'sync' = "Sync")
dodge <- position_dodge(width = .7)
data$stim <- factor(data$stim, levels = c("async", "sync"))
data$vis <- factor(data$vis, levels = c("low", "mid", "high"))
data$illusion_perceived <- factor(data$illusion_perceived)

ggplot(data = data)+ 
  # mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  geom_mosaic(aes(x = product(illusion_perceived,vis), fill = illusion_perceived))+
  labs(x = "", y = "")+
  facet_grid(. ~stim)+
  scale_fill_manual(labels=c("No","Yes"),values = c("#ff3333", "#009933"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=10),
        # panel.grid = element_blank(),
        # legend.title=element_blank(),legend.key=element_blank(),legend.background=element_blank(),
        legend.text = element_text(size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill = guide_legend(reverse = TRUE))

# vis
lsmeans(model2,pairwise ~ vis, adjust="sidak")

ggplot(data = data)+ 
  # mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  geom_mosaic(aes(x = product(illusion_perceived,vis), fill = illusion_perceived))+
  labs(x = "", y = "")+
  # facet_grid(. ~stim)+
  scale_fill_manual(labels=c("No","Yes"),values = c("#ff3333", "#009933"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=10),
        # panel.grid = element_blank(),
        # legend.title=element_blank(),legend.key=element_blank(),legend.background=element_blank(),
        legend.text = element_text(size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill = guide_legend(reverse = TRUE))  

# stim
lsmeans(model2,pairwise ~ stim, adjust="sidak")

ggplot(data = data)+ 
  # mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  geom_mosaic(aes(x = product(illusion_perceived,stim), fill = illusion_perceived))+
  labs(x = "", y = "")+
  # facet_grid(. ~stim)+
  scale_fill_manual(labels=c("No","Yes"),values = c("#ff3333", "#009933"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=10),
        # panel.grid = element_blank(),
        # legend.title=element_blank(),legend.key=element_blank(),legend.background=element_blank(),
        legend.text = element_text(size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill = guide_legend(reverse = TRUE))  

# stim
lsmeans(model2,pairwise ~ run, adjust="sidak")

ggplot(data = data)+ 
  # mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  geom_mosaic(aes(x = product(illusion_perceived,run), fill = illusion_perceived))+
  labs(x = "", y = "")+
  # facet_grid(. ~stim)+
  scale_fill_manual(labels=c("No","Yes"),values = c("#ff3333", "#009933"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=10),
        # panel.grid = element_blank(),
        # legend.title=element_blank(),legend.key=element_blank(),legend.background=element_blank(),
        legend.text = element_text(size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill = guide_legend(reverse = TRUE))  


##################################


### Illusion onset time ###
rm(list = ls())

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$illusion_onset_time),]

Summarize(illusion_onset_time ~ vis + stim,data=data,digits=2)

data$stim <- factor(data$stim)
data$subj <- factor(data$subj)
# data$vis = factor(data$vis,levels=unique(data$vis))
data$vis = factor(data$vis,levels = c("low","mid","high"))
# data$vis = factor(data$vis,ordered = TRUE)
# data$Likert.f = factor(data$q3,ordered = TRUE)
# data$run <- factor(data$run)
data$run = factor(data$run,levels = c("1","2","3","4"))
# data$run = factor(data$run,ordered = TRUE)

model2 = lmer(illusion_onset_time ~ vis*stim*run + (1|subj),data = data)
car::Anova(model2, type=2)

# Violin plot
stim_names <- c('async' = "Async",'sync' = "Sync")
dodge <- position_dodge(width = .7)
data %>%
  mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  ggplot(aes(fill=stim, y=illusion_onset_time, x=vis))+
  geom_boxplot(aes(group=interaction(stim,vis)), position=dodge, width=0.1, alpha=0.7, color="black", outlier.colour="transparent") +
  geom_violin(aes(fill = stim), position=dodge, width=.6, alpha=0.4, outlier.colour="transparent")+
  facet_grid(. ~ stim, labeller = as_labeller(stim_names))+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         # legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Medium", "high" = "High"))+
  # scale_y_discrete(labels=c("rhi" = "RHI", "norhi" = "noRHI"))+
  scale_fill_manual(values=c("green", "red"))+
  theme(panel.grid.major.x = element_blank())+
  ylab("Illusion onset time")+
  xlab("Visibility")
# ylim(-3,3.6)

################################