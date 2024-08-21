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
# library(MuMIn)
# library("r2glmm")

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$q2),]
data$q2[data$q2<0]=0

data=data %>%
  group_by(subj,stim,vis,run) %>%
  summarise(q2 = mean(q2, na.rm=TRUE), .groups = "drop")

data$subj = as.factor(data$subj)
data$stim = as.factor(data$stim)
data$vis = as.factor(data$vis)
data$run = as.factor(data$run)
data$vis <- ordered(data$vis, levels =c("low", "mid", "high"))
data$run <- ordered(data$run, levels =c("1", "2", "3", "4"))

fit = lmer(q2 ~ vis*stim*run + (1|subj),data = data)
res = anova(fit)
res

r2beta(fit, method = "kr")

##### Main effect of vis

df=data %>%
  group_by(subj,vis) %>%
  summarise(q2 = mean(q2, na.rm=TRUE), .groups = "drop")

pwc = df %>% 
  emmeans_test(
    q2 ~ vis, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc = pwc[c(1,3,2),]
pwc <- pwc %>% add_xy_position('vis', step.increase = 0.2)

c=1
vec=pwc$y.position
for (i in 1:3){
  if (pwc$p.adj.signif[i]!="ns"){
    pwc$y.position[i]=vec[c]
    c=c+1
  }
}

lab.names <- as_labeller(c("low" = "Low", "mid" = "Mid", "high" = "High"))
ggplot(df, aes(y=q2, x=vis)) +
  geom_violin(aes(fill = vis), width=0.6, alpha=1)+
  geom_boxplot(aes(fill=vis), width=0.1, alpha=1, color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#954595", "#C488C4", "#E4C9E4"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  labs(y= "Scores Statement 2")+
  labs(x= "Visibility")

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q2scores_vis.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(df, aes(y=q2, x=vis)) +
  geom_violin(aes(fill = vis), width=0.6, alpha=1)+
  geom_boxplot(aes(fill=vis), width=0.1, alpha=1, color="black")+
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
  # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  labs(y= "Scores Statement 2")+
  labs(x= "Visibility")

# Save figure BW
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q2scores_vis_bvw.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(res)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$q2, df$vis), digits = 4)

# Eta-square
print("EFFECT SIZE")
print(r.squaredGLMM(fit))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc)

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
print(df %>% coh_d(q2~ vis, ref = "low", se = TRUE))
print(df %>% coh_d(q2~ vis, ref = "mid", se = TRUE))



##### Main effect of stim

df=data %>%
  group_by(subj,stim) %>%
  summarise(q2 = mean(q2, na.rm=TRUE), .groups = "drop")

pwc = df %>% 
  emmeans_test(
    q2 ~ stim, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
# pwc = pwc[c(1,3,2),]
pwc <- pwc %>% add_xy_position('stim', step.increase = 0.2)

# c=1
# vec=pwc$y.position
# for (i in 1:3){
#   if (pwc$p.adj.signif[i]!="ns"){
#     pwc$y.position[i]=vec[c]
#     c=c+1
#   }
# }

# lab.names <- as_labeller(c("low" = "Low", "mid" = "Mid", "high" = "High"))
ggplot(df, aes(y=q2, x=stim)) +
  geom_violin(aes(fill = stim), width=0.6, alpha=1)+
  geom_boxplot(aes(fill=stim), width=0.1, alpha=1, color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#8888ff", "#ff8888"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Scores Statement 2")+
  labs(x= "Stimulation")

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q2scores_stim.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(df, aes(y=q2, x=stim))+
  theme_bw()+
  geom_violin(fill = 'lightgray', width=0.6, alpha=1)+
  geom_boxplot_pattern(aes(fill=stim), width=0.1, alpha=1, 
                       pattern = c("none", "stripe" ),
                       pattern_angle = c(45, 45),
                       fill            = c('lightgray','lightgray'),
                       colour          = 'black',
                       pattern_density = .1,
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  scale_x_discrete(labels=c("sync" = "Sync", "async" = "Async"))+
  labs(y= "Scores Statement 2")+
  labs(x= "Stimulation")

# Save figure BW
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q2scores_stim_bw.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(res)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$q2, df$stim), digits = 4)

# Eta-square
print("EFFECT SIZE")
print(r.squaredGLMM(fit))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc[,1:10])
print(pwc[,11:14])

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
print(df %>% coh_d(q2~ stim, ref = "async", se = TRUE))





##### Main effect of run

df=data %>%
  group_by(subj,run) %>%
  summarise(q2 = mean(q2, na.rm=TRUE), .groups = "drop")

pwc = df %>% 
  emmeans_test(
    q2 ~ run, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc = pwc[c(1,4,6,2,5,3),]
pwc <- pwc %>% add_xy_position('run', step.increase = .5)

c=1
vec=pwc$y.position
for (i in 1:6){
  if (pwc$p.adj.signif[i]!="ns"){
    pwc$y.position[i]=vec[c]
    c=c+1
  }
}

ggplot(df, aes(y=q2, x=run)) +
  geom_violin(aes(fill = run), width=0.6, alpha=1)+
  geom_boxplot(aes(fill=run), width=0.1, alpha=1, color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#C488C4", "#C488C4", "#C488C4", "#C488C4"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  labs(y= "Scores Statement 2")+
  labs(x= "Run")

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q2scores_run.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(df, aes(y=q2, x=run)) +
  geom_violin(aes(fill = run), width=0.6, alpha=1)+
  geom_boxplot(aes(fill=run), width=0.1, alpha=1, color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#A0A0A0", "#A0A0A0", "#A0A0A0", "#A0A0A0"))+ #gray: 656565, A0A0A0, D4D4D4
  # geom_boxplot_pattern(pattern = c("none", "stripe" ),
  #                      pattern_angle = c(45, 45),
  #                      fill            = c('lightgray','lightgray'),
  #                      colour          = 'black', 
  #                      pattern_density = .1, 
  #                      pattern_fill    = 'black',
  #                      pattern_colour  = 'black')+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  labs(y= "Scores Statement 2")+
  labs(x= "Run")

# Save figure BW
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q2scores_run_bw.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(res)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$q2, df$run), digits = 4)

# Eta-square
print("EFFECT SIZE")
print(r.squaredGLMM(fit))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc)

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
print(df %>% coh_d(q2~ run, ref = "1", se = TRUE))
print(df %>% coh_d(q2~ run, ref = "2", se = TRUE))
print(df %>% coh_d(q2~ run, ref = "3", se = TRUE))






# Interaction stim x vis
df=data %>%
  group_by(subj,stim,vis) %>%
  summarise(q2 = mean(q2, na.rm=TRUE), .groups = "drop")

pwc = df %>% group_by(stim) %>%
  emmeans_test(
    q2 ~ vis, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc <- pwc %>% add_xy_position('stim', step.increase = 0.2)
pwc_ytemp = pwc$y.position
pwc = pwc[c(1,3,2,4,6,5),]
pwc$y.position = pwc_ytemp

c=1
vec=pwc$y.position
for (i in 1:3){
  if (pwc$p.adj.signif[i]!="ns"){
    pwc$y.position[i]=vec[c]
    c=c+1
  }
}
c=4
vec=pwc$y.position
for (i in 4:6){
  if (pwc$p.adj.signif[i]!="ns"){
    pwc$y.position[i]=vec[c]
    c=c+1
  }
}

pwc2 = df %>% group_by(vis) %>%
  emmeans_test(
    q2 ~ stim, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc2 <- pwc2 %>% add_xy_position('vis', step.increase = 0.2)
pwc2 = pwc2[c(2,3,1),]
pwc2$xmin = c(0.73333,1,1.26667)
pwc2$xmax = pwc2$xmin+1
pwc2$y.position[1] = max(pwc$y.position)+.3
c=1
for (i in 2:3){
  if (pwc2$p.adj.signif[i]!="ns"){
    pwc2$y.position[i]=pwc2$y.position[c]+7
    c=c+1
  }
}

pwc$y.position[3]=110
pwc$y.position[6]=111

lab.names <- as_labeller(c("low" = "Low", "med" = "Med", "high" = "High"))
dodge <- position_dodge(width = 0.7)
ggplot(df, aes(y=q2, x=stim)) +
  geom_violin(aes(fill = interaction(stim,vis)), position = dodge, width=0.8, alpha=1)+
  geom_boxplot(aes(fill = interaction(stim,vis)), position = dodge, width=0.1, alpha=1, color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#4545e5", "#e54545", "#8888ff", "#ff8888", "#c9c9ff", "#ffc9c9"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0) +
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1) +
  # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Scores Statement 2")+
  labs(x= "Stimulation")

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q2scores_interaction.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

lab.names <- as_labeller(c("low" = "Low", "med" = "Med", "high" = "High"))
dodge <- position_dodge(width = 0.7)
ggplot(df, aes(y=q2, x=stim))+
  geom_violin(aes(fill=interaction(stim,vis)), position = dodge, width=0.8, alpha=1)+
  geom_boxplot_pattern(aes(fill=interaction(stim,vis)), position = dodge, width=0.1, alpha=1,
                       pattern = c("none","stripe","none", "stripe", "none", "stripe" ),
                       pattern_angle = c(45, 45,45, 45,45, 45),
                       colour          = 'black',
                       pattern_density = .1,
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  theme_bw()+
  scale_fill_manual(values=c("#656565", "#656565", "#A0A0A0","#A0A0A0", "#D4D4D4", "#D4D4D4"))+ #gray: 656565, A0A0A0, D4D4D4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0) +
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1) +
  # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Scores Statement 2")+
  labs(x= "Stimulation")

# Save figure BW
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q2scores_interaction_bw.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(res)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$q2, df$vis:df$stim), digits = 4)

# Eta-square
print("EFFECT SIZE")
print(r.squaredGLMM(fit))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc[,1:13])
print(pwc[,14:15])
print(pwc2[,1:13])
print(pwc2[,14:15])

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")

df2<-subset(df,df$stim=="sync")
print("sync")
print(df2 %>% coh_d(q2~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(q2~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$stim=="async")
print("async")
print(df2 %>% coh_d(q2~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(q2~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$vis=="low")
print("low")
print(df2 %>% coh_d(q2~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$vis=="mid")
print("mid")
print(df2 %>% coh_d(q2~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$vis=="high")
print("high")
print(df2 %>% coh_d(q2~ stim, ref = "async", se = TRUE))
