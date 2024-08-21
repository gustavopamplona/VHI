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

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$q3),]
data$q3[data$q3<0]=0

data=data %>%
  group_by(subj,stim,vis,run) %>%
  summarise(q3 = mean(q3, na.rm=TRUE), .groups = "drop")

data$subj = as.factor(data$subj)
data$stim = as.factor(data$stim)
data$vis = as.factor(data$vis)
data$run = as.factor(data$run)
data$vis <- ordered(data$vis, levels =c("low", "mid", "high"))
data$run <- ordered(data$run, levels =c("1", "2", "3", "4"))

fit = lmer(q3 ~ vis*stim*run + (1|subj),data = data)
res = anova(fit)
res

r2beta(fit, method = "kr")


##### Main effect of stim

df=data %>%
  group_by(subj,stim) %>%
  summarise(q3 = mean(q3, na.rm=TRUE), .groups = "drop")

pwc = df %>% 
  emmeans_test(
    q3 ~ stim, p.adjust.method = "sidak",
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

pwc$y.position[1] = 105

# lab.names <- as_labeller(c("low" = "Low", "mid" = "Mid", "high" = "High"))
ggplot(df, aes(y=q3, x=stim)) +
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
  scale_y_continuous(breaks=seq(25,125,25), limits=c(25, 110))+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Scores Statement 3")+
  labs(x= "Stimulation")

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q3scores_stim.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(df, aes(y=q3, x=stim))+
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
  scale_y_continuous(breaks=seq(25,125,25), limits=c(25, 110))+
  scale_x_discrete(labels=c("sync" = "Sync", "async" = "Async"))+
  labs(y= "Scores Statement 3")+
  labs(x= "Stimulation")

# Save figure BW
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Q3scores_stim_bw.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(res)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$q3, df$stim), digits = 4)

# Eta-square
print("EFFECT SIZE")
print(r.squaredGLMM(fit))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc[,1:10])
print(pwc[,11:14])

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
print(df %>% coh_d(q3~ stim, ref = "async", se = TRUE))


