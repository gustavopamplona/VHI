### Illusion onset time ###
rm(list = ls())

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$illusion_onset_time),]

data=data %>%
  group_by(subj,stim,vis,run) %>%
  summarise(illusion_onset_time = mean(illusion_onset_time, na.rm=TRUE), .groups = "drop")

data$stim <- factor(data$stim)
data$subj <- factor(data$subj)
data$vis = factor(data$vis,levels = c("low","mid","high"))
data$run = factor(data$run,levels = c("1","2","3","4"))

model2 = lmer(illusion_onset_time ~ vis*stim*run + (1|subj),data = data)
car::Anova(model2, type=2)

# Violin plot
lab.names <- as_labeller(c("low" = "Low", "med" = "Med", "high" = "High"))
dodge <- position_dodge(width = 0.7)
ggplot(data, aes(y=illusion_onset_time, x=stim)) +
  geom_violin(aes(fill = interaction(stim,vis)), position = dodge, width=1, alpha=1)+
  geom_boxplot(aes(fill = interaction(stim,vis)), position = dodge, width=0.15, alpha=1, color="black")+
  theme_bw()+
  scale_fill_manual(values=c("#4545e5", "#e54545", "#8888ff", "#ff8888", "#c9c9ff", "#ffc9c9"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  # stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0) +
  # stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1) +
  # # geom_hline(yintercept=0,linetype="dashed")+
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         legend.position="none",
         legend.title=element_blank(),
         strip.text = element_text(size = 12, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  # scale_y_continuous(breaks=seq(0,100,25), limits=c(0, 100))+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Illusion onset time (s)")+
  labs(x= "Stimulation")

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Illusion_onset_time.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(car::Anova(model2, type=2))

# Eta-square
print("EFFECT SIZE")
print(r.squaredGLMM(model2))

# # Descriptive statistics
# print("DESCRIPTIVE STATISTICS")
# print(describeBy(data$illusion_onset_time, data$stim), digits = 4)
# 
# # Post-hoc analysis
# print("POST-HOC ANALYSIS")
# print(pwc[,1:10])
# print(pwc[,11:14])
# 
# # Effect sizes
# print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
# print(df %>% coh_d(q3~ stim, ref = "async", se = TRUE))
