#############
# ROI analysis VHI study - syringe block - contrast varying with factors stimulation, visibility, and illusion

#library(readxl)
#library(lme4)
#library(lmerTest) # for the p-values
#library(ggplot2)
#library(rstatix)
#library("ggpattern")

rm(list = ls())

data<-read_excel("D:/VHI/Analysis/GSR/Tables/table_GSRbetas_VHI_filter_5e-3_5e0.xlsx")

df<-subset(data,!is.na(gsr_beta))
df<-subset(df,!is.na(illusion))

df$subj = as.factor(df$subj)
df$run = as.factor(df$run)
df$stim = as.factor(df$stim)
df$vis = as.factor(df$vis)
df$illusion = as.factor(df$illusion)

m04 <-lmer(gsr_beta ~ stim*vis*illusion + (1|subj), data=df)
anova(m04)

r2beta(m04, method = "kr")

pwc = df %>% group_by(illusion) %>%
  emmeans_test(
    value ~ stim, p.adjust.method = "sidak",
    model = m04, detailed = TRUE
  )
pwc2 = df %>% group_by(stim) %>%
  emmeans_test(
    value ~ illusion, p.adjust.method = "sidak",
    model = m04, detailed = TRUE
  )

df2=df %>%
  group_by(subj,stim,vis,illusion) %>%
  summarise(gsr_beta = mean(gsr_beta, na.rm=TRUE), .groups = "drop")

ill_names <- as_labeller(c(`0` = "No Illusion", `1` = "Illusion"))
ggplot(df2, aes(x=stim, y=gsr_beta)) +
  geom_violin(aes(fill = interaction(stim,illusion)), width=1.1, alpha=1)+
  geom_boxplot(aes(fill = interaction(stim,illusion)), width=0.2, alpha=1, color="black")+
  theme_bw()+
  facet_wrap(~illusion, ncol = 2, labeller = ill_names)+
  scale_fill_manual(values=c("#8888ff", "#ff8888","#8888ff", "#ff8888"))+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 15, color="black"))+
  theme(axis.text.x = element_text(size = 15, color="black"))+
  theme(axis.title.y = element_text(size = 15, color="black"))+
  theme(axis.title.x = element_text(size = 15, color="black"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Beta")+
  labs(x= "Stimulation")

path='D:/VHI/Analysis/GSR/Figures';
ggsave(paste("GSR_stim_illusion.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

dodge <- position_dodge(width = 0.7)
ggplot(df2, aes(y=gsr_beta, x=stim))+
  theme_bw()+
  geom_violin(aes(fill=interaction(illusion,stim)), width=1.1, alpha=1)+
  geom_boxplot_pattern(aes(fill=interaction(illusion,stim)), width=0.2, alpha=1,
                       pattern = c("none","stripe","none", "stripe"),
                       pattern_angle = c(45, 45,45, 45),
                       colour          = 'black',
                       pattern_density = .1,
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  facet_wrap(~illusion, ncol = 2, labeller = ill_names)+
  scale_fill_manual(values=c("lightgray", "lightgray","lightgray", "lightgray"))+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 15, color="black"))+
  theme(axis.text.x = element_text(size = 15, color="black"))+
  theme(axis.title.y = element_text(size = 15, color="black"))+
  theme(axis.title.x = element_text(size = 15, color="black"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Beta")+
  labs(x= "Stimulation")

path='D:/VHI/Analysis/GSR/Figures';
ggsave(paste("GSR_stim_illusion_bw.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(anova(m04))

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$gsr_beta, df$illusion:df$stim), digits = 4)

# Eta-square
print("EFFECT SIZE")
print(r.squaredGLMM(m04))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc[,1:13])
print(pwc[,14:15])
print(pwc2[,1:13])
print(pwc2[,14:15])

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
df3<-subset(df,df$stim=="sync")
print("sync")
print(df3 %>% coh_d(gsr_beta~ illusion, ref = "0", se = TRUE))
df3<-subset(df,df$stim=="async")
print("async")
print(df3 %>% coh_d(gsr_beta~ illusion, ref = "0", se = TRUE))
df3<-subset(df,df$illusion=="1")
print("illusion")
print(df3 %>% coh_d(gsr_beta~ stim, ref = "async", se = TRUE))
df3<-subset(df,df$illusion=="0")
print("no illusion")
print(df3 %>% coh_d(gsr_beta~ stim, ref = "async", se = TRUE))
