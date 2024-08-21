#############
# ROI analysis VHI study - syringe block - contrast varying with factors stimulation, visibility, and illusion

rm(list = ls())

# library(tidyverse)
# library(ggpubr)
# library(rstatix)
# library(readxl)
# library(lme4)
# library(lmerTest) # for the p-values
# library(ggplot2)
# library("ggpattern")
# library(psych)
# library(MuMIn)

# for (roi in 1:21) {
roi = 3

pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelA/1stLevel_movCor2_5s_2_6mm_run_illusion/"
tableFile = list.files(path = pathFolder)

data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
tableFile[roi]

df<-subset(data,!is.na(value))

df$subj = as.factor(df$subj)
df$run = as.factor(df$run)
df$stim = as.factor(df$stim)
df$vis = as.factor(df$vis)
df$vis <- ordered(df$vis, levels =c("low", "mid", "high"))
df$illusion = as.factor(df$illusion)
# levels(df$illusion) <- c("no Illusion", "Illusion")
# df$illusion <- ordered(df$illusion, levels =c("no Illusion", "Illusion"))

# m01 <-lmer(value ~ stim*vis*illusion + (1|subj) + (1|run), data=df) # crossed model, runs are the same measurement and are not nested in subj
# # https://stats.stackexchange.com/questions/412628/random-effects-in-repeated-measures-design-using-lme/412989#412989
# m02 <-lmer(value ~ stim*vis*illusion + (1|subj) + (1|stim) + (1|vis) + (1|illusion) + (1|subj:stim) + (1|subj:vis) + (1|subj:illusion), data=df)
# m03 <-lmer(value ~ stim*vis*illusion + (1|subj) + (1|vis) + (1|subj:vis), data=df)
m04 <-lmer(value ~ stim*vis*illusion + (1|subj), data=df)
#m04 <-lmer(gsr_beta ~ stim*vis*illusion + (1|subj), data=df)
res = anova(m04)
res

pwc = df %>% group_by(stim,illusion) %>%
  emmeans_test(
    value ~ vis, p.adjust.method = "sidak",
    model = m04, detailed = TRUE
  )
pwc <- pwc %>% add_xy_position('stim', step.increase = 0.2)
if (roi == 3){
  pwc$y.position = pwc$y.position
}
pwc_ytemp = pwc$y.position
pwc = pwc[c(1,3,2,4,6,5,7,9,8,10,12,11),]
pwc$y.position = pwc_ytemp

# c=1
vec=pwc$y.position
# pwc$y.position[1] = min(pwc$y.position)
offset = min(pwc$y.position)
for (i in 1:12){
  if (pwc$p.adj.signif[i]!="ns"){
    # pwc$y.position[i]=vec[c]
    pwc$y.position[i]=offset
    offset=offset+.35
  }
}

pwc2 = df %>% group_by(vis,illusion) %>%
  emmeans_test(
    value ~ stim, p.adjust.method = "sidak",
    model = m04, detailed = TRUE
  )
pwc2 <- pwc2 %>% add_xy_position('vis', step.increase = 0.2)
pwc2 = pwc2[c(4,6,2,3,5,1),]
pwc2$xmin = c(0.73333,1,1.26667,0.73333,1,1.26667)
pwc2$xmax = pwc2$xmin+1
temppwc<-subset(pwc,pwc$p.adj.signif!="ns")
pwc2$y.position[1] = max(temppwc$y.position)+.3
# c=1
offset = max(temppwc$y.position)+1
for (i in 1:6){
  if (pwc2$p.adj.signif[i]!="ns"){
    # pwc2$y.position[i]=pwc2$y.position[c]+.35
    # c=c+1
    pwc2$y.position[i]=offset
    offset=offset+.35
  }
}

pwc3 = df %>% group_by(stim,vis) %>%
  emmeans_test(
    value ~ illusion, p.adjust.method = "sidak",
    model = m04, detailed = TRUE
  )

df$vis = factor(df$vis,levels = c("low","mid","high"))
# df$illusion = factor(df$illusion,levels = c("no Illusion","Illusion"))
ill_names <- as_labeller(c(`0` = "No Illusion", `1` = "Illusion"))
ggplot(df, aes(x=stim, y=value)) +
  geom_boxplot(aes(fill = interaction(stim,vis,illusion)))+
  theme_bw()+
  facet_wrap(~illusion, ncol = 2, labeller = ill_names)+
  scale_fill_manual(values=c("#4545e5", "#e54545", "#8888ff", "#ff8888", "#c9c9ff", "#ffc9c9", "#4545e5", "#e54545", "#8888ff", "#ff8888", "#c9c9ff", "#ffc9c9"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0)+
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1) +
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Contrast")+
  labs(x= "Stimulation")
  
# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Figures/ModelA';
# ggsave(paste("3way_Interaction_modelA_roi",as.character(roi),".jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)
  
ggplot(df, aes(y=value, x=stim)) +
  # geom_boxplot(aes(fill = interaction(stim,vis)))+
  theme_bw()+
  facet_wrap(~illusion, ncol = 2, labeller = ill_names)+
  scale_fill_manual(values=c("#656565", "#656565", "#A0A0A0","#A0A0A0", "#D4D4D4", "#D4D4D4","#656565", "#656565", "#A0A0A0","#A0A0A0", "#D4D4D4", "#D4D4D4"))+ #gray: 656565, A0A0A0, D4D4D4
  geom_boxplot_pattern(aes(fill = interaction(stim,vis,illusion)),
                       pattern = c("none", "none", "none", "stripe", "stripe", "stripe","none", "none", "none", "stripe", "stripe", "stripe"),
                       pattern_angle = c(0,0,0,45,45,45,0,0,0,45,45,45),
                       colour          = 'black',
                       pattern_density = .1,
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0) +
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Contrast")+
  labs(x= "Stimulation")

# Save figure BW
path='D:/VHI/Analysis/ROI_analysis/Figures/ModelA';
# ggsave(paste("3way_Interaction_modelA_roi",as.character(roi),"_bw.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)


# Results ANOVA
print("ANOVA")
print(res)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$value, df$vis:df$stim:df$illusion), digits = 4)

# Eta-square
print("EFFECT SIZE - ")
print(r.squaredGLMM(m04))

# Post-hoc analysis
print("POST-HOC ANALYSIS")
print(pwc[,1:13])
print(pwc[,14:15])
print(pwc2[,1:13])
print(pwc2[,14:15])
print(pwc3[,1:13])
print(pwc3[,14:15])

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D")
df2<-subset(df,df$illusion==0)
df2<-subset(df2,df2$stim=="sync")
print("sync and NO illusion")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))
df2<-subset(df,df$illusion==0)
df2<-subset(df2,df2$stim=="async")
print("async and NO illusion")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))
df2<-subset(df,df$illusion==1)
df2<-subset(df2,df2$stim=="sync")
print("sync and illusion")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))
df2<-subset(df,df$illusion==1)
df2<-subset(df2,df2$stim=="async")
print("async and illusion")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$illusion==1)
df2<-subset(df2,df2$vis=="mid")
print("illusion and mid")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$illusion==1)
df2<-subset(df2,df2$vis=="low")
print("illusion and low")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$stim=="async")
df2<-subset(df2,df2$vis=="low")
print("async and low")
print(df2 %>% coh_d(value~ illusion, ref = "0", se = TRUE))

df2<-subset(df,df$stim=="sync")
df2<-subset(df2,df2$vis=="mid")
print("sync and mid")
print(df2 %>% coh_d(value~ illusion, ref = "0", se = TRUE))

df2<-subset(df,df$stim=="sync")
df2<-subset(df2,df2$vis=="low")
print("sync and low")
print(df2 %>% coh_d(value~ illusion, ref = "0", se = TRUE))

############# 2-way interaction

rm(list = ls())

roi = 3

pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelA/1stLevel_movCor2_5s_2_6mm_run_illusion/"
tableFile = list.files(path = pathFolder)

data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
tableFile[roi]

data<-subset(data,!is.na(value))

data$subj = as.factor(data$subj)
data$run = as.factor(data$run)
data$stim = as.factor(data$stim)
data$vis = as.factor(data$vis)
data$vis <- ordered(data$vis, levels =c("low", "mid", "high"))
data$illusion = as.factor(data$illusion)

m04 <-lmer(value ~ stim*vis*illusion + (1|subj), data=data)
res = anova(m04)
res

df=data %>%
  group_by(subj,stim,vis) %>%
  summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

# data<-subset(data,!is.na(value))
# 
# data$subj = as.factor(data$subj)
# data$stim = as.factor(data$stim)
# data$vis = as.factor(data$vis)
# data$vis <- ordered(data$vis, levels =c("low", "mid", "high"))
# 
# df = data
# 
# m04 <-lmer(value ~ stim*vis + (1|subj), data=df)
# res = anova(m04)
# res

pwc = df %>% group_by(stim) %>%
  emmeans_test(
    value ~ vis, p.adjust.method = "sidak",
    model = m04, detailed = TRUE
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
    value ~ stim, p.adjust.method = "sidak",
    model = m04, detailed = TRUE
  )
pwc2 <- pwc2 %>% add_xy_position('vis', step.increase = 0.2)
pwc2 = pwc2[c(2,3,1),]
pwc2$xmin = c(0.73333,1,1.26667)
pwc2$xmax = pwc2$xmin+1
pwc2$y.position[1] = max(pwc$y.position)+.3
c=1
for (i in 2:3){
  if (pwc2$p.adj.signif[i]!="ns"){
    pwc2$y.position[i]=pwc2$y.position[c]+.35
    c=c+1
  }
}

lab.names <- as_labeller(c("low" = "Low", "med" = "Med", "high" = "High"))
ggplot(df, aes(y=value, x=stim)) +
  geom_boxplot(aes(fill = interaction(stim,vis)))+
  theme_bw()+
  scale_fill_manual(values=c("#4545e5", "#e54545", "#8888ff", "#ff8888", "#c9c9ff", "#ffc9c9"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0) +
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1) +
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Contrast")+
  labs(x= "Stimulation")

# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Figures/ModelA';
# ggsave(paste("2way_Interaction_modelA_roi",as.character(roi),".jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

df$vis = factor(df$vis,levels = c("low","mid","high"))
lab.names <- as_labeller(c("low" = "Low", "med" = "Med", "high" = "High"))
ggplot(df, aes(y=value, x=stim)) +
  # geom_boxplot(aes(fill = interaction(stim,vis)))+
  theme_bw()+
  scale_fill_manual(values=c("#656565", "#656565", "#A0A0A0","#A0A0A0", "#D4D4D4", "#D4D4D4"))+ #gray: 656565, A0A0A0, D4D4D4
  geom_boxplot_pattern(aes(fill = interaction(stim,vis)),
                       pattern = c("none", "none", "none", "stripe", "stripe", "stripe" ),
                       pattern_angle = c(0,0,0,45,45,45),
                       colour          = 'black',
                       pattern_density = .1,
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0) +
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  scale_x_discrete(labels=c("async" = "Async", "sync" = "Sync"))+
  labs(y= "Contrast")+
  labs(x= "Stimulation")

# Save figure BW
path='D:/VHI/Analysis/ROI_analysis/Figures/ModelA';
# ggsave(paste("2way_Interaction_modelA_roi",as.character(roi),"_bw.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$value, df$vis:df$stim), digits = 4)

# Effect size model
print("EFFECT SIZE")
print(r2beta(m04), method = "kr")

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
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))

df2<-subset(data,data$stim=="sync")
print("sync")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$stim=="async")
print("async")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$vis=="high")
print("high")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$vis=="mid")
print("mid")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$vis=="low")
print("low")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))


