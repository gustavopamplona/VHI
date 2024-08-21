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

roi = 1

pathFolder = "D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelB/07-Interaction time x stim x vis/Clusters/Selected/tables/"
tableFile = list.files(path = pathFolder)

data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
print(roi)

data=data %>%
  group_by(subj,stim,vis,time) %>%
  summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

data<-subset(data,!is.na(value))

data$subj = as.factor(data$subj)
data$stim = as.factor(data$stim)
data$vis = as.factor(data$vis)
data$vis <- ordered(data$vis, levels =c("low", "mid", "high"))
data$time = as.factor(data$time)

fit = aov(value ~ (stim*vis*time) + Error(subj/(stim*vis*time)), data=data) 
summary(fit)

df=data %>%
  group_by(subj,stim,vis,time) %>%
  summarise(value = mean(value, na.rm=TRUE), .groups = "drop")

pwc = df %>% group_by(stim,time) %>%
  emmeans_test(
    value ~ vis, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc <- pwc %>% add_xy_position('stim', step.increase = 0.2)
pwc_ytemp = pwc$y.position
pwc = pwc[c(1,3,2,4,6,5,7,9,8,10,12,11),]
pwc$y.position = pwc_ytemp

c=1
vec=pwc$y.position
for (i in 1:6){
  if (pwc$p.adj.signif[i]!="ns"){
    pwc$y.position[i]=vec[c]
    c=c+1
  }
}
c=7
vec=pwc$y.position
for (i in 7:12){
  if (pwc$p.adj.signif[i]!="ns"){
    pwc$y.position[i]=vec[c]
    c=c+1
  }
}

pwc2 = df %>% group_by(vis, stim) %>%
  emmeans_test(
    value ~ time, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc2 = pwc2[c(3,5,1,4,6,2),]
pwc2 <- pwc2 %>% add_xy_position('vis', step.increase = 0.2)
pwc2$xmin = c(0.73333,1,1.26667,0.73333,1,1.26667)
pwc2$xmax = pwc2$xmin+1

for (i in 1:2){
  if (i == 1){
    offset = 0
  } else {
    offset = 3  
  }
  pwc_temp = pwc[1:3+offset,]
  if (any(pwc_temp$p.adj.signif!="ns")) {
    pwc_temp <- subset(pwc_temp,pwc_temp$p.adj.signif!="ns")
    pwc2$y.position[1+offset] = max(pwc_temp$y.position)+.3
    c=1+offset
    for (i in 2:3+offset) {
      if (pwc2$p.adj.signif[i]!="ns") {
        pwc2$y.position[i]=pwc2$y.position[c]+.3
        c=c+1
      }
    }
  } else {
    pwc2$y.position[1+offset] = min(pwc_temp$y.position)+.3
    c=1+offset
    for (i in 2:3+offset){
      if (pwc2$p.adj.signif[i]!="ns"){
        pwc2$y.position[i]=pwc2$y.position[c]+.3
        c=c+1
      }
    }
  }
}

pwc3 = df %>% group_by(vis, time) %>%
  emmeans_test(
    value ~ stim, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )

lab.names <- as_labeller(c("low" = "Low", "med" = "Med", "high" = "High"))
stim_names <- as_labeller(c(`sync` = "Sync", `async` = "Async"))
ggplot(df, aes(y=value, x=time), labeller = time_names) +
  geom_boxplot(aes(fill = interaction(stim,vis,time)))+
  theme_bw()+
  facet_wrap(~stim, ncol = 2, labeller = stim_names)+
  scale_x_discrete(labels=c("firstHalf" = "Early", "secondHalf" = "Late"))+
  scale_fill_manual(values=c("#4545e5", "#e54545", "#8888ff", "#ff8888", "#c9c9ff", "#ffc9c9","#4545e5", "#e54545", "#8888ff", "#ff8888", "#c9c9ff", "#ffc9c9"))+ #blue: 4545e5 8888ff c9c9ff #red: e54545 ff8888 ffc9c9 # purple: 954595 C488C4 E4C9E4
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0)+
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  labs(y= "Contrast")+
  labs(x= "Time")

# Save figure Color
path='D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelB/07-Interaction time x stim x vis/Clusters/Selected/figures';
ggsave(paste("InteractionTimeStimVis_modelB_roi",as.character(roi),".jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(df, aes(y=value, x=time)) +
  theme_bw()+
  scale_fill_manual(values=c("#656565", "#656565", "#A0A0A0","#A0A0A0", "#D4D4D4", "#D4D4D4","#656565", "#656565", "#A0A0A0","#A0A0A0", "#D4D4D4", "#D4D4D4"))+ #gray: 656565, A0A0A0, D4D4D4
  geom_boxplot_pattern(aes(fill = interaction(stim,vis,time)),
                       pattern = c("none", "none", "none", "stripe", "stripe", "stripe","none", "none", "none", "stripe", "stripe", "stripe"),
                       pattern_angle = c(0,0,0,45,45,45,0,0,0,45,45,45),
                       colour          = 'black',
                       pattern_density = .1,
                       pattern_fill    = 'black',
                       pattern_colour  = 'black')+
  facet_wrap(~stim, ncol = 2, labeller = stim_names)+
  scale_x_discrete(labels=c("firstHalf" = "Early", "secondHalf" = "Late"))+
  stat_pvalue_manual(pwc, hide.ns = TRUE, label.size = 8, bracket.size = 1, tip.length = 0)+
  stat_pvalue_manual(pwc2, hide.ns = TRUE, label.size = 8, bracket.size = 1)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.x = element_text(size = 20, color="black"))+
  theme(axis.title.y = element_text(size = 20, color="black"))+
  theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position="none")+
  labs(y= "Contrast")+
  labs(x= "Time")
# Save figure BW
path='D:/VHI/Analysis/ROI_analysis/Posthoc_wholeBrain/ModelB/07-Interaction time x stim x vis/Clusters/Selected/figures';
ggsave(paste("InteractionTimeStimVis_modelB_roi",as.character(roi),"_bw.jpg",sep=""), path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(summary(fit))

# Descriptive statistics
print("DESCRIPTIVE STATISTICS")
print(describeBy(df$value, df$vis:df$stim:df$time), digits = 4)

# Eta-square
print("EFFECT SIZE - ETA-SQUARE")
print(EtaSq(fit, type = 1))

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

df2<-subset(df,df$time=="firstHalf")
df2<-subset(df2,df2$stim=="sync")
print("Sync and Early")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$time=="firstHalf")
df2<-subset(df2,df2$stim=="async")
print("Async and Early")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$time=="secondHalf")
df2<-subset(df2,df2$stim=="sync")
print("Sync and Late")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$time=="secondHalf")
df2<-subset(df2,df2$stim=="async")
print("Async and Late")
print(df2 %>% coh_d(value~ vis, ref = "low", se = TRUE))
print(df2 %>% coh_d(value~ vis, ref = "mid", se = TRUE))

df2<-subset(df,df$time=="firstHalf")
df2<-subset(df2,df2$vis=="low")
print("Low and Early")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$time=="firstHalf")
df2<-subset(df2,df2$vis=="mid")
print("Mid and Early")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$time=="firstHalf")
df2<-subset(df2,df2$vis=="high")
print("High and Early")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$time=="secondHalf")
df2<-subset(df2,df2$vis=="low")
print("Low and Late")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$time=="secondHalf")
df2<-subset(df2,df2$vis=="mid")
print("Mid and Late")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$time=="secondHalf")
df2<-subset(df2,df2$vis=="high")
print("High and Late")
print(df2 %>% coh_d(value~ stim, ref = "async", se = TRUE))

df2<-subset(df,df$stim=="sync")
df2<-subset(df2,df2$vis=="low")
print("Sync and Low")
print(df2 %>% coh_d(value~ time, ref = "firstHalf", se = TRUE))

df2<-subset(df,df$stim=="sync")
df2<-subset(df2,df2$vis=="mid")
print("Sync and Mid")
print(df2 %>% coh_d(value~ time, ref = "firstHalf", se = TRUE))

df2<-subset(df,df$stim=="sync")
df2<-subset(df2,df2$vis=="high")
print("Sync and High")
print(df2 %>% coh_d(value~ time, ref = "firstHalf", se = TRUE))

df2<-subset(df,df$stim=="async")
df2<-subset(df2,df2$vis=="low")
print("Async and Low")
print(df2 %>% coh_d(value~ time, ref = "firstHalf", se = TRUE))

df2<-subset(df,df$stim=="async")
df2<-subset(df2,df2$vis=="mid")
print("Async and Mid")
print(df2 %>% coh_d(value~ time, ref = "firstHalf", se = TRUE))

df2<-subset(df,df$stim=="async")
df2<-subset(df2,df2$vis=="high")
print("Async and High")
print(df2 %>% coh_d(value~ time, ref = "firstHalf", se = TRUE))
