rm(list = ls())

roi = 21

# pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelA/"
pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelA/1stLevel_movCor2_5s_2_6mm/"
tableFile = list.files(path = pathFolder)

#library(readxl)
data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))

# data = data[data$hrf == "canonical",]

data <- na.omit(data)

# #library(dplyr)
# data_temp = data
# data=data_temp %>%
#   group_by(subj,stim,vis) %>%
#   summarise(value=mean(value))

# library("ggpubr")
# ggboxplot(data, x = "angle", y = "beta", color = "mode")+
#   geom_hline(yintercept=0,linetype="dashed")
#library(ggplot2)
data$vis = factor(data$vis,levels = c("low","mid","high"))
ggplot(data, aes(x=stim, y=value, fill=vis)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot() 

# #data <- data[data$angle == '90' | data$angle == '270',]
# #data <- data[data$angle != '270',]

# #library(car)
# d <- data[data$angle == '270',]
# d <- d[d$mode == 'vis',]
# qqPlot(d$beta, dist = "norm")
# shapiro.test(d$beta)
# data = data[data$subj != "23",]
# ggboxplot(data, x = "angle", y = "beta", color = "mode")+geom_hline(yintercept=0,linetype="dashed")

data$subj = as.factor(data$subj)
data$stim = as.factor(data$stim)
data$vis = as.factor(data$vis)

fit = aov(value ~ (stim*vis) + Error(subj/(stim*vis)), data=data)
summary(fit)

#install.packages("DescTools")
#library(DescTools)
EtaSq(fit, type = 1)


#library(emmeans)
emmeans(fit, pairwise ~ vis|stim, adjust = "sidak")
emmeans(fit, pairwise ~ stim|vis, adjust = "sidak")

emmeans(fit, pairwise ~ vis, adjust = "sidak")

emmeans(fit, pairwise ~ stim, adjust = "sidak")

#library(rstatix)
df <- data
pwc = df %>%
  emmeans_test(
    value ~ vis, p.adjust.method = "sidak",
    detailed = TRUE
  )
pwc
df <- data
pwc = df %>% group_by(stim) %>%
  emmeans_test(
    value ~ vis, p.adjust.method = "sidak",
    detailed = TRUE
  )
pwc

pwc2 = df %>% group_by(angle) %>%
  emmeans_test(
    beta ~ mode, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc2
# pwc3 = df %>%
#   emmeans_test(
#     beta ~ angle, p.adjust.method = "sidak",
#     model = fit
#   )
# pwc3
# pwc <- pwc %>% add_xy_position('angle', group = 'mode')
# ggboxplot(data, x = "angle", y = "beta", color = "mode")+
#   geom_hline(yintercept=0,linetype="dashed")
#   stat_pvalue_manual(pwc,hide.ns = TRUE)

#library(viridis)
#library(hrbrthemes)
pwc = pwc[c(1,4,6,2,5,3,7,10,12,8,11,9),]
pwc <- pwc %>% add_xy_position('angle')
1# pwc2 <- pwc2 %>% add_xy_position('mode')
c=1
vec=pwc$y.position
for (i in 1:6){
  if (pwc$p.adj.signif[i]!="ns"){
    pwc$y.position[i]=vec[c]
    print(pwc$y.position[i])
    c=c+1
  }
}
c=7
for (i in 7:12){
  if (pwc$p.adj.signif[i]!="ns"){
    pwc$y.position[i]=vec[c]
    print(pwc$y.position[i])
    c=c+1
  }
}

#library("ggpubr")
# pwc2 <- pwc2 %>% add_xy_position('angle')
# roi01: pwc$y.position = c(1.659240,1.799928,1.940616,2.081304,2.221992,2.362680,1.236240,1.3,1.3,1.5,1.798992,1.939680)
# roi03: pwc$y.position = c(1.642760,1.811672,1.980584,1.811672,2.318408,2.487320,1.017760,1.017760,1.017760,1.186672,1.693408,1.862320)
# roi09: pwc$y.position = c(2.04580,2.22796,2.41012,2.59228,2.77444,2.95660,1.07780,1.07780,1.07780,1.25996,1.80644,1.98860)
# roi11: pwc$y.position = c(1.96140,2.15508,2.34876,2.54244,2.73612,2.92980,1.46440,1.46440,1.46440,1.65808,2.23912,2.43280)
# roi21: pwc$y.position = c(1.150360,1.150360,1.352824,1.454056,1.555288,1.251592,0.815360,0.916592,1.017824,0.815360,1.220288,1.321520)
lab.names <- as_labeller(c('tac' = "Tactile", 'vis' = "Visual"))
ggplot(data, aes(x=angle, y=beta, fill=mode)) +
  scale_fill_manual(values=c("#ff7878", "#8282ff")) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~mode, labeller = lab.names) +
  geom_jitter(colour = "black", size=1, alpha=.5) +
  theme( axis.text.x = element_text( size = 20),
         axis.text.y = element_text( size = 18),
         # labs(y = "New y label")+
         axis.title = element_text( size = 18, face = "bold"),
         axis.title.x = element_blank(),
         legend.position="none",
         strip.text = element_text(size = 20, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("0" = "0°", "90" = "Med", "180" = "180°", "270" = "Lat"))+
  stat_pvalue_manual(pwc,hide.ns = TRUE)+
  labs( y="Parameter estimates")

pwc = df %>% 
  emmeans_test(
    beta ~ angle, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc
pwc2 = df %>% 
  emmeans_test(
    beta ~ mode, p.adjust.method = "sidak",
    model = fit, detailed = TRUE
  )
pwc2

ggplot(data, aes(x=angle, y=beta, color=mode)) +
  scale_color_manual(values=c("#f94747", "#4747f9")) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot(outlier.shape = NA, size=.75) +
  facet_wrap(~mode, labeller = lab.names) +
  geom_jitter(aes(colour = mode), size=1, alpha=.5) +
  theme( axis.text.x = element_text( size = 14),
         axis.text.y = element_text( size = 14),
         axis.title = element_text( size = 14, face = "bold"),
         axis.title.x = element_blank(),
         legend.position="none",
         strip.text = element_text(size = 14, face = "bold"),
         panel.background = element_rect(fill = "white", colour = "gray", size = 0.5, linetype = "solid"),
         panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "lightgray"),
         panel.grid.major.x = element_blank())+
  scale_x_discrete(labels=c("0" = "0°", "90" = "Med", "180" = "180°", "270" = "Lat"))+
  stat_pvalue_manual(pwc,hide.ns = TRUE)

pwc3 <- pwc3 %>% add_xy_position('angle')
ggboxplot(data, x = "angle", y = "beta", color = "mode")+
  geom_hline(yintercept=0,linetype="dashed")+
  stat_pvalue_manual(pwc3,hide.ns = TRUE)


#library(lme4)
#library(lmerTest) # for the p-values
# m01 <-lmer(beta ~ mode*angle + (1|subj) + (1|subj:angle), data=data) # now nesting only angle, because it's the only observation > 2
# anova(m01)

# data.tac = data[data$mode == 'tac',]
# data.vis = data[data$mode == 'vis',]
# data.tac$subj = as.factor(data.tac$subj)
# data.tac$angle = as.factor(data.tac$angle)
# data.vis$subj = as.factor(data.vis$subj)
# data.vis$angle = as.factor(data.vis$angle)
# fit4 = kruskal.test(beta ~ angle, data=data.tac)
# fit4
# fit5 = kruskal.test(beta ~ angle, data=data.vis)
# fit5

#library(emmeans)
emmeans(fit, pairwise ~ angle|mode, adjust = "tukey")
emmeans(fit, pairwise ~ mode|angle, adjust = "tukey")

emmeans(fit, pairwise ~ mode, adjust = "tukey")
emmeans(fit, pairwise ~ angle, adjust = "tukey")

#install.packages("FSA")
#library(FSA)
dunnTest(beta ~ angle,data=data.vis,method="bh")

# data_diff<-read_excel("D:/TacMentRot/3-ROIanalysis/diff_tables/table_diff_roi01_sphere_8--18_-64_52_roi.xlsx")
# 
# ggboxplot(data_diff, x = "angle", y = "beta", color = "mode")+
# geom_hline(yintercept=0,linetype="dashed")